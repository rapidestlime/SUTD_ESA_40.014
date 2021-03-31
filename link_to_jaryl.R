source("usePackages.R")
pkgnames <- c("tidyverse","shiny","DBI","jsonlite","shinydashboard")
loadPkgs(pkgnames)



# get connection to aws database
getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student063",
    host = "esddbinstance-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student063",
    password = getOption("AWSPassword"))
  conn
}

# end-game, display the wrong questions
# return a 'table', input is vector of the wrong question, ie [1,4,6]
retrieve_question_and_answer <- function(j) {
  # q is a vector
  #open the connection
  conn <- getAWSConnection()
  
  df <- NULL
  
  for (i in 1:length(j)) {
    query <- paste0("SELECT * FROM QuestionPool WHERE QuestionNo = ", j[i])
    result <- dbGetQuery(conn,query)
    # result should be a dataframe with a single row and a column named 'playername'
    store <- as.data.frame(result)
    #question[1] <- result$QuestionNo[1]
    #question[2] <- result$Question[1]
    #question[3] <- result$QuestionAns[1]
    df <- rbind(df,store)
  }
  
  dbDisconnect(conn)
  df
}

#test
retrieve_question_and_answer(c(6,9,3))



# Once the game ends, update database
# reflect score on final modal

update_Score <- function(player_name, score){
  conn <- getAWSConnection()
  querytemplate <- "INSERT INTO LeaderBoardScore (playername,playerscore) VALUES (?id1,?id2)"
  query <- sqlInterpolate(conn, querytemplate,id1=player_name,id2=score)
  #print(query) #for debug
  success <- FALSE
  tryCatch(
    {  # This is not a SELECT query so we use dbExecute
      result <- dbExecute(conn,query)
      print("Score published")
      success <- TRUE
    }, error=function(cond){print("publishScore: ERROR")
      print(cond)
    }, 
    warning=function(cond){print("publishScore: WARNING")
      print(cond)},
    finally = {}
  )
  dbDisconnect(conn)
}

# update_Score("abc", 150)

# display leader board
getLeaderBoard <- function(){
  conn <- getAWSConnection()

  query <- paste0("SELECT * FROM LeaderBoardScore ORDER BY playerscore DESC")
  result <- dbGetQuery(conn,query)
  #result should return a single row
  
  dbDisconnect(conn)
  result
}


#------------------Not Done

# server function

output$moreControls <- renderUI({
  req(vals$score,vals$playerid) # if vals$score is NULL, the controls will not be visible
  tagList(
    actionButton("publishscore", "Publish Your Score"),
    tableOutput("leaderboard")
  )
})
observeEvent(input$publishscore,{
  publishScore(vals$playerid,vals$gamevariantid,vals$score)
})
output$leaderboard <- renderTable({numclicks <- input$publishscore +input$playgame #to force a refresh whenever one of these buttons is clicked
leaderboard <- getLeaderBoard(vals$gamevariantid)
leaderboard}
)