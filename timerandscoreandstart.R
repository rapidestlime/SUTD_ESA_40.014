

library(lubridate)
library(shiny)
library(shinyBS)
source("usePackages.R")
pkgnames <- c("tidyverse","shiny","DBI","jsonlite","shinydashboard")
loadPkgs(pkgnames)

#source("setAWSPassword.R")

# Now, anywhere in your code where the password is needed you can get it using
#getOption("AWSPassword")

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

ui <- fluidPage(
  actionButton("playgame", "Play the Game"),
  br(),
  br(),
  actionButton("dirtyplate", label = "DirtyPlate1"),
  br(),
  hr(),
  htmlOutput("score"),
  textOutput('timeleft')
)



server <- function(input, output, session) {
  
  vals <- reactiveValues(password = NULL,playerid=NULL,playername=NULL,score=0)
  
  timer <- reactiveVal(10)
  active <- reactiveVal(FALSE)
  
  
  #observeEvent(input$playgame, {
  #  vals$score = as.integer(runif(1,min=0,max=100))
  #  print(vals$score)
  #}) 
  #input new scoring function here
  #need to use reactiveValues
  
  observeEvent(input$dirtyplate,{
    vals$score <- vals$score+10
    print(vals$score)
  })
  

  output$score <- renderUI({
    if (vals$score == 0)
      "No score yet."
    else
      as.character(vals$score)
  })
  
  # Output the time left.
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })
  
  #when the user clicks play game the timer will start
  observeEvent(input$playgame, {active(TRUE)})
  
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active())
      {
        timer(timer()-1)
        if(timer()<1)
        {
          active(FALSE)
          
          timer(10)
          vals$score = 0
          
          showModal(modalDialog(
            title = "Important message",
            "Game is Over!",
            br(),
            tags$h3("Cleaning up after yourself is everybody's responsibility!"),
            "Here are the questions you got wrong:",
            renderDataTable({retrieve_question_and_answer(c(1,3,4))})
          ))
        }
      }
    })
  })
  
  
  
}

shinyApp(ui, server)
