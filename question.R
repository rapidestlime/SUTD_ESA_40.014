# At the beginning of any R session, record your AWS database password:
source("setAWSPassword.R")

# Now, anywhere in your code where the password is needed you can get it using
#getOption("AWSPassword")
# Otherwise it is hidden. So now this code can be shared with anyone 
# without giving them access to your personal AWS database.

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

# retrieve random question from database during the game
get_question_and_answer <- function(q) {
  # q is a vector
  #open the connection
  conn <- getAWSConnection()
  
  
  # choose random index from vector q
  a <-sample(1:length(q), 1)
  questionNo <- q[a]
  
  query <- paste0("SELECT * FROM QuestionPool WHERE QuestionNo = ", questionNo)
  result <- dbGetQuery(conn,query)
  # result should be a dataframe with a single row and a column named 'playername'
  question <- c(1:3)
  question[1] <- result$QuestionNo[1]
  question[2] <- result$Question[1]
  question[3] <- result$QuestionAns[1]
  
  # remove the element from q
  dbDisconnect(conn)
  question
  
}

# retrieve quesitons got wrong
retrieve_question_and_answer <- function(j) {
  # j is a vector
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

popupModal <- function(question) {
  modalDialog(
    title = "Popup Question",
    h2(question), #function for query, placeholder
    br(),
    br(),
    selectInput("select_option","Are you sure?", c("Yes", "No")),
    # actionButton("questionminuspoints","Yes"), #function to minus points bc Yes is wrong answer
    # actionButton("questionpluspoints","No"), #function to plus points bc No is right answer
    actionButton("confirm", "Confirm"),
    
    footer = tagList(
      #modalButton("Cancel"), #later we can remove, cos users dont use
      #actionButton("popupok", "OK")
    )
  )
}
