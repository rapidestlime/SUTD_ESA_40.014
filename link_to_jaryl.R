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

# return a 'table', input is vector of the wrong question, ie [1,4,6]
retrieve_question_and_answer <- function(j) {
  # q is a vector
  #open the connection
  conn <- getAWSConnection()
  
  frame1 <- as.data.frame(0, )
  ?as.data.frame
  
  # choose random index from vector 
  
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