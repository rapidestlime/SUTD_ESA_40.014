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

# retrieve question from database
get_question_and_answer <- function(store) {
  #open the connection
  conn <- getAWSConnection()
  
  # choose random index from vector q
  questionNo <- sample(store, 1)
  
  query <- paste0("SELECT * FROM QuestionPool WHERE QuestionNo = ", toString(questionNo))
  result <- dbGetQuery(conn,query)
  
  # result should be a dataframe with a single row and a column named 'playername'
  question <- c()
  question[1] <- result$QuestionNo
  question[2] <- result$Question
  question[3] <- result$QuestionAns
  
  # update question_pool in vals to remove used question
  vals$question_pool <- vals$question_pool[x != questionNo]
  
  # remove the element from q
  dbDisconnect(conn)
  question
  
}


