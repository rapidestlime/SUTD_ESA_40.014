
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




########server########
server <- function(input, output, session) {
  #Set game parameters
  
  
  # reactiveValues objects for storing items like the user password
  vals <- reactiveValues(password = NULL,playerid=NULL,playername=NULL, 
                         question_store = c(1:10))
  GRIDSIZE <- 3
  pieces <- matrix(rep(0,GRIDSIZE*GRIDSIZE),nrow=GRIDSIZE,ncol=GRIDSIZE,byrow=TRUE)
  gamevals <- reactiveValues(turncount=0,pieces=pieces)
  
  #Fire some code if the user clicks the Register button
  observeEvent(input$register, {
    showModal(passwordModal(failed=FALSE))
  })
  # Fire some code if the user clicks the passwordok button
  observeEvent(input$passwordok, {
    # Check that password1 exists and it matches password2
    if (str_length(input$password1) >0 && (input$password1 == input$password2)) {
      #store the password and close the dialog
      vals$password <- input$password1
      print(vals$password) # for debugging
      vals$playername = registerPlayer(vals$password)
      if (!is.null(vals$playername)){
        vals$playerid <- getPlayerID(vals$playername,vals$password)
      }
      print(vals$playerid) # for debugging
      removeModal()
    } else {
      showModal(passwordModal(failed = TRUE))
    }
  })
  #Fire some code if the user clicks the Login button
  observeEvent(input$login, {
    showModal(loginModal(failed=FALSE))
  })
  # Fire some code if the user clicks the loginok button
  observeEvent(input$loginok, {
    # Get the playerID and check if it is valid
    playerid <- getPlayerID(input$playername,input$password3)
    if (playerid>0) {
      #store the playerid and playername and close the dialog
      vals$playerid <- playerid
      #print(vals$playerid) # for debugging
      vals$playername <- input$playername
      #print(vals$playername) # for debugging
      removeModal()
    } else {
      showModal(loginModal(failed = TRUE))
    }
  })
  
  # React to successful login
  output$loggedInAs <- renderUI({
    if (is.null(vals$playername))
      p("Not logged in yet.",style='color:#D6AC18;font-family:Times New Roman;;align:center;')
    else
      vals$playername
  })
  

  output$moreControls <- renderUI({
    req(vals$playerid) # if not logged in, the controls will not be visible
    tagList(
      actionButton("startgame", "Start a New Game"),
      htmlOutput("stateofturn"),
      # uiOutput("turnbutton"),
      
      
    )
  })
  
  
  # Something of a trick here: explained in the reference "change color actionButton Shiny R"
  #  output$turnbutton <- renderUI({
  #    if(!is.null(vals$turnstate) && (vals$turnstate==vals$playercolor) ){
  #        actionButton("taketurn","Take Turn")  # this is a 'do something' button
  #    } else {
  #      actionButton("notturn","Take Turn",style="font-style:italic;color:lightgrey") # this is a 'do nothing' button
  #    }
  #  })
  
  observeEvent(input$startgame,{
    gamevals$turncount <- 0
    gamevals$pieces <- matrix(rep(0,GRIDSIZE*GRIDSIZE),nrow=GRIDSIZE,ncol=GRIDSIZE,byrow=TRUE)
    initialstate <- list(turncount=gamevals$turncount,pieces=gamevals$pieces)
    vals$turnstate <- vals$playercolor  # Assume the current playercolor has the first turn
    startNewGame(vals$playerid,vals$gamevariantid,vals$turnstate,initialstate)
  })
  
  #  observeEvent(input$taketurn,{
  #    #change the state of the game
  ##    gamevals$turncount <- gamevals$turncount+1
  #    newstate <- list(turncount=gamevals$turncount)
  #    # check for end of game
  #    if (gamevals$turncount>MAXTURNS){
  #      vals$turnstate <- 0 # turnstate=0 signals end of the game
  #    } else{
  #      #switch turnstate between player colors
  #      if (vals$turnstate==1)vals$turnstate <- 2 else vals$turnstate <- 1
  #    }
  #    updateGame(vals$playerid,vals$gamevariantid,vals$turnstate,newstate)
  #  })
  
  renderCell <- function(gridrow,gridcol){
    renderImage({
      #select the icon appropriate for this cell
      imageid <- 1
      if (!is.null(gamevals$pieces)) imageid <- gamevals$pieces[gridrow,gridcol]+1
      imgsrc=switch(imageid,"www/blanksmall.png","www/RedStoneSmall.png","www/BlueStoneSmall.png")
      # Unfortunately, we are not able to re-size the image and still have the click event work. 
      # So the image files must have exactly the size we want.
      # Also, z-order works only if 'position' is set.
      list(src=imgsrc,style="position:relative;z-order:999") 
    },deleteFile=FALSE)
  }
  
  output$cell11 <- renderCell(1,1)  
  output$cell12 <- renderCell(1,2) 
  output$cell13 <- renderCell(1,3)  
  output$cell14 <- renderCell(1,4)  
  output$cell21 <- renderCell(2,1) 
  output$cell22 <- renderCell(2,2) 
  output$cell23 <- renderCell(2,3)  
  output$cell24 <- renderCell(2,4)  
  output$cell31 <- renderCell(3,1) 
  output$cell32 <- renderCell(3,2) 
  output$cell33 <- renderCell(3,3)  
  output$cell34 <- renderCell(3,4)
  output$cell41 <- renderCell(4,1) 
  output$cell42 <- renderCell(4,2) 
  output$cell43 <- renderCell(4,3)  
  output$cell44 <- renderCell(4,4)
  
  processClickEvent <- function(gridrow,gridcol){
    # If it is not this player's turn or if the cell is occupied, then ignore the click
    req(vals$playerid)
    if ((gamevals$pieces[gridrow,gridcol]==0)&&(vals$turnstate==as.integer(vals$playercolor))){
      #change the state of the game
      gamevals$pieces[gridrow,gridcol] <- as.integer(vals$playercolor) # as.integer necessary because vals$playercolor is actually a string
      gamevals$turncount <- gamevals$turncount+1
      newstate <- list(turncount=gamevals$turncount,pieces=gamevals$pieces)
      # check for end of game
      if (gamevals$turncount>MAXTURNS){
        vals$turnstate <- 0 # turnstate=0 signals end of the game
      } else{
        #switch turnstate between player colors
        if (vals$turnstate==1)vals$turnstate <- 2 else vals$turnstate <- 1
      }
      updateGame(vals$playerid,vals$gamevariantid,vals$turnstate,newstate)
    }
  }
  
  observeEvent(input$click11,{processClickEvent(1,1)})
  observeEvent(input$click12,{processClickEvent(1,2)})
  observeEvent(input$click13,{processClickEvent(1,3)})
  observeEvent(input$click14,{processClickEvent(1,4)})
  observeEvent(input$click21,{processClickEvent(2,1)})
  observeEvent(input$click22,{processClickEvent(2,2)})
  observeEvent(input$click23,{processClickEvent(2,3)})
  observeEvent(input$click24,{processClickEvent(2,4)})
  observeEvent(input$click31,{processClickEvent(3,1)})
  observeEvent(input$click32,{processClickEvent(3,2)})
  observeEvent(input$click33,{processClickEvent(3,3)})
  observeEvent(input$click34,{processClickEvent(3,4)})
  observeEvent(input$click41,{processClickEvent(4,1)})
  observeEvent(input$click42,{processClickEvent(4,2)})
  observeEvent(input$click43,{processClickEvent(4,3)})
  observeEvent(input$click44,{processClickEvent(4,4)})
  
  
  
  ###########import server functions from testshiny13###############
  
  observeEvent(input$playgame, {
    vals$score = as.integer(runif(1,min=0,max=100))
    print(vals$score)
  })
  
  # React to completion of game
  output$score <- renderUI({
    if (is.null(vals$score))
      "No score yet."
    else
      as.character(vals$score)
  })
  
  
  output$moreControls1 <- renderUI({
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
}