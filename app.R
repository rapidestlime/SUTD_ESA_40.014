# At the beginning of any R session, record your AWS database password:
#source("setAWSPassword.R")

# Now, anywhere in your code where the password is needed you can get it using
getOption("AWSPassword")
# Otherwise it is hidden. So now this code can be shared with anyone 
# without giving them access to your personal AWS database.

#source("usePackages.R")
pkgnames <- c("tidyverse","shiny","DBI","jsonlite","shinydashboard","shinyWidgets")
loadPkgs(pkgnames)

source('login_registration.R')



#thisisattest
#thisisatest2






#######ui########
ui <- dashboardPage(
  dashboardHeader(title = "HAWKER CLEANER DASH"),
  dashboardSidebar(
    sidebarMenu(
      #https://fontawesome.com/icons?d=gallery
      menuItem("Welcome", tabName = "welcome", icon = icon("door-open")),
      menuItem("Game", tabName = "game", icon = icon("gamepad")),
      menuItem("Scores", tabName = "Scores", icon = icon("bullseye"))
    )
    
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Welcome",
              tags$div(
                tags$img(
                  src='game background image.png',
                  style="position: absolute; 
                         top: -50%; 
                         left: -50%; 
                         width: 150%; 
                         height: 150%;
                         display: flex;
                         z-order: 0;"
                  ),
                tags$div(
                  style="z-order:1;position:relative;",
                  tags$img(
                    src="Platershiny game logo.png",
                    style= 
                    'margin-left: auto;
                     margin-right: auto;
                     width: 80%; 
                     height: 80%;
                     align: center;'
                    ),
                  tags$br(),
                  tags$h2(
                    'WELCOME!',
                    style='color: #FFFFFF;
                           font-family: Times New Roman;
                           align: center;'
                    ),
                  tags$br(),
                  tags$h3(
                    'About Game',
                    style='color: #FFFFFF;
                           font-family: Times New Roman;
                           align: center;'
                    ),
                  tags$p(
                    "The game is designed to raise awareness about best practices in hawker centres, and make them 
                    a better place for everyone!",
                    style='color:#FFFFFF;
                           font-family:Times New Roman;
                           align:center;'
                    ),
                  tags$h3(
                    "Instructions",
                    style='color:#FFFFFF;
                          font-family:Times New Roman;
                          align:center;'
                    ),
                  tags$p("You are given 60 sec to try to click as many", tags$b("dirty plates"), "as possible. Every dirty plate
on the grid will disappear in", tags$b("2 seconds."), ".A special", tags$b("trivia icon"), "will also pop up randomly, and you need to answer the question to earn points.
Not clicking dirty plates, or answering the question incorrectly will cause you to lose points. You need to earn as many points as you can.
                         We are simply demonstrating the elements of a two-player game.",
                         style='color:#FFFFFF;
                               font-family:Times New Roman;
                               align:center;'
                         ),
                  tags$h3(
                    "Team Members",
                    style='color:#FFFFFF;
                           font-family:Times New Roman;
                           align:center;'
                    ),
                  actionButton("register", "Register"),
                  actionButton("login", "Login"),
                  tags$h4("Logged in as:"),
                  htmlOutput("loggedInAs")
                  )
                )
              ),
      
      
      # Second tab content
      tabItem(tabName = "game",
              h2("Make Your Own Rules"),
              fluidRow(
                box(
                  title = "A Very Simple Board Game",width=12,
                  htmlOutput("playercolorchoice"),
                  uiOutput("moreControls"),
                  # the trick here is to make the gameboard image 'position:absolute;z-order:0'; 
                  # Then, to superimpose images, style them to be 'position:relative;z-order:999'
                  img(src='FantasyMap.jpg',style="position:absolute;z-order:0",width="415px",height="400px"),
                  imageOutput("cell11",height="100px",width="100px",click="click11",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell12",height="100px",width="100px",click="click12",inline=TRUE),  # height and width are for the containing div, not the image itself
                  imageOutput("cell13",height="100px",width="100px",click="click13",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell14",height="100px",width="100px",click="click14",inline=TRUE),  # height and width are for the containing div, not the image itself
                  tags$br(),
                  imageOutput("cell21",height="100px",width="100px",click="click21",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell22",height="100px",width="100px",click="click22",inline=TRUE),  # height and width are for the containing div, not the image itself
                  imageOutput("cell23",height="100px",width="100px",click="click23",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell24",height="100px",width="100px",click="click24",inline=TRUE),  # height and width are for the containing div, not the image itself
                  tags$br(),
                  imageOutput("cell31",height="100px",width="100px",click="click31",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell32",height="100px",width="100px",click="click32",inline=TRUE),  # height and width are for the containing div, not the image itself
                  imageOutput("cell33",height="100px",width="100px",click="click33",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell34",height="100px",width="100px",click="click34",inline=TRUE),  # height and width are for the containing div, not the image itself
                  tags$br(),
                  imageOutput("cell41",height="100px",width="100px",click="click41",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell42",height="100px",width="100px",click="click42",inline=TRUE),  # height and width are for the containing div, not the image itself
                  imageOutput("cell43",height="100px",width="100px",click="click43",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell44",height="100px",width="100px",click="click44",inline=TRUE),  # height and width are for the containing div, not the image itself
                  tags$br(),
                  p("ESD Fantasy Map and Game Pieces by Tan Yi Lin")
                )
              )
      ),
      
      
      #Third Tab
      tabItem(tabName= "Scores",
              h2("Publish Your Score And See Where You Stand")
              )
      )
    )
  )



########server########
server <- function(input, output, session) {
  #Set game parameters
  MAXTURNS <- 15 # Keep it short for debugging
  # reactiveValues objects for storing items like the user password
  vals <- reactiveValues(password = NULL,playerid=NULL,playername=NULL,gamevariantid=1,playercolor=1,turnstate=NULL)
  GRIDSIZE <- 4
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
      p("Not logged in yet.",style='color:#FFFFFF;font-family:Times New Roman;;align:center;')
    else
      vals$playername
  })
  
  # React to change in player color
  output$playercolorchoice <- renderUI({
    vals$playercolor <- input$playercolor
    result <- "On RED side."
    if (vals$playercolor==2) result <- "On BLUE side."
    result
  })
  
  output$moreControls <- renderUI({
    req(vals$playerid) # if not logged in, the controls will not be visible
    tagList(
      actionButton("startgame", "Start a New Game"),
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
    #initialstate <- list(turncount=gamevals$turncount,pieces=gamevals$pieces)
    #vals$turnstate <- vals$playercolor  # Assume the current playercolor has the first turn
    startNewGame(vals$playerid,vals$gamevariantid)
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

shinyApp(ui, server)





