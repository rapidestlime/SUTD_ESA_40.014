# At the beginning of any R session, record your AWS database password:
#source("setAWSPassword.R")

# Now, anywhere in your code where the password is needed you can get it using
#getOption("AWSPassword")
# Otherwise it is hidden. So now this code can be shared with anyone 
# without giving them access to your personal AWS database.

source("usePackages.R")
pkgnames <- c("tidyverse","shiny","DBI","jsonlite","shinydashboard","shinyWidgets","lubridate","hash")
loadPkgs(pkgnames)

source('login_registration.R')
source('question.R')















#######ui########
ui <- dashboardPage(
  dashboardHeader(title = "HAWKER-CLEANER-DASH"),
  dashboardSidebar(
    sidebarMenu(
      #https://fontawesome.com/icons?d=gallery
      menuItem("Welcome", tabName = "welcome", icon = icon("door-open")),
      menuItem("Game", tabName = "game", icon = icon("gamepad")),
      menuItem("Scores", tabName = "scores", icon = icon("bullseye"))
    )
    
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "welcome",
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
                  tags$p("You are given 60 sec to try to click as many", tags$b("dirty plates"), "as possible. ", "A special", tags$b("trivia icon"), "will also pop up randomly, and you need to answer the question to earn points.
Answering the question incorrectly will incurs a penalty of 20 points. You need to earn as many points as you can.",
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
                  tags$ul(
                    p('JARYL LIM YUHENG'),
                    p('NICHOLAS TAN YI DA')
                  ),
                  actionButton("register", "Register"),
                  actionButton("login", "Login"),
                  tags$h4("Logged in as:"),
                  htmlOutput("loggedInAs", style="font-size:30;color:#D6AC18;font-family:Times New Roman;align:center;")
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "game",
              h2("WELCOME TO THE GAME"),
              fluidRow(
                #useShinyjs(),
                box(
                  title = "Start Cleaning!!!",width=12,
                  # the trick here is to make the gameboard image 'position:absolute;z-order:0'; 
                  # Then, to superimpose images, style them to be 'position:relative;z-order:999'
                  img(src='game background image.png',style="position:absolute;z-order:0",width="315px",height="300px"),
                  imageOutput("cell11",height="100px",width="100px",click="click11",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell12",height="100px",width="100px",click="click12",inline=TRUE),  # height and width are for the containing div, not the image itself
                  imageOutput("cell13",height="100px",width="100px",click="click13",inline=TRUE), # height and width are for the containing div, not the image itself
                  tags$br(),
                  imageOutput("cell21",height="100px",width="100px",click="click21",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell22",height="100px",width="100px",click="click22",inline=TRUE),  # height and width are for the containing div, not the image itself
                  imageOutput("cell23",height="100px",width="100px",click="click23",inline=TRUE), # height and width are for the containing div, not the image itself
                  tags$br(),
                  imageOutput("cell31",height="100px",width="100px",click="click31",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell32",height="100px",width="100px",click="click32",inline=TRUE),  # height and width are for the containing div, not the image itself
                  imageOutput("cell33",height="100px",width="100px",click="click33",inline=TRUE), # height and width are for the containing div, not the image itself
                  tags$br(),
                  actionButton("start", "START"),
                  p("ESD Game Map by NTYD"),
                  textOutput('timeleft'),
                  textOutput('score')
                )
              )
      )

  )
)
)

################SERVER####################
server <- function(input, output, session) {
  #reactiveValues objects for storing items like the user password
  vals <- reactiveValues(password = NULL,playerid=NULL,playername=NULL,question_store = c(1:10),wrong_qns=c(),score=0,grid=list(0,0,0,0,0,0,0,0,0),query=NULL)
  timer <- reactiveVal(10)
  active <- reactiveVal(FALSE)
  
  
  ####################registration/login#####################
  
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
      p("Not logged in yet.",style='font-size:30;color:#D6AC18;font-family:Times New Roman;;align:center;')
    else
      p(vals$playername,style='font-size:30;color:#D6AC18;font-family:Times New Roman;;align:center;')
  })
  ####################################################
  
  
  #######image renderer#########
  renderCell <- function(imgtype){
    renderImage({
      imgsrc <- paste0('www/' , imgtype)
      # Unfortunately, we are not able to re-size the image and still have the click event work. 
      # So the image files must have exactly the size we want.
      # Also, z-order works only if 'position' is set.
      list(src=imgsrc,style="position:relative;z-order:999")},deleteFile=FALSE)
  }
  ##############################
  
  
  
  #############load the defaults##############
  output$cell11 <- renderCell('blanksmall.png')  
  output$cell12 <- renderCell('blanksmall.png') 
  output$cell13 <- renderCell('blanksmall.png')  
  output$cell14 <- renderCell('blanksmall.png')  
  output$cell21 <- renderCell('blanksmall.png') 
  output$cell22 <- renderCell('blanksmall.png') 
  output$cell23 <- renderCell('blanksmall.png')  
  output$cell24 <- renderCell('blanksmall.png')  
  output$cell31 <- renderCell('blanksmall.png') 
  output$cell32 <- renderCell('blanksmall.png') 
  output$cell33 <- renderCell('blanksmall.png')  
  output$cell34 <- renderCell('blanksmall.png')
  output$cell41 <- renderCell('blanksmall.png') 
  output$cell42 <- renderCell('blanksmall.png') 
  output$cell43 <- renderCell('blanksmall.png')  
  output$cell44 <- renderCell('blanksmall.png')
  #############################################
  
  
  ############activate game##############
  observeEvent(input$start,{active(TRUE)})
  ########################################
  
  
  ############timer function##############
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })
  
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
          temp_score <- vals$score
          vals$score <- 0
          vals$grid <- list(0,0,0,0,0,0,0,0,0)
          vals$question_store <- c(1:10)
          
          output$cell11 <- renderCell('blanksmall.png')  
          output$cell12 <- renderCell('blanksmall.png') 
          output$cell13 <- renderCell('blanksmall.png')  
          output$cell14 <- renderCell('blanksmall.png')  
          output$cell21 <- renderCell('blanksmall.png') 
          output$cell22 <- renderCell('blanksmall.png') 
          output$cell23 <- renderCell('blanksmall.png')  
          output$cell24 <- renderCell('blanksmall.png')  
          output$cell31 <- renderCell('blanksmall.png') 
          output$cell32 <- renderCell('blanksmall.png') 
          output$cell33 <- renderCell('blanksmall.png')  
          output$cell34 <- renderCell('blanksmall.png')
          output$cell41 <- renderCell('blanksmall.png') 
          output$cell42 <- renderCell('blanksmall.png') 
          output$cell43 <- renderCell('blanksmall.png')  
          output$cell44 <- renderCell('blanksmall.png')
          
          showModal(modalDialog(
            title = "GAME OVER",
            tags$h1(paste("FINAL SCORE: ", temp_score),style="text-align:center;"),
            tags$h1("Cleaning up after yourself is everybody's responsibility!",style="text-align:center;"),
            tags$h2("Think its tiring to click those dirty plates?",style="text-align:center;"),
            tags$h2("Cleaning Aunties have a tougher time than YOU!!!",style="text-align:center;color:red;"),
            HTML('<img src="https://www.memesmonkey.com/images/memesmonkey/b7/b7f445890fad3baa8cb18fe2871525ba.jpeg" style="display: block;
  margin-left: auto;
  margin-right: auto;
  width: 100%;" >'),
            br(),
            tags$h3("Here are the questions you got wrong:"),
            if (length((vals$wrong_qns))>0){
            renderDataTable({retrieve_question_and_answer(vals$wrong_qns)})}

          ))
        }
      }
    })
  })
  #########################################
  
  
  ##########show score###########
  output$score <- renderText({
    paste("Score: ", vals$score)
  })
  ###############################
  
  
  ###############function to output plates icons on unoccupied spaces#############
  observe({
    invalidateLater(700, session)
    isolate({
      if(active()){
        #add the name of file to a reactive value to access inside/outside of this observe event
        img <- "dishimage.png"
        if (length(which(vals$grid==0)) > 0){
          cell <- sample(which(vals$grid==0),1)
          print(vals$grid)
          
          if (cell == 1){
            vals$grid[cell] <- "a"
            output$cell11 <- renderCell(img)
            
          }
          if (cell == 2){
            vals$grid[cell] <- "a"
            output$cell12 <- renderCell(img)
            
          }
          if (cell == 3){
            vals$grid[cell] <- "a"
            output$cell13 <- renderCell(img)
            
          }
          if (cell == 4){
            vals$grid[cell] <- "a"
            output$cell21 <- renderCell(img)
            
          }
          if (cell == 5){
            vals$grid[cell] <- "a"
            output$cell22 <- renderCell(img)
            
          }
          if (cell == 6){
            vals$grid[cell] <- "a"
            output$cell23 <- renderCell(img)
            
          }
          if (cell == 7){
            vals$grid[cell] <- "a"
            output$cell31 <- renderCell(img)
            
          }
          if (cell == 8){
            vals$grid[cell] <- "a"
            output$cell32 <- renderCell(img)
            
          }
          if (cell == 9){
            vals$grid[cell] <- "a"
            output$cell33 <- renderCell(img)
          }
        } else{}
      }
    })
    
  })
  ###########################################
  
  
  ######function to add trivia question######
  observeEvent(timer(),{
    isolate({
    if (timer() %in% c(45,30,15)){
    cell <- sample(which(vals$grid==0),1) 
    if (cell == 1){
      vals$grid[cell] <- "b"
      output$cell11 <- renderCell('trivia.png')
      
    }
    if (cell == 2){
      vals$grid[cell] <- "b"
      output$cell12 <- renderCell('trivia.png')
      
    }
    if (cell == 3){
      vals$grid[cell] <- "b"
      output$cell13 <- renderCell('trivia.png')
      
    }
    if (cell == 4){
      vals$grid[cell] <- "b"
      output$cell21 <- renderCell('trivia.png')
      
    }
    if (cell == 5){
      vals$grid[cell] <- "b"
      output$cell22 <- renderCell('trivia.png')
      
    }
    if (cell == 6){
      vals$grid[cell] <- "b"
      output$cell23 <- renderCell('trivia.png')
      
    }
    if (cell == 7){
      vals$grid[cell] <- "b"
      output$cell31 <- renderCell('trivia.png')
      
    }
    if (cell == 8){
      vals$grid[cell] <- "b"
      output$cell32 <- renderCell('trivia.png')
      
    }
    if (cell == 9){
      vals$grid[cell] <- "b"
      output$cell33 <- renderCell('trivia.png')
    }
    } else{}
      })
      
    })
  ############################################
  
  
  ##############change question cell to state c###############
  observe({
    invalidateLater(1700)
    isolate({
      if (length(which(vals$grid=="b")) != 0){
        cell <- which(vals$grid=="b")
        if (cell == 1){
          vals$grid[cell] <- "c"
        }
        if (cell == 2){
          vals$grid[cell] <- "c"
        }
        if (cell == 3){
          vals$grid[cell] <- "c"
        }
        if (cell == 4){
          vals$grid[cell] <- "c"
        }
        if (cell == 5){
          vals$grid[cell] <- "c"
        }
        if (cell == 6){
          vals$grid[cell] <- "c"
        }
        if (cell == 7){
          vals$grid[cell] <- "c"
        }
        if (cell == 8){
          vals$grid[cell] <- "c"
        }
        if (cell == 9){
          vals$grid[cell] <- "c"
        }
       else{}
        }
        })
  })
#############################################  

  
############remove unanswered qns if any and penalty#################
  observe({
    invalidateLater(2200)
    isolate({
      if (length(which(vals$grid=="c")) != 0){
      cell <- which(vals$grid=="c")
      if (cell == 1){
        output$cell11 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0
        vals$score <- vals$score -20
      }
      if (cell == 2){
        output$cell12 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0
        vals$score <- vals$score -20
      }
      if (cell == 3){
        output$cell13 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0
        vals$score <- vals$score -20
      }
      if (cell == 4){
        output$cell21 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0
        vals$score <- vals$score -20
      }
      if (cell == 5){
        output$cell22 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0
        vals$score <- vals$score -20
      }
      if (cell == 6){
        output$cell23 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0
        vals$score <- vals$score -20
      }
      if (cell == 7){
        output$cell31 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0
        vals$score <- vals$score -20
      }
      if (cell == 8){
        output$cell32 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0
        vals$score <- vals$score -20
      }
      if (cell == 9){
        output$cell33 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0
        vals$score <- vals$score -20
      }
      else{}}
    })
    
  })
  #############################################  
  
  ##########Process grid clicks################
  processClickEvent <- function(cell){
    print(cell)
    value <- vals$grid[cell]
    
    if (value == "a"){
      if (cell == 1){
        output$cell11 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0  # return cell to empty state
        vals$score <- vals$score + 10
      }
      if (cell == 2){
        output$cell12 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0  # return cell to empty state
        vals$score <- vals$score + 10
      }
      if (cell == 3){
        output$cell13 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0  # return cell to empty state
        vals$score <- vals$score + 10
      }
      if (cell == 4){
        output$cell21 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0  # return cell to empty state
        vals$score <- vals$score + 10
      }
      if (cell == 5){
        output$cell22 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0  # return cell to empty state
        vals$score <- vals$score + 10
      }
      if (cell == 6){
        output$cell23 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0  # return cell to empty state
        vals$score <- vals$score + 10
      }
      if (cell == 7){
        output$cell31 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0  # return cell to empty state
        vals$score <- vals$score + 10
      }
      if (cell == 8){
        output$cell32 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0  # return cell to empty state
        vals$score <- vals$score + 10
      }
      if (cell == 9){
        output$cell33 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0  # return cell to empty state
        vals$score <- vals$score + 10
      }
    }
    if (value %in% c("b","c")){
      query <- get_question_and_answer(vals$question_store) # this returns a list
      vals$query <- query
      question_No <- as.integer(query[1])
      vals$question_store <- vals$question_store[-question_No]
      if (cell == 1){
        output$cell11 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0  # return cell to empty state
        showModal(popupModal(query[2]))
      }
      if (cell == 2){
        output$cell12 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0  # return cell to empty state
        showModal(popupModal(query[2]))
      }
      if (cell == 3){
        output$cell13 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0  # return cell to empty state
        showModal(popupModal(query[2]))
      }
      if (cell == 4){
        output$cell21 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0  # return cell to empty state
        showModal(popupModal(query[2]))
      }
      if (cell == 5){
        output$cell22 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0  # return cell to empty state
        showModal(popupModal(query[2]))
      }
      if (cell == 6){
        output$cell23 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0  # return cell to empty state
        showModal(popupModal(query[2]))
      }
      if (cell == 7){
        output$cell31 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0  # return cell to empty state
        showModal(popupModal(query[2]))
      }
      if (cell == 8){
        output$cell32 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0  # return cell to empty state
        showModal(popupModal(query[2]))
      }
      if (cell == 9){
        output$cell33 <- renderCell('blanksmall.png')
        vals$grid[cell] <- 0  # return cell to empty state
        showModal(popupModal(query[2]))
      }
    }
    }
  
  ##################################################
  
  
  ############handle question####################
  observeEvent(input$confirm, {
    ans = input$select_option
    if (toString(ans) == vals$query[3]) {
      print("correct")
      
      #
      vals$score <- vals$score + 10
      # link this to point system
    } else{
      print("wrong")
      vals$score <- vals$score - 20
      vals$wrong_qns <- c(vals$wrong_qns, vals$query[1])
      
    }
    vals$query <- NULL
    removeModal()
  })
  ##############################################
  
  
  ################observe clicks#######################
  observeEvent(input$click11,{processClickEvent(1)})
  observeEvent(input$click12,{processClickEvent(2)})
  observeEvent(input$click13,{processClickEvent(3)})
  observeEvent(input$click21,{processClickEvent(4)})
  observeEvent(input$click22,{processClickEvent(5)})
  observeEvent(input$click23,{processClickEvent(6)})
  observeEvent(input$click31,{processClickEvent(7)})
  observeEvent(input$click32,{processClickEvent(8)})
  observeEvent(input$click33,{processClickEvent(9)})
  ######################################################
  
  }
  




shinyApp(ui, server)





