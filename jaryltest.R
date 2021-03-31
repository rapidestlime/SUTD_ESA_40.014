

ui <- dashboardPage(
  dashboardHeader(title = "A Two-Player Game"),
  dashboardSidebar(
    sidebarMenu(
      #https://fontawesome.com/icons?d=gallery
      menuItem("Welcome", tabName = "welcome", icon = icon("door-open")),
      menuItem("Game", tabName = "game", icon = icon("chess-board"))
    ),
    selectInput("playercolor", "Select which side to play on:",
                c("Red"=1,"Blue"=2))
    
  ),
  dashboardBody(
    tabItems(
      #insert first tab item
      #2nd tab item
      tabItem(tabName = "game",
              h2("Make Your Own Rules"),
              fluidRow(
                box(
                  title = "A Very Simple Board Game",width=12,
                  htmlOutput("playercolorchoice"),
                  uiOutput("moreControls"),
                  # the trick here is to make the gameboard image 'position:absolute;z-order:0'; 
                  # Then, to superimpose images, style them to be 'position:relative;z-order:999'
                  img(src='background.jpg',style="position:absolute;z-order:0",width="315px",height="300px"),
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
                  p("ESD Fantasy Map and Game Pieces Courtesy of Nicholas Tan Yi Da")
                )
              )
      )
    )
  
))
server <- function(input, output, session) {
  vals <- reactiveValues(password = NULL,playerid=NULL,playername=NULL,question_store = c(1:10),score = 0)
  GRIDSIZE <- 3
  pieces <- matrix(rep(0,GRIDSIZE*GRIDSIZE),nrow=GRIDSIZE,ncol=GRIDSIZE,byrow=TRUE)
  gamevals <- reactiveValues(turncount=0,pieces=pieces)
  
  renderCell <- function(gridrow,gridcol){
    renderImage({
      #select the icon appropriate for this cell
      imageid <- 1
      if (!is.null(gamevals$pieces)) imageid <- gamevals$pieces[gridrow,gridcol]+1
      imgsrc=switch(imageid,"www/dirtyplateicon.png","www/BlueStoneSmall.png")
      # Unfortunately, we are not able to re-size the image and still have the click event work. 
      # So the image files must have exactly the size we want.
      # Also, z-order works only if 'position' is set.
      list(src=imgsrc,style="position:relative;z-order:999") 
    },deleteFile=FALSE)
  }
  
  output$cell11 <- renderCell(1,1)  
  output$cell12 <- renderCell(1,2) 
  output$cell13 <- renderCell(1,3)  
  output$cell21 <- renderCell(2,1) 
  output$cell22 <- renderCell(2,2) 
  output$cell23 <- renderCell(2,3)  
  output$cell31 <- renderCell(3,1) 
  output$cell32 <- renderCell(3,2) 
  output$cell33 <- renderCell(3,3)  

}
shinyApp(ui, server)