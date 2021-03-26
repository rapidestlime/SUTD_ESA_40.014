


library(shiny)


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
            "Game is Over!"
          ))
        }
      }
    })
  })
  
  
  
}

shinyApp(ui, server)
