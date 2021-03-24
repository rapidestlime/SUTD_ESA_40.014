library(lubridate)
library(shiny)
# https://stackoverflow.com/questions/49250167/how-to-create-a-countdown-timer-in-shiny/49254510
ui <- fluidPage(
  hr(),
  actionButton('start','Start'),
  actionButton('reset','Reset'),
  textOutput('timeleft')
  
)

server <- function(input, output, session) {
  
  # Initialize the timer, 60 seconds, not active.
  timer <- reactiveVal(60)
  active <- reactiveVal(FALSE)
  
  # Output the time left.
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })
  
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
          
          showModal(modalDialog(
            title = "Important message",
            "Countdown completed!"
          ))
        }
      }
    })
  })
  
  # observers for actionbuttons
  observeEvent(input$start, {active(TRUE)})
  observeEvent(input$reset, {timer(60)})
  
}

shinyApp(ui, server)
