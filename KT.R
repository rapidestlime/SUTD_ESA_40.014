library(shiny)


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


ui <- fluidPage(
  actionButton("popupok", "Popup") #we will change to QQ_Time
)


server <- function(input, output, session){
  #Fire some code if the user clicks the Popup button
  # change to observeEvent(input$click11,{})

  query <- get_question_and_answer(vals$question_store) # this returns a list
  
  
  #Fire some code if the user clicks the Popup button
  # change to observeEvent(input$click11,{})
  
  observeEvent(input$popupok, {
    # remove the question index from vals$question_store to prevent repetition
    question_No <- as.integer(query[1])
    vals$question_store <- vals$question_store[-question_No]
    
    # Show the question in pop-up modal
    showModal(popupModal(query[2]))
  })
  
  # Check user's answer using query[3]
  observeEvent(input$confirm, {
    ans = input$select_option
    if (toString(ans) == query[3]) {
      print("add points")
      
      #
      vals$score <- vals$score + 10
      # link this to point system
    } else {
      vals$score <- vals$score - 10
    }
    removeModal()
  })
  
  
  
}

shinyApp(ui, server)