library(shiny)
library(tidyverse)
source('connections.r')
source('wordle.r')

ui <- fluidPage(
  titlePanel("Game Results Logger"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("a_connections", "Player A Connections", ""),
      textAreaInput("b_connections", "Player B Connections", ""),
      textAreaInput("a_wordle", "Player A Wordle", ""),
      textAreaInput("b_wordle", "Player B Wordle", ""),
      actionButton("submit_connections", "Submit Connections"),
      actionButton("submit_wordle", "Submit Wordle")
    ),
    mainPanel(
      tableOutput("resultsTable")
    )
  )
)

server <- function(input, output, session) {
  results <- reactiveVal(tibble(date = as.Date(character()), gameType = character(), wonBy = character(), margin = integer()))
  
  observeEvent(input$submit_connections, {
    a_processed <- process_connections(input$a_connections, "Player A")
    b_processed <- process_connections(input$b_connections, "Player B")
    judgement <- judge_connections(a_processed, b_processed)
    
    new_entry <- tibble(date = Sys.Date(), gameType = "Connections", wonBy = judgement$wonBy, margin = judgement$margin)
    results(bind_rows(results(), new_entry))
  })
  
  observeEvent(input$submit_wordle, {
    a_processed <- process_wordle(input$a_wordle, "Player A")
    b_processed <- process_wordle(input$b_wordle, "Player B")
    judgement <- judge_wordle(a_processed, b_processed)
    
    new_entry <- tibble(date = Sys.Date(), gameType = "Wordle", wonBy = judgement$wonBy, margin = judgement$margin)
    results(bind_rows(results(), new_entry))
  })
  
  output$resultsTable <- renderTable({
    results()
  })
}

shinyApp(ui, server)