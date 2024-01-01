library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)  
source('connections.r')
source('wordle.r')

ui <- dashboardPage(
  dashboardHeader(title = "Game Results Logger"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(title = "Game Inputs", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
          fluidRow(
            column(4,
                   textAreaInput("a_connections", "A Connections", ""),
                   textAreaInput("b_connections", "R Connections", ""),
                   
            ),
            
            column(4,
                   textAreaInput("a_wordle", "A Wordle", ""),
                   textAreaInput("b_wordle", "R Wordle", "")
            )
          ),
          fluidRow(
            column(12,
                   actionButton("submit", "Submit", class = "btn-primary")
            )
          )
      )
    ),
    fluidRow(
      box(title = "Results", status = "info", solidHeader = TRUE, width = 12,
          DT::dataTableOutput("resultsTable")
      )
    )
  )
)


server <- function(input, output, session) {
  results <- reactiveVal(tibble(date = as.Date(character()), gameType = character(), wonBy = character(), margin = integer()))
  
  observeEvent(input$submit, {
    if (isTruthy(input$a_connections) && isTruthy(input$b_connections) &&
        isTruthy(input$a_wordle) && isTruthy(input$b_wordle)) {
      
      a_con_processed <- process_connections(input$a_connections, "Player A")
      b_con_processed <- process_connections(input$b_connections, "Player R")
      con_judgement <- judge_connections(a_con_processed, b_con_processed)
      
      wordle_a_processed <- process_wordle(input$a_wordle, "Player A")
      wordle_b_processed <- process_wordle(input$b_wordle, "Player R")
      wordle_judgement <- judge_wordle(wordle_a_processed, wordle_b_processed)
      
      new_entries <- tribble(
        ~date, ~gameType, ~wonBy, ~margin,
        Sys.Date(), "Connections", con_judgement$wonBy, con_judgement$margin,
        Sys.Date(), "Wordle", wordle_judgement$wonBy, wordle_judgement$margin
      )
      
      results(bind_rows(results(), new_entries))
    } else {
      shiny::showNotification("Please fill in all fields before submitting.", type = "error")
    }
  })
  
  output$resultsTable <- DT::renderDataTable({
    DT::datatable(results(), options = list(pageLength = 5, autoWidth = TRUE))
  })
}

shinyApp(ui, server)
