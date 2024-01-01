library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)
library(googledrive)
library(googlesheets4)
library(shinyjs)
source("connections.r")
source("wordle.r")
source("calendar.r")
source("playerDataCheck.r")
source('crossword.r')
source('gameHeatmaps.r')
options(gargle_oauth_email = TRUE,
        gargle_oauth_cache = "./.secrets")

sheet_id <- googledrive::drive_get("word-games")$id

ui  <- dashboardPage(
  dashboardHeader(title = "🔠🗞 NYT Word Games Logger", titleWidth = 450),
  dashboardSidebar(disable = TRUE),
  dashboardBody(useShinyjs(),
                tabsetPanel(
                  tabPanel("Player Log", fluidRow(
                    box(
                      status = "warning",
                      width = 12,
                      column(
                        4,
                        h3("💾 Log your day"),
                        hr(),
                        selectInput("player", "Choose player", choices = c("Aman", "Rhea")),
                        textAreaInput("player_wordle", "Wordle", ""),
                        textAreaInput("player_connections", "Connections", ""),
                        numericInput("player_crossword", "Crossword Time", value = NA),
                        actionButton("submit", "Submit", class = "btn-primary")
                      ),
                      column(8,
                             h3("🏆 Wins"),
                             hr(),
                             DT::dataTableOutput("mainSheetTable"))
                    ),
                    box(
                      title = "📊 Visualizations",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      column(
                        4,
                        h3("Play streak"),
                        hr(),
                        plotOutput("calendarHeatmap", width = '100%', height = '800px')
                      ),
                      column(
                        8,
                        fluidRow(
                        h3("Wordle Win Margins"),
                        hr(),
                        plotOutput("wordleWins", width = '100%', height = '300px')
                      )),
                    )
                  )),
                  tabPanel("Raw Data", fluidRow(
                    box(
                      title = "Raw Data",
                      status = "info",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      DT::dataTableOutput("resultsTable")
                    )
                  ))
                ))
)



server <- function (input, output, session) {
  # Read initial data from Google Sheets
  values <- reactive({
    read_sheet(ss = sheet_id, sheet = "gameplays")
  })
  
  main_sheet_values <- reactive({
    read_sheet(ss = sheet_id, sheet = "results")
  })
  
  # Update table on app load
  output$resultsTable <- renderDataTable({
    values()
  })
  
  output$mainSheetTable <- renderDataTable({
    main_sheet_values() %>%
      rename(
        Date = date,
        `Connections Winner` = connections_won_by,
        `Connections Win Margin` = connections_margin,
        `Wordle Winner` = wordle_won_by,
        `Wordle Win Margin` = wordle_margin,
        `Crossword Winner` = crossword_won_by,
        `Crossword Win Margin` = crossword_win_margin
      )
  })
  
  output$calendarHeatmap <- renderPlot({
    data <- values()
    generate_heatmap(data)
  })
  
  # Game heatmaps
  output$wordleWins <- renderPlot({
    data <- main_sheet_values()
   # makeGameHeatmap(data, "Aman", "wordle")
    # makeGameHeatmap(data, "Aman", "connections")
  })
  
  todays_entries <- reactive({
    data <- values()
    data %>% filter(date == as.Date(Sys.Date()), player %in% c("Aman", "Rhea"))
  })
  
  
  observe({
    todays_data <- values() %>%
      filter(date == Sys.Date()) %>%
      group_by(player) %>%
      summarise(
        wordle_filled = any(!is.na(wordle)),
        connections_filled = any(!is.na(connections)),
        crossword_filled = any(!is.na(crossword)),
        .groups = 'drop'
      )
    
    # Check if both players have all games filled
    if (nrow(todays_data) == 2 &&
        all(todays_data$wordle_filled) &&
        all(todays_data$connections_filled) &&
        all(todays_data$crossword_filled)) {
      showNotification("All three games for both players are filled", type = "message")
    }
    
    playerDataCheck(session, values, input, output)
  })
  
  
  observeEvent(input$submit, {
   
    
    
    data_to_save <-tibble(
      date = as.character(Sys.Date()),
      player = as.character(input$player),
      wordle = as.character(input$player_wordle),
      connections = as.character(input$player_connections),
      crossword = ifelse(
        is.na(input$player_crossword),
        NA_character_,
        as.character(input$player_crossword)
      )
    )
    
    # Append data to Google Sheets with explicit column names
    sheet_append(ss = sheet_id,
                 data = data_to_save,
                 sheet = "gameplays")
    
    updated_values <- read_sheet(ss = sheet_id, sheet = "gameplays")
    
    updated_values <-updated_values %>%
      mutate(date = as.Date(date))
    
    today <-as.character(Sys.Date())
    
    # Now filter using the Date type
    todays_data <-updated_values %>%
      filter(date == Sys.Date(), player %in% c("Aman", "Rhea"))
    
    if (nrow(todays_data) == 2) {
      
      connections_results < -todays_connections(todays_data)
      crossword_results <- todays_crossword(todays_data)
      wordle_results < -todays_wordle(todays_data)
      
      main_data_to_save < -tibble(
        date = today,
        connections_won_by = connections_results$wonBy,
        connections_margin = connections_results$margin,
        wordle_won_by = wordle_results$wonBy,
        wordle_margin = wordle_results$margin,
        crossword_won_by = crossword_results$wonBy,
        crossword_margin = crossword_results$margin
      )
      sheet_append(ss = sheet_id,
                   data = main_data_to_save,
                   sheet = "results")
    }
    
    # Update results table
    output$resultsTable <-renderDataTable({
      updated_values
      
    })
    
    output$mainSheetTable <-renderDataTable({
      main_sheet_values() %>%
        rename(
          Date = date,
          `Connections Winner` = connections_won_by,
          `Connections Win Margin` = connections_margin,
          `Wordle Winner` = wordle_won_by,
          `Wordle Win Margin` = wordle_margin,
          `Crossword Winner` = crossword_won_by,
          `Crossword Win Margin` = crossword_win_margin
        )
    })
  })
}

shinyApp(ui, server)