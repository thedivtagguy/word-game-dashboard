library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)
library(googledrive)
library(googlesheets4)
library(shinyjs)
source("./helpers/connections.r")
source("./helpers/wordle.r")
source("./helpers/calendar.r")
source("./helpers/playerDataCheck.r")
source('./helpers/crossword.r')
source('./helpers/gameHeatmaps.r')


options(gargle_oauth_email = TRUE,
        gargle_oauth_cache = "./.secrets")

sheet_id <- googledrive::drive_get("word-games")$id

playerTabPanelUI <- function(playerName) {
  tabPanel(playerName, fluidRow(
    div(
      class = "panel-heading",
      style = "padding-left: 15px; padding-right: 15px;",
      h4(paste("Wordle")),
      plotOutput(
        paste("wordleWins", playerName, sep = "_"),
        width = '100%',
        height = '200px'
      )
    ),
    div(
      class = "panel-heading",
      style = "padding-left: 15px; padding-right: 15px;",
      h4(paste("Connections")),
      plotOutput(
        paste("connectionsWins", playerName, sep = "_"),
        width = '100%',
        height = '200px'
      )
    ),
    div(
      class = "panel-heading",
      style = "padding-left: 15px; padding-right: 15px;",
      h4(paste("Mini")),
      plotOutput(
        paste("crosswordWins", playerName, sep = "_"),
        width = '100%',
        height = '200px'
      )
    )
  ))
}


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
                             DT::dataTableOutput("resultsTable"))
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
                          tabsetPanel(
                            id = "player_tabs",
                            playerTabPanelUI("Aman"),
                            playerTabPanelUI("Rhea")
                          )
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
                      DT::dataTableOutput("rawDataTable")
                    )
                  ))
                ))
)

renderPlayerPlots <- function(playerName, output, dataFunc) {
  output[[paste("wordleWins", playerName, sep = "_")]] <- renderPlot({
    makeGameHeatmap(dataFunc, playerName, "wordle", "Win advantage")
  })
  
  output[[paste("connectionsWins", playerName, sep = "_")]] <-
    renderPlot({
      makeGameHeatmap(dataFunc, playerName, "connections", "Win advantage")
    })
  
  output[[paste("crosswordWins", playerName, sep = "_")]] <-
    renderPlot({
      makeGameHeatmap(dataFunc, playerName, "crossword", "Time advantage")
    })
}



server <- function (input, output, session) {
  rawData <- reactive({
    read_sheet(ss = sheet_id, sheet = "gameplays")
  })
  
  calculatedResults <- reactive({
    read_sheet(ss = sheet_id, sheet = "results")
  })
  
  
  output$calendarHeatmap <- renderPlot({
    data <- rawData()
    generate_heatmap(data)
  })
  
  # Game heatmaps
  observe({
    whoHasPlayedToday <- rawData() %>%
      filter(date == Sys.Date()) %>%
      group_by(player) %>%
      summarise(
        wordle_filled = any(!is.na(wordle)),
        connections_filled = any(!is.na(connections)),
        crossword_filled = any(!is.na(crossword)),
        .groups = 'drop'
      )
    
    # Check if both players have all games filled
    if (nrow(whoHasPlayedToday) == 2 &&
        all(whoHasPlayedToday$wordle_filled) &&
        all(whoHasPlayedToday$connections_filled) &&
        all(whoHasPlayedToday$crossword_filled)) {
      showNotification("All three games for both players are filled", type = "message")
    }
    # If some fields still remain, selectively update those
    playerDataCheck(session, rawData, input, output)
    
    today <- as.character(Sys.Date())
    updated_values <- read_sheet(ss = sheet_id, sheet = "gameplays")
    if (!today %in% calculatedResults()$date) {
      updated_values <- updated_values %>%
        mutate(date = as.Date(date)) %>%
        filter(date == today, player %in% c("Aman", "Rhea"))
      
      if (nrow(updated_values) == 2) {
        connections_results <- todays_connections(updated_values)
        crossword_results <- todays_crossword(updated_values)
        wordle_results <- todays_wordle(updated_values)
        
        main_data_to_save <- tibble(
          date = today,
          connections_won_by = connections_results$wonBy,
          connections_margin = connections_results$margin,
          wordle_won_by = wordle_results$wonBy,
          wordle_margin = wordle_results$margin,
          crossword_won_by = crossword_results$wonBy,
          crossword_margin = crossword_results$margin
        ) %>% head(1)
        
        
        sheet_append(ss = sheet_id,
                     data = main_data_to_save,
                     sheet = "results")
      }
    }
    
    newResults <- reactive({
      read_sheet(ss = sheet_id, sheet = "results")
    })
    renderPlayerPlots("Aman", output, newResults())
    renderPlayerPlots("Rhea", output, newResults())
  })
  
 
  observeEvent(input$submit, {
    newInputData <- tibble(
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
    
    sheet_append(ss = sheet_id,
                 data = newInputData,
                 sheet = "gameplays")
    
  })
  
  output$resultsTable <- renderDataTable({
    calculatedResults() %>%
      rename(
        Date = date,
        `Connections Winner` = connections_won_by,
        `Connections Win Margin` = connections_margin,
        `Wordle Winner` = wordle_won_by,
        `Wordle Win Margin` = wordle_margin,
        `Crossword Winner` = crossword_won_by,
        `Crossword Win Margin` = crossword_margin
      )
  })
}

shinyApp(ui, server)