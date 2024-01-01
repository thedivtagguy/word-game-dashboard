# playerDataCheck.R

playerDataCheck <- function(session, values, input, output) {
  # Check if player's data is already submitted for today
  check_player_data <- reactive({
    selected_player <- input$player
    todays_data <- values() %>%
      filter(date == as.Date(Sys.Date()), player == selected_player)
    
    list(
      wordle_filled = any(!is.na(todays_data$wordle)),
      connections_filled = any(!is.na(todays_data$connections)),
      crossword_filled = any(!is.na(todays_data$crossword))
    )
  })
  
  # Observe changes in player selection or data update
  observe({
    player_data_status <- check_player_data()
    
    # Disable fields based on whether the data is already filled
    shinyjs::disable("submit")  # Default to disable submit

    # Enable input fields based on data status
    if (!player_data_status$wordle_filled) {
      shinyjs::enable("player_wordle")
    } else {
      shinyjs::disable("player_wordle")
    }
    
    if (!player_data_status$connections_filled) {
      shinyjs::enable("player_connections")
    } else {
      shinyjs::disable("player_connections")
    }
    
    if (!player_data_status$crossword_filled) {
      shinyjs::enable("player_crossword")
    } else {
      shinyjs::disable("player_crossword")
    }
    
    # Enable submit if any field is enabled
    if (!player_data_status$wordle_filled ||
        !player_data_status$connections_filled ||
        !player_data_status$crossword_filled) {
      shinyjs::enable("submit")
    }
  })
}
