library(tidyverse)


process_wordle <- function(wordle, player) {
  return(
    read_lines(wordle) %>%
      as_tibble() %>%
      filter(str_detect(value, '^([⬜🟩🟨⬛]+)$')) %>%
      mutate(
        turn = row_number(),
        player = player,
        solved = str_detect(value, '^🟩{5}$')
      )
  )
}

judge_wordle <- function(a, b) {
  combined_data <- bind_rows(a, b)
  
  player_status <- combined_data %>%
    group_by(player) %>%
    summarise(
      attempts = max(turn),
      solved = any(solved),
      .groups = 'drop'
    )
  
  game_status <- if (all(player_status$solved) && n_distinct(player_status$attempts) == 1) {
    "Tie"
  } else if (any(player_status$solved)) {
    player_status$player[which.min(player_status$attempts)]
  } else {
    "No winner"
  }
  
  margin <- if (sum(player_status$solved) >= 1) {
    attempts_difference <- diff(player_status$attempts)
    if (length(attempts_difference) > 0) {
      abs(attempts_difference)
    } else {
      0
    }
  } else {
    NA_integer_
  }
  
  tibble(wonBy = game_status, margin = margin) %>%
    distinct()
}

todays_wordle <- function(todays_data) {
  a_connections <- todays_data$wordle[todays_data$player == "Aman"]
  b_connections <- todays_data$wordle[todays_data$player == "Rhea"]
  
  a_c <- process_wordle(a_connections, "Aman")
  b_c <- process_wordle(b_connections, "Rhea")
  wordle_result <- judge_wordle(a_c, b_c)
  
  return(wordle_result)
}