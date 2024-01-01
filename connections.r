library(tidyverse)

# Sample inputs
a_connections <- "Connections 
Puzzle #204
🟩🟩🟩🟩
🟪🟪🟪🟪
🟨🟨🟨🟨
🟦🟦🟦🟦
"

b_connections <- "
Connections 
Puzzle #204
🟩🟩🟩🟨
🟦🟦🟦🟦
🟩🟨🟩🟩
🟨🟨🟨🟨
🟩🟩🟩🟩
🟪🟪🟪🟪
"


process_connections <- function(connections, player) {
  return(
    read_lines(connections) %>%
      as_tibble() %>%
      filter(str_detect(value, '^([🟩🟦🟨🟪]+)$')) %>%
      mutate(
        color = case_when(
          str_detect(value, '^🟩+$') ~ 'green',
          str_detect(value, '^🟦+$') ~ 'blue',
          str_detect(value, '^🟨+$') ~ 'yellow',
          str_detect(value, '^🟪+$') ~ 'purple'
        ),
        turn = row_number(),
        player = player
      )
  )
}

judge_connections <- function(a, b) {
  combined_data <- bind_rows(a, b)
  
  player_status <- combined_data %>%
    filter(!is.na(color)) %>%
    group_by(player) %>%
    summarise(
      solved = n_distinct(color),
      wonGame = solved == 4,
      last_turn = max(turn),
      .groups = 'drop'
    )
  
  if (all(player_status$wonGame)) {
    # Both players won, find who won in fewer turns
    min_turns_winner <- player_status %>%
      filter(wonGame) %>%
      slice_min(last_turn) %>%
      pull(player)
    game_status <- min_turns_winner
    margin <- abs(diff(player_status$last_turn))
  } else if (any(player_status$wonGame)) {
    # Only one player won
    game_status <- player_status$player[player_status$wonGame]
    winner_last_turn <- max(player_status$last_turn[player_status$wonGame])
    loser_last_turn <- max(player_status$last_turn[!player_status$wonGame])
    margin <- abs(winner_last_turn - loser_last_turn)
  } else {
    # No winner
    game_status <- "No winner"
    margin <- NA_integer_
  }
  
  tibble(wonBy = game_status, margin = margin) %>%
    distinct()
}


a_c <- process_connections(a_connections, "player1")
b_c <- process_connections(b_connections, "player2")
judge_connections(a_c, b_c)
