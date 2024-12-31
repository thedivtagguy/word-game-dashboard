library(tidyverse)
# Code from 
# https://dominikkoch.github.io/Calendar-Heatmap/
# dummy <- read_csv("dummy_data.csv")
source('./helpers/githubStyleHeatmap.r')
df  <-  tibble(full_date = seq(dmy(paste0("01/01/", format(Sys.Date(), "%Y"))),
                               dmy(paste0("31/12/", format(Sys.Date(), "%Y"))),
                               "days")) %>%
  mutate(
    weekday = wday(full_date, label = T, week_start = 7),
    month = month(full_date, label = T),
    date = yday(full_date),
    week = epiweek(full_date)
  )

df$week[df$month == "Dec" & df$week == 1] = 53
df <- df %>%
  group_by(month) %>%
  mutate(monthweek = 1 + week - min(week))


makeLong <- function(data) {
  current_year <- format(Sys.Date(), "%Y")
  df <- data %>%
    # Filter for current year and transform data
    mutate(date = ymd(date)) %>%
    filter(format(date, "%Y") == current_year) %>%
    # Select and pivot 'won_by' columns
    select(date, ends_with("won_by")) %>%
    pivot_longer(cols = -date, names_to = "game_type", values_to = "winner") %>%
    mutate(game_type = str_replace(game_type, "_won_by", "")) %>%
    
    # Save as a temporary data frame
    inner_join(
      data %>%
        mutate(date = ymd(date)) %>% 
        filter(format(date, "%Y") == current_year) %>%
        # Select and pivot 'margin' columns
        select(date, ends_with("margin")) %>%
        pivot_longer(cols = -date, names_to = "game_type", values_to = "margin") %>%
        mutate(game_type = str_replace(game_type, "_margin", ""),
               game_type = str_replace(game_type, "_win", "")),
      by = c("date", "game_type")
    )
  return(df)
}



makeGameHeatmap <- function(data,
                            player = "",
                            gameType = "",
                            legendTitle = "") {
  # print(player, gameType)
  long_df <- makeLong(data)
  full <- df %>%
    left_join(long_df, by  = c("full_date" = "date"))
  
  dates <- unique(full$full_date)
  # print(dates)
  newValues <- full %>%
    ungroup() %>% 
    filter(game_type == gameType) %>%
    complete(full_date = dates, game_type = gameType) %>%
    mutate(margin = ifelse(winner == player, margin, NA)) %>%
    pull(margin)
  #print(newValues)
  
  newValues[is.na(newValues)] <- NA_real_
  #print(values)
  
  calendarHeatmap(dates, values = newValues, legendtitle = legendTitle)
}

# makeGameHeatmap(dummy, player = "Aman", gameType = "wordle")
