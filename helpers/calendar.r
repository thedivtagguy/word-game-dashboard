library(tidyverse)

df  <-  tibble(full_date = seq(dmy(paste0("01/01/", format(Sys.Date(), "%Y"))),
                               dmy(paste0("31/12/", format(Sys.Date(), "%Y"))),
                               "days")) %>%
  mutate(
    weekday = lubridate::wday(full_date, label = T, week_start = 7),
    month = lubridate::month(full_date, label = T),
    date = lubridate::yday(full_date),
    week = lubridate::epiweek(full_date)
  )

df$week[df$month == "Dec" & df$week == 1] = 53
df <- df %>%
  group_by(month) %>%
  mutate(monthweek = 1 + week - min(week))

generate_heatmap <- function(todays_data) {
  
  current_year <- format(Sys.Date(), "%Y")
  todays_data <- todays_data %>%  
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    filter(format(date, "%Y") == current_year)  # Only use current year's data
  p <- df %>%
    left_join(todays_data, by = c("full_date" = "date")) %>%
    group_by(date) %>%
    mutate(player_status = case_when(
      "Aman" %in% player & "Rhea" %in% player ~ "Both",
      "Aman" %in% player ~ "Aman",
      "Rhea" %in% player ~ "Rhea",
      TRUE ~ NA_character_
    )) %>% 
    replace_na(list(player_status = "None")) %>%
    
    ggplot(aes(weekday, -monthweek, fill = player_status)) +
    geom_tile(colour = "gray70")  +
    geom_text(aes(label = ifelse(player_status != "None", substr(player_status, 1, 1), "")), vjust = 0.5, hjust = 0.5, size = 3, color = "white") +  
    scale_fill_manual(
      values = c(
        "Both" = "#610345",
        "Aman" = "#044B7F",
        "Rhea" = "#9DAD6F",
        "None" = "#F7F0F5"
      )
    ) +
    theme_minimal() +  
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 10),
      panel.border = element_rect(
        colour = "transparent",
        fill = NA,
        size = 1
      ),
      legend.position = "top", 
      legend.direction = "horizontal",  
      legend.justification = "center" ,
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
    ) +
    facet_wrap(~ month, nrow = 4, ncol = 3, scales = "free") +
    labs(
      fill = "",
      title = paste("Game plays in", format(Sys.Date(), "%Y")),
      subtitle = "Daily participation of Aman and Rhea, minimum two plays"
    )
  return(p) 
}


calculate_streaks <- function(data, player) {
  data %>%
    filter(player == !!player) %>%
    arrange(date) %>%
    mutate(
      streak_group = cumsum(c(1, diff(as.Date(date)) != 1)),
      streak_length = sequence(rle(streak_group)$lengths)
    ) %>%
    summarise(
      current_streak = if_else(max(date) == Sys.Date(), 
                               last(streak_length), 
                               0),
      longest_streak = max(streak_length)
    )
}






