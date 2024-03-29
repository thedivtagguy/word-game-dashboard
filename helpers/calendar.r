library(tidyverse)

df  <-  tibble(full_date = seq(dmy("01/01/2024"),
                               dmy("31/12/2024"),
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

generate_heatmap <- function(todays_data) {
  
  todays_data <- todays_data %>%  mutate(date = as.Date(date, format = "%Y-%m-%d"))
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
      title = "Game plays in 2024",
      subtitle = "Daily participation of Aman and Rhea, minimum two plays"
    )
  return(p) 
}










