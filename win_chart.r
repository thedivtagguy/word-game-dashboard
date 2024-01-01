library(tidyverse)
library(ggbump)

df <- read_csv("dummy_data.csv")



long_df <- df %>%
  select(date, ends_with("won_by")) %>%
  pivot_longer(cols = -date, names_to = "game_type", values_to = "winner") %>%
  mutate(game_type = str_replace(game_type, "_won_by", "")) %>%
  
  inner_join(
    df %>%
      # Select and pivot 'margin' columns
      select(date, ends_with("margin")) %>%
      pivot_longer(cols = -date, names_to = "game_type", values_to = "margin") %>%
      mutate(game_type = str_replace(game_type, "_margin", "")),
    by = c("date", "game_type")
  )


long_df %>%
  group_by(date, game_type) %>%
  arrange(date, game_type, desc(margin)) %>%
  mutate(rank = ifelse(winner == "A", 1, 2))%>% 
  ggplot(aes(x = date, y = rank, color = winner)) +
  geom_bump() +
  facet_wrap(~game_type) +
  theme_minimal() +
  labs(title = "Ranking Changes of Players A and B", x = "Date", y = "Rank")

ggplot(long_df, aes(x = date, y = margin, group = game_type, color = winner)) +
  geom_line() +
  facet_wrap(~game_type) +
  theme_minimal() +
  labs(title = "Margin of Victory Over Time", x = "Date", y = "Margin")
