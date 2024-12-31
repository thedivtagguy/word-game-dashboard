library(testthat)
library(tidyverse)
library(lubridate)

# Set working directory to the project root
setwd("/home/amnbh/Desktop/Dev Projects/personal/wordGames")

# Source all helper files
source("helpers/calendar.r")
source("helpers/connections.r")
source("helpers/crossword.r")
source("helpers/gameHeatmaps.r")
source("helpers/githubStyleHeatmap.r")
source("helpers/wordle.r")

# Get current system time
current_time <- as.POSIXct("2024-12-31 18:42:59 +0530")
current_year <- year(current_time)
next_year <- current_year + 1

# Generate test data spanning current and next year
mock_data <- tibble(
  date = c(
    format(current_time - days(1), "%Y-%m-%d"), 
    format(current_time - days(1), "%Y-%m-%d"),
    format(current_time, "%Y-%m-%d"), 
    format(current_time, "%Y-%m-%d"),
    format(current_time + days(1), "%Y-%m-%d"), 
    format(current_time + days(1), "%Y-%m-%d")
  ),
  player = rep(c("Aman", "Rhea"), 3),
  wordle = c(
    sprintf("Wordle %d 4/6\n⬜⬜🟨⬜⬜\n⬜🟨⬜⬜⬜\n🟩🟩🟩⬜⬜\n🟩🟩🟩🟩🟩", 924),
    sprintf("Wordle %d 3/6\n⬜🟨⬜⬜⬜\n🟩🟩🟩⬜⬜\n🟩🟩🟩🟩🟩", 924),
    sprintf("Wordle %d 3/6\n⬜🟨⬜⬜⬜\n🟩🟩🟩⬜⬜\n🟩🟩🟩🟩🟩", 925),
    sprintf("Wordle %d 4/6\n⬜⬜🟨⬜⬜\n⬜🟨⬜⬜⬜\n🟩🟩🟩⬜⬜\n🟩🟩🟩🟩🟩", 925),
    sprintf("Wordle %d 3/6\n⬜🟨⬜⬜⬜\n🟩🟩🟩⬜⬜\n🟩🟩🟩🟩🟩", 926),
    sprintf("Wordle %d 4/6\n⬜⬜🟨⬜⬜\n⬜🟨⬜⬜⬜\n🟩🟩🟩⬜⬜\n🟩🟩🟩🟩🟩", 926)
  ),
  connections = c(
    sprintf("Connections\nPuzzle #%d\n🟩🟩🟩🟩\n🟪🟪🟪🟪\n🟨🟨🟨🟨\n🟦🟦🟦🟦", 204),
    sprintf("Connections\nPuzzle #%d\n🟩🟩🟩🟩\n🟪🟪🟪🟪\n🟨🟨🟨🟨\n🟦🟦🟦🟦", 204),
    sprintf("Connections\nPuzzle #%d\n🟩🟩🟩🟩\n🟪🟪🟪🟪\n🟨🟨🟨🟨\n🟦🟦🟦🟦", 205),
    sprintf("Connections\nPuzzle #%d\n🟩🟩🟩🟩\n🟪🟪🟪🟪\n🟨🟨🟨🟨\n🟦🟦🟦🟦", 205),
    sprintf("Connections\nPuzzle #%d\n🟩🟩🟩🟩\n🟪🟪🟪🟪\n🟨🟨🟨🟨\n🟦🟦🟦🟦", 206),
    sprintf("Connections\nPuzzle #%d\n🟩🟩🟩🟩\n🟪🟪🟪🟪\n🟨🟨🟨🟨\n🟦🟦🟦🟦", 206)
  ),
  crossword = c("120", "150", "130", "145", "125", "155")
)

# Tests for data filtering
test_that("Data filtering works correctly across year boundary", {
  # Test current year data
  filtered_current <- mock_data %>%
    mutate(date = as.Date(date)) %>%
    filter(format(date, "%Y") == format(current_time, "%Y"))
  
  expect_equal(nrow(filtered_current), 4)  # Should have Dec 30-31 data
  expect_true(all(year(filtered_current$date) == current_year))
  
  # Test next year data
  filtered_next <- mock_data %>%
    mutate(date = as.Date(date)) %>%
    filter(format(date, "%Y") == format(current_time + days(1), "%Y"))
  
  expect_equal(nrow(filtered_next), 2)  # Should only have Jan 1 data
  expect_true(all(year(filtered_next$date) == next_year))
})

# Tests for game results calculation
test_that("Game results calculation works across year boundary", {
  # Test Wordle results for current year
  data_current <- mock_data %>%
    mutate(date = as.Date(date)) %>%
    filter(format(date, "%Y") == format(current_time, "%Y"), 
           date == max(date))
  
  wordle_current <- todays_wordle(data_current)
  expect_false(is.null(wordle_current$wonBy))
  expect_false(is.null(wordle_current$margin))
  
  # Test Wordle results for next year
  data_next <- mock_data %>%
    mutate(date = as.Date(date)) %>%
    filter(format(date, "%Y") == format(current_time + days(1), "%Y"),
           date == max(date))
  
  wordle_next <- todays_wordle(data_next)
  expect_false(is.null(wordle_next$wonBy))
  expect_false(is.null(wordle_next$margin))
  
  # Test Connections results
  connections_current <- todays_connections(data_current)
  expect_false(is.null(connections_current$wonBy))
  expect_false(is.null(connections_current$margin))
  
  connections_next <- todays_connections(data_next)
  expect_false(is.null(connections_next$wonBy))
  expect_false(is.null(connections_next$margin))
})

# Test week number handling
test_that("Week numbers are handled correctly at year boundary", {
  # Test last week of current year
  df_current <- tibble(full_date = seq(as.Date(format(current_time, "%Y-01-01")), 
                                     as.Date(format(current_time, "%Y-12-31")), 
                                     "days")) %>%
    mutate(
      week = epiweek(full_date),
      month = month(full_date)
    )
  
  # Handle last week of year correctly
  df_current <- df_current %>%
    mutate(week = if_else(month == 12 & week == 1, 53L, as.integer(week)))
  
  last_week <- df_current %>% 
    filter(month == 12) %>% 
    pull(week) %>% 
    max()
  expect_equal(last_week, 53)  # Last week should be 53
  
  # Test first week of next year
  df_next <- tibble(full_date = seq(as.Date(format(current_time + days(1), "%Y-01-01")), 
                                  as.Date(format(current_time + days(1), "%Y-12-31")), 
                                  "days")) %>%
    mutate(
      week = epiweek(full_date),
      month = month(full_date)
    )
  first_week <- df_next %>% 
    filter(month == 1) %>% 
    pull(week) %>% 
    min()
  expect_equal(first_week, 1)  # First week should be 1
})
