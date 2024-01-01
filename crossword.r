todays_crossword <- function(data) {
  crossword_times <- data %>% 
    select(player, crossword) %>% 
    mutate(crossword = as.numeric(crossword)) %>% 
    drop_na(crossword)
  
  if (nrow(crossword_times) == 2) {
    winner <- crossword_times %>% 
      slice_min(order_by = crossword, n = 1) %>% 
      pull(player)
    
    margin <- diff(crossword_times$crossword)
    
    return(list(wonBy = winner, margin = margin))
  }
  
  return(list(wonBy = NA_character_, margin = NA_real_))
}
