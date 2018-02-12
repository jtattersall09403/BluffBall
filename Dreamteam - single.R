# Dreamteam - single transfer approach

# Messing around with dream team
# Research into optimisation options not as positive as hoped. Brute force approach??

### How about recursively applying the single transfer algorithm?
bank <- 10

optim_single <- function(myteam2, fpl.3, bank) {
  
  # Single transfers
  single <- myteam2 %>%
    inner_join(select(fpl.3, id, web_name, pos, now_cost, team, goalprob, xp), by = c('pos' = 'pos')) %>%
    filter(xp.y > xp.x,
           now_cost < price + bank,
           !id %in% myteam2$element) %>%
    mutate(xpdiff = xp.y - xp.x) %>%
    arrange(desc(xpdiff))
  
  # Check if transfer increases xp
  if (!is.na(single$xpdiff[1])) {
    # Choose transfers out and in
    trans_out <- c(single$element[1])
    trans_in <- c(single$id[1])
    
    # Ids after transfers
    new_ids <- append(myteam2$element[!myteam2$element %in% trans_out], trans_in)
    
    # Get team after transfers
    trans <- fpl.3[fpl.3$id %in% new_ids,] %>%
      mutate(position = row_number(), dum =1, 'player_name'= web_name) %>%
      select(position, player_name, now_cost, id, first_name.x, web_name, pos, team, goalprob, xp, dum)
    names(trans) <- names(myteam2)
    
    # Update bank
    bank <<- bank - (single$now_cost[1] - single$price[1])
  } else {
    
    # Alert
    print("optimal team reached")
    
    # Return original team
    trans <- myteam2
    
  }
  
  return(trans)
}

trans1 <- optim_single(myteam2, fpl.3, bank)
trans2 <- optim_single(trans1, fpl.3, bank)

# Initialise loop
trans <- list()
trans[[1]] <- myteam2
iter_max <- 100
for (i in 2:iter_max) {
  
  # Optimise
  trans[[i]] <- optim_single(trans[[i-1]], fpl.3, bank)
  
  # Record progress
  print(paste0(round(100*i/iter_max,1), "% processed"))
  
}

