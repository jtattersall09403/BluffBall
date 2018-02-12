# Dreamteam - single transfer approach

# Messing around with dream team
# Research into optimisation options not as positive as hoped. Brute force approach??

### How about recursively applying the single transfer algorithm?
optim_single <- function(myteam2, fpl.3, bank) {
  
  # Single transfers
  single <- myteam2 %>%
    inner_join(select(fpl.3, id, web_name, pos, now_cost, team, goalprob, xp), by = c('pos' = 'pos')) %>%
    filter(!id %in% myteam2$element,
           now_cost <= price + bank) %>%
    mutate(xpdiff = xp.y - xp.x) %>%
    arrange(desc(xpdiff))
  
  # Check if transfer increases xp
  if (single$xpdiff[1] > 0) {
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
    print("Optimal team reached")
    
    # Return original team
    trans <- myteam2
    
  }
  
  return(trans)
}



# Initialise loop
i <- 1
bank <- 10
iter_max <- 100
trans0 <- myteam2
trans1 <- optim_single(myteam2, fpl.3, bank)

# Get best single transfer team
while (sum(trans1$xp) > sum(trans0$xp) & i <= iter_max) {
  trans0 <- trans1
  trans1 <- optim_single(trans0, fpl.3, bank)
  i <- i + 1
  print(paste(i, "iterations processed"))
}

View(trans1)

# Get best starting 11
opt_single <- getBestTeam(trans1)
View(opt_single)

# Total xp - yours vs dreamteam
myteam3 %>% mutate(xp = ifelse(captain == 1, xp *2, xp)) %>% ungroup %>% summarise(xp = sum(xp)) %>% unlist
opt_single %>% mutate(xp = ifelse(captain == 1, xp *2, xp)) %>% ungroup %>% summarise(xp = sum(xp)) %>% unlist

trans1[!trans1$element %in% opt_single$element,]



