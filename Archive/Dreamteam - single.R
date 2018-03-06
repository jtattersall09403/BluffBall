# Dreamteam - single transfer approach

# Messing around with dream team
# Research into optimisation options not as positive as hoped. Brute force approach??

### How about recursively applying the single transfer algorithm?
# Disadvantages: wastes money on bench players
# Advantages: provides strength in depth, allows for different formations, first team xp is similar to the
# first 11 optimisation approach (1 point lower, in one trial)

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

# --------------------- Optimise starting 11 ---------------

# Strategy - make bench players as cheap as possible to allow more to spend on first team.
# Achieve this by first picking your best possible current team.
# Then transfer all bench players for their cheapest alternatives, updating the bank.
# Then run the original process just on the starting 11.
# Major limitation - restricts you to the formation the original team is playing in.

### How about recursively applying the single transfer algorithm?
optim_single <- function(trans0, fpl.3, bank) {
  
  # Single transfers
  single <- trans0 %>%
    inner_join(select(fpl.3, id, web_name, pos, now_cost, team, goalprob, xp), by = c('pos' = 'pos')) %>%
    filter(!id %in% trans0$element,
           now_cost <= price + bank) %>%
    mutate(xpdiff = xp.y - xp.x) %>%
    arrange(desc(xpdiff))
  
  # Check if transfer increases xp
  if (single$xpdiff[1] > 0) {
    # Choose transfers out and in
    trans_out <- c(single$element[1])
    trans_in <- c(single$id[1])
    
    # Ids after transfers
    new_ids <- append(trans0$element[!trans0$element %in% trans_out], trans_in)
    
    # Get team after transfers
    trans <- fpl.3[fpl.3$id %in% new_ids,] %>%
      mutate(position = row_number(), dum =1, 'player_name'= web_name, captain = 0) %>%
      select(position, player_name, now_cost, id, first_name.x, web_name, pos, team, goalprob, xp, dum, captain)
    names(trans) <- names(trans0)
    
    # Update bank
    bank <<- bank - (single$now_cost[1] - single$price[1])
  } else {
    
    # Alert
    print("Optimal team reached")
    
    # Return original team
    trans <- trans0
    
  }
  
  return(trans)
}



# Set up first team for optimisation. Myteam3 is result from getBestTeam
subs <- myteam2[!myteam2$element %in% myteam3$element,] %>%
  inner_join(select(fpl.3, id, web_name, pos, now_cost, team, goalprob, xp), by = c('pos' = 'pos')) %>%
  filter(!id %in% myteam2$element,
         now_cost <= price + bank) %>%
  mutate(xpdiff = xp.y - xp.x, pricediff = now_cost - price) %>%
  arrange(pricediff, desc(xpdiff)) %>%
  group_by(id) %>%
  mutate(rank = row_number()) %>%
  filter(rank == 1) %>% # De-dupe - each sub-in can only appear once
  arrange(pricediff, desc(xpdiff)) %>%
  group_by(element) %>% # Select one sub for each
  mutate(rank = row_number()) %>%
  filter(rank == 1)

#Get ids of new subs
newsubs <- myteam2[!myteam2$element %in% myteam3$element,] %>%
  ungroup %>%
  select(element) %>%
  left_join(select(subs, element, id), by = 'element') %>%
  mutate(id = ifelse(is.na(id), element, id)) %>%
  select(id)

# Get sub details
subdeets <- fpl.3[fpl.3$id %in% newsubs$id,] %>%
  arrange(ifelse(pos == 'Goalkeeper',0,1),desc(xp)) %>%
  mutate(position = row_number(), order = row_number() + 11, 'player_name'= web_name, captain = 0) %>%
  select(position, player_name, now_cost, id, first_name.x, web_name, pos, team, goalprob, xp, order, captain)


# Need to append subs to final team

# Initialise loop
i <- 1
bank <- 10
bank <- bank - sum(subs$pricediff)
iter_max <- 100
trans0 <- myteam3
trans1 <- optim_single(myteam3, fpl.3, bank)

# Get best single transfer team
while (sum(trans1$xp) > sum(trans0$xp) & i <= iter_max) {
  trans0 <- trans1
  trans1 <- optim_single(trans0, fpl.3, bank)
  i <- i + 1
  print(paste(i, "iterations processed"))
}


# Put in correct order, mark captain, and append subs
names(subdeets) <- names(trans1)
opt_single <- trans1 %>%
  mutate(order = case_when(pos == 'Goalkeeper' ~ 1,
                                       pos == 'Defender' ~ 2,
                                       pos == 'Midfielder' ~ 3,
                                       pos == 'Forward' ~ 4)) %>%
  arrange(desc(xp)) %>%
  mutate(captain = ifelse(row_number()==1, 1, 0)) %>%
  arrange(order) %>%
  rbind(subdeets)

View(opt_single)

# Total xp - yours vs dreamteam
myteam3 %>% mutate(xp = ifelse(captain == 1, xp *2, xp)) %>% ungroup %>% summarise(xp = sum(xp)) %>% unlist
opt_single %>% filter(order <= 11) %>% mutate(xp = ifelse(captain == 1, xp *2, xp)) %>% ungroup %>% summarise(xp = sum(xp)) %>% unlist

# Check total price. Remember during the season this may be different to 100 mil.
sum(opt_single$price)
bank


