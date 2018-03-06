# Try applying double transfer algorithm
# Disadvantages - significantly slower. Wastes money on bench players.
# Advantages - appears to converge in fewer iterations. More globally optimal than single transfer solution.



# Get all fpl pairs
fpl.3$dum <- 1
fplsquad <- fpl.3 %>%
  select(dum, id,  first_name.x, second_name.x, pos, team, now_cost, xp) %>%
  inner_join(select(fpl.3, id, dum, first_name.x, second_name.x, pos, team, now_cost, xp), by = 'dum') %>%
  filter(!(first_name.x.x == first_name.x.y & second_name.x.x == second_name.x.y)) %>%
  mutate(pos = paste(pos.x, pos.y, sep="-"),
         price = now_cost.x + now_cost.y,
         xp = xp.x + xp.y) %>%
  select(id.x,
         id.y,
         first_name.x.x,
         second_name.x.x,
         first_name.x.y,
         second_name.x.y,
         pos, price, xp)

optim_double <- function(trans0, fpl.3, fplsquad, bank) {
  
  # Get all squad pairs
  trans0$dum <- 1
  squad <- trans0 %>%
    select(dum,  element, first_name.x, player_name, pos, team, price, xp) %>%
    inner_join(trans0, by = 'dum') %>%
    filter(!(first_name.x.x == first_name.x.y & player_name.x == player_name.y)) %>%
    mutate(pos = paste(pos.x, pos.y, sep="-"),
           price = price.x + price.y,
           xp = xp.x + xp.y) %>%
    select(element.x,
           first_name.x.x,
           player_name.x,
           element.y,
           first_name.x.y,
           player_name.y,
           pos, price, xp)
  
  # Remove duplicates
  squad <- squad[!duplicated(data.frame(t(apply(squad[,c(2,4)], 1, sort)), squad$price)),]
  
  # Double transfers
  double <- inner_join(squad, fplsquad, by = 'pos') %>%
    mutate(xpdiff = xp.y - xp.x) %>%
    filter(price.x + bank >= price.y,
           !id.x %in% trans0$element,
           !id.y %in% trans0$element) %>%
    group_by(pos) %>%
    mutate(rank = rank(desc(xpdiff), ties.method = 'first')) %>%
    filter(rank <= 5) %>%
    arrange(desc(xpdiff))
  
  # Remove duplicates
  if (nrow(double) > 0) double <- double[!duplicated(data.frame(t(apply(double[,c('second_name.x.x','second_name.x.y')], 1, sort)), double$price.x)),]
  
  # Check if transfer increases xp
  if (double$xpdiff[1] > 0) {
    # Choose transfers out and in
    trans_out <- c(double$element.x[1], double$element.y[1])
    trans_in <- c(double$id.x[1], double$id.y[1])
    
    # Ids after transfers
    new_ids <- append(trans0$element[!trans0$element %in% trans_out], trans_in)
    
    # Get team after transfers
    trans <- fpl.3[fpl.3$id %in% new_ids,] %>%
      mutate(position = row_number(), dum =1, 'player_name'= web_name) %>%
      select(position, player_name, now_cost, id, first_name.x, web_name, pos, team, goalprob, xp, dum)
    names(trans) <- names(trans0)
    
    # Update bank
    bank <<- bank - (double$price.y[1] - double$price.x[1])
  } else {
    
    # Alert
    print("Optimal team reached")
    
    # Return original team
    trans <- trans0
    
  }
  
  return(trans)
}



# Initialise loop
i <- 0
bank <- 10
iter_max <- 20
trans0 <- myteam2
trans1 <- optim_double(trans0, fpl.3, fplsquad, bank)

# Get best single transfer team
while (sum(trans1$xp) > sum(trans0$xp) & i <= iter_max) {
  trans0 <- trans1
  trans1 <- optim_double(trans0, fpl.3, fplsquad, bank)
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
