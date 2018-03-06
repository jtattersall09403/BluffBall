
library(dplyr)
library(FLSSS)
library(dummies)
library(data.table)

load('./.RData')

# Set up fpl dummy data
  n <- 20
  teams.fpl <- paste0('team', round(runif(n = n, min = 1, max = 20),0))
  pos <- round(runif(n, 1, 4))
  now_cost <- round(rnorm(n, 70, 15),0)
  xp <- round(rnorm(n, 4, 1.2), 2)
  fpl.3 <- data.frame(id = 1:n, team = teams.fpl, pos, now_cost, xp)
  
# ---- Select legal dreamteam in first pass -------
  # Filter fpl data
  dreamteamdat <- fpl.3 %>%
    data.frame %>%
    select(id, web_name, team, pos, now_cost, xp) %>%
    filter(xp > 0, !is.na(xp))
    
  # Set up optimisation data
  mV <- dreamteamdat %>%
    data.frame %>%
    dummy.data.frame(names = 'pos') %>%
    select(-id, -team, -web_name)
  
  # Save out df for optimisation in python
  #fwrite(mV, '~/Python/dreamteam.csv')
  
  # Get theoretical max xp
  xpmax <- dreamteamdat %>% arrange(desc(xp)) %>% slice(1:15) %>% summarise(n = sum(xp)) %>% unlist
  xpmax
  
  rst <- mmFLknapsack(15, mV, lbound = c(-Inf, 0.75*xpmax), ubound = c(1000, Inf))
  
  # Get all solutions
  rst <- mmFLknapsack(15,
                      mV,
                      lbound = c(2, 5, 5, 3, -Inf, 0.75*xpmax),
                      ubound = c(2, 5, 5, 3, 1000, Inf),
                      tlimit = 240,
                      singleTimeLimit = 120,
                      totalSolutionNeeded = 1)
  
  # Function to check teams. This one optimises the full squad.
  checkteam <- function(i) {
    tmp = fpl.3[rst[[i]],] %>%
      group_by(team) %>%
      mutate(num = n())
    
    return(data.frame(index = i, legal = ifelse(max(tmp$num) <= 3, TRUE, FALSE), xp = sum(tmp$xp)))
  }
  
  # Function to check teams. This one optimises the first 11. It will be slower.
  checkteam2 <- function(i) {
    tmp = dreamteamdat[rst[[i]],]
    
    # Only select on positions
    mV2 <- tmp %>%
      ungroup %>%
      select(pos) %>%
      data.frame %>%
      dummy.data.frame(names = 'pos')
    
    # Get valid combinations for first 11
    rst2 <- mmFLknapsack(11, mV2, lbound = c(1, 3, 2, 1), ubound = c(1, 5, 5, 3))
    
    # Get best first 11
    index11 <- do.call(rbind, lapply(1:length(rst2), function(x) data.frame(index = x, xp = sum(tmp[rst2[[x]],'xp'])))) %>%
      arrange(desc(xp)) %>%
      slice(1) %>%
      select(index) %>%
      unlist
    
    # Record first 11
    first11 <- tmp[rst2[[index11]],]
    
    # Check players per team
    tmp = tmp %>%
      group_by(team) %>%
      mutate(num = n())
    
    # Record progress
    print(paste('Processed', i, 'of', length(rst)))
    
    # Return object
    return(data.frame(index = i, legal = ifelse(max(tmp$num) <= 3, TRUE, FALSE), xp = sum(first11$xp)))
  }
  
  # Get legality and xp for all affordable team combinations
  result <- lapply(1:length(rst), checkteam2)
  result.2 <- do.call(rbind, result)
  
  # Get best combination
  index <- result.2 %>%
    filter(legal == TRUE) %>%
    arrange(desc(xp)) %>%
    slice(1) %>%
    select(index) %>%
    unlist
  
  # Get dreamteam
  dt.1 <- dreamteamdat[rst[[index]],]
  dt.2 <- dt.1 %>%
    ungroup %>%
    select(pos) %>%
    data.frame %>%
    dummy.data.frame(names = 'pos')
  
  # Get valid combinations for first 11
  rst3 <- mmFLknapsack(11, dt.2, lbound = c(1, 3, 2, 1), ubound = c(1, 5, 5, 3))
  
  # Get best first 11
  index11 <- do.call(rbind, lapply(1:length(rst3), function(x) data.frame(index = x, xp = sum(dt.1[rst3[[x]],'xp'])))) %>%
    arrange(desc(xp)) %>%
    slice(1) %>%
    select(index) %>%
    unlist
  
  # Record first 11
  first11 <- dt.1[rst3[[index11]],] %>%
    arrange(pos)
  
  # Full squad - first 11 plus subs
  first11 %>% rbind(filter(dt.1, !id %in% first11$id))
  
  # Show total xp and cost
  sum(first11$xp)
  sum(dt.1$now_cost)
  
  # And as a proportion of the xp of the top 11 players (no constraints)
  sum(dt.1$xp)/sum(sort(xp, decreasing = T)[1:15])
  
  
  