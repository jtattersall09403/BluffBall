require(jsonlite)
require(dplyr)
require(ggplot2)
require(ggrepel)
require(engsoccerdata)
library(devtools)
library(fplr)
library(FLSSS)

#--------------------------------------------------------------------------
# getFPLSummary()
#--------------------------------------------------------------------------
# Get summary of all FPL player statistics from official API
#--------------------------------------------------------------------------

getFPLSummary <- function() {
  require(jsonlite)
  
  # get summarised player data
  player_details <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$elements
  
  # match player team and position to keys
  pos_key <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$element_types
  team_key <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$teams
  player_details$pos <- pos_key[match(player_details$element_type, pos_key$id), "singular_name"]
  player_details$team <- team_key[match(player_details$team, team_key$id), "name"]
  
  
  #change variable classes
  player_details$team <- as.factor(player_details$team)
  player_details$ict_index <- as.factor(player_details$ict_index)
  player_details <- transform(player_details, pos = factor(pos, levels=c("Goalkeeper", "Defender", "Midfielder", "Forward")))
  
  return(player_details)
}


# Function to check if combination is legal
legal <- function(team) {
  
  # Initialise result
  result <- TRUE
  
  # Count players in each position
  team2 <- team %>%
    group_by(pos) %>%
    summarise(num = n()) %>%
    dcast( . ~ pos, value.var = 'num')
  
  # Check all positions present
  if (sum(points$pos %in% names(team2)) == 4) {
    
    # Check correct number in each position
    if (team2$Goalkeeper != 1 |
        team2$Defender < 3 |
        team2$Defender > 5 |
        team2$Midfielder < 2 |
        team2$Midfielder > 5 |
        team2$Forward > 3 |
        team2$Forward < 1) {result <- FALSE}
    
  } else {
    result <- FALSE 
  }
  
  return(result)
}

# Squad legality
squadlegal <- function(team) {
  
  # Initialise result
  result <- TRUE
  
  # Count players in each team
  team2 <- team %>%
    group_by(team) %>%
    summarise(num = n())
    
  if (max(team2$num) > 3) result <- FALSE
  
  return(result)
}

# ----------------------- Subsitution optimisation --------------------


# # data
# data <- data.frame(id = 1:15,
#                    pos = as.factor(c(1,1,
#                            rep(2,5),
#                            rep(3,5),
#                            rep(4,3)))) %>%
#   mutate(now_cost = round(rnorm(15, 66, 15), 1),
#          xp = round(now_cost*runif(15, min = 0.05, max = 0.1), 2))
# 
# # Check
# data

getBestTeam <- function(data, pos = "pos") {
  # formulate multidimensional vector
  mV=as.data.frame(model.matrix(as.formula(paste("~", pos, "-1")), data))
  
  # formulate lbound ubound
  lbound=c(1, 3, 2, 1)
  
  # try if a portfolio with total expect value no less than 75% of the maximum
  # can be found. Change "0.75" manually or in loop to detect the optimal
  ubound=c(1, 5, 5, 3)
  
  # singleTimeLimit and tlimit shall be changed for heavier task. The 5s is
  # merely for passing R package publish requirement
  rst=mmFLknapsack(11, mV, lbound, ubound, totalSolutionNeeded = 1000, tlimit=5, randomizeTargetOrder = FALSE)
  
  # Which has higest xp?
  index <- which.max(lapply(rst, function(x) sum(data[x, 'xp'])))
  bestTeam = data[rst[[index]],]
  
  # Create team
  myteam3 <- bestTeam %>%
    mutate(order = case_when(pos == 'Goalkeeper' ~ 1,
                             pos == 'Defender' ~ 2,
                             pos == 'Midfielder' ~ 3,
                             pos == 'Forward' ~ 4)) %>%
    arrange(order)
  
  # Mark captain
  myteam3 <- myteam3 %>% mutate(captain = ifelse(xp == max(myteam3$xp), 1, 0))
  
} 

# Team after transfers
transfers <- function(myteam2, fpl.3, trans_in, trans_out) {
  
  # Ids after transfers
  new_ids <- append(myteam2$element[!myteam2$element %in% trans_out], trans_in)
  
  # Get team after transfers
  trans <- fpl.3[fpl.3$id %in% new_ids,] %>%
    mutate(position = row_number(), dum =1, 'player_name'= web_name) %>%
    select(position, player_name, now_cost, id, first_name.x, web_name, pos, team, goalprob, xp, dum)
  names(trans) <- names(myteam2)
  
  # Get best new team
  myteam_tran <- getBestTeam(trans)
  
  return(myteam_tran)
}

# Define points sim function
pointssim <- function(x, teamdetails) {
  
  x <- teamdetails %>% filter(element == x) %>%
    mutate(goalprob.y = goalprob/(1-prob0),
           goalprob1 = goalprob1/(1-prob0),
           probBrace = probBrace/(1-prob0),
           probHt = probHt/(1-prob0))
  
  # Appearances
  ap <- sample(0:2, 50000, prob=c(x$prob0, x$probless60, x$prob60), replace = TRUE)
  
  # Team clean sheets
  tcs <- sample(0:1, 50000, prob=c(1-x$cs, x$cs), replace = TRUE) * points$cleansheet[match(x$pos.x, points$pos)]
  
  # Player clean sheets
  cs <- ifelse(ap > 1, tcs, 0)
  
  # Assists
  as.1 <- sample(0:1, 50000, prob=c(1-x$probas, x$probas), replace = TRUE) * 3
  as <- ifelse(ap > 0, as.1, 0)
  
  # Goals
  g1 <- sample(0:3, 50000,
              prob=c(1-x$goalprob.y, x$goalprob1, x$probBrace, x$probHt), replace = TRUE) * points$goal[match(x$pos.x, points$pos)]
  g <- ifelse(ap == 0, 0, g1)
  
  # Total points
  p <- cbind(ap, cs, as, g) %>%
    rowSums() * (x$captain + 1)
  
  return(p)
}

# Define function to generate a team's points in one simulation
pointssim <- function(x, teamdetails, n) {
  
  x <- teamdetails %>% filter(element == x) %>%
    mutate(goalprob.y = goalprob/(1-prob0),
           goalprob1 = goalprob1/(1-prob0),
           probBrace = probBrace/(1-prob0),
           probHt = probHt/(1-prob0))
  
  # Appearances
  ap <- sample(0:2, n, prob=c(x$prob0, x$probless60, x$prob60), replace = TRUE)
  
  # Team clean sheets
  tcs <- sample(0:1, n, prob=c(1-x$cs, x$cs), replace = TRUE) * points$cleansheet[match(x$pos.x, points$pos)]
  
  # Player clean sheets
  cs <- ifelse(ap > 1, tcs, 0)
  
  # Assists
  as.1 <- sample(0:1, n, prob=c(1-x$probas, x$probas), replace = TRUE) * 3
  as <- ifelse(ap > 0, as.1, 0)
  
  # Goals
  g1 <- sample(0:3, n,
               prob=c(1-x$goalprob.y, x$goalprob1, x$probBrace, x$probHt), replace = TRUE) * points$goal[match(x$pos.x, points$pos)]
  g <- ifelse(ap == 0, 0, g1)
  
  # Total points
  p <- cbind(ap, cs, as, g) %>%
    rowSums() * (x$captain + 1)
  
  return(p)
}

# Define function to draw team formation
teamvis <- function(myteam2) {
  
  myteam2[1:11,] %>%
    group_by(pos) %>%
    mutate(rank = row_number()) %>%
    mutate(av = mean(rank)) %>%
    mutate(x = 2*(rank-av) + 5,
           y = (as.integer(pos)-1)*-10,
           xp = prettyNum(round(xp, 2), digits = 2)) %>%
    ggplot(aes(x=x, y=y)) +
    geom_segment(x = 0, y = -16.5, xend = 10, yend = -16.5, color = 'white') +
    geom_segment(x = 0, y = -35, xend = 10, yend = -35, color = 'white') +
    geom_segment(x = 0, y = 0, xend = 0, yend = -35, color = 'white') +
    geom_segment(x = 10, y = 0, xend = 10, yend = -35, color = 'white') +
    geom_segment(x = 0, y = 0, xend = 10, yend = -0, color = 'white') +
    geom_segment(x = 3, y = -5, xend = 7, yend = -5, color = 'white') +
    geom_segment(x = 3, y = -0, xend = 3, yend = -5, color = 'white') +
    geom_segment(x = 7, y = -0, xend = 7, yend = -5, color = 'white') +
    geom_point(aes(colour = team), size = 7) +
    scale_colour_manual(name = 'team', values = c("Arsenal"="red1",
                                                  "Bournemouth"="red4",
                                                  "Brighton"="deepskyblue",
                                                  "Burnley"="deeppink4",
                                                  "Chelsea"="blue3",
                                                  "Crystal Palace"="blue",
                                                  "Everton"="navy",
                                                  "Huddersfield"="skyblue2",
                                                  "Leicester"="dodgerblue4",
                                                  "Liverpool"="red3",
                                                  "Man City"="skyblue1",
                                                  "Man Utd"="red2",
                                                  "Newcastle"="grey19",
                                                  "Southampton"="firebrick1",
                                                  "Stoke"="red",
                                                  "Swansea"="oldlace",
                                                  "Spurs"="white",
                                                  "Watford"="gold",
                                                  "West Brom"="navyblue",
                                                  "West Ham"="firebrick4"))+
    geom_text(aes(label=paste0(player_name, '\n(', xp, ')')),vjust=1.5) +
    expand_limits(y = c(3,-37), x = c(-0.5,10.5)) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background = element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_rect(fill = 'palegreen3'))
  
}


# Automatic subs for dreamteam
autosub <- function(dt.last) {
  # Auto subs
  dt.last$order <- 1:15
  dt.last$dum <- 1
  autosubs <- dt.last[1:11,] %>%
    group_by(pos) %>%
    mutate(numpos = n()) %>%
    mutate(minperpos = case_when(pos == 'Goalkeeper' ~ 1,
                                 pos == 'Defender' ~ 3,
                                 pos == 'Midfielder' ~ 2,
                                 pos == 'Forward' ~ 1),
           minpos = ifelse(numpos == minperpos, 1, 0)) %>%
    filter(event_points == 0) %>%
    left_join(dt.last[12:15,], by = 'dum') %>%
    filter(pos.x == pos.y | minpos == 0) %>%
    filter(event_points.y > 0) %>%
    arrange(order.y) %>%
    ungroup %>%
    slice(1) %>%
    select(element.x, element.y)
  
  # If any eligible auto subs, make them
  while (nrow(autosubs) > 0) {
    autosubs <- unlist(autosubs)
    
    dt.last.2<- data.frame(element = replace(dt.last$element,
                                             match(autosubs, dt.last$element),
                                             dt.last$element[c(match(autosubs, dt.last$element)[2],
                                                               match(autosubs, dt.last$element)[1])]
    )
    ) %>%
      inner_join(dt.last)
    
    # Update team
    dt.last <- dt.last.2 %>%
      slice(1:11) %>%
      arrange(as.integer(pos)) %>%
      rbind(dt.last.2[12:15,])
    
    # Check if any more to do
    autosubs <- dt.last[1:11,] %>%
      group_by(pos) %>%
      mutate(numpos = n()) %>%
      mutate(minperpos = case_when(pos == 'Goalkeeper' ~ 1,
                                   pos == 'Defender' ~ 3,
                                   pos == 'Midfielder' ~ 2,
                                   pos == 'Forward' ~ 1),
             minpos = ifelse(numpos == minperpos, 1, 0)) %>%
      filter(event_points == 0) %>%
      left_join(dt.last[12:15,], by = 'dum') %>%
      filter(pos.x == pos.y | minpos == 0) %>%
      filter(event_points.y > 0) %>%
      ungroup %>%
      arrange(order.y) %>%
      slice(1) %>%
      select(element.x, element.y)
  }
  
  # Keep relevant variables
  dt.last <- dt.last[,1:8]
  
  return(dt.last)
}
