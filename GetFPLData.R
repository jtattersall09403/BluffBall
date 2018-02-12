require(jsonlite)
require(dplyr)
require(ggplot2)
require(ggrepel)
require(engsoccerdata)
library(devtools)
library(fplr)

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

# ----------------------- Subsitution optimisation --------------------

getBestTeam <- function (myteam2){
    # Get ids of first team and subs  
    first <- myteam2[myteam2$position <= 11,]
    subs <- myteam2[myteam2$position > 11,]
    
    # Get first team players with xp less than at least one sub
    subs_potential <- first %>%
      arrange(xp) %>%
      slice(1:3) %>%
      filter(xp < max(subs$xp))
    first2 <- first[!first$element %in% subs_potential$element,]
    
    # Append to subs
    subs2 <- rbind(subs, subs_potential)
    
    # All possibile combos
    combns <- combn(subs2$element, nrow(subs_potential), simplify=T) %>% as.data.frame
    
    # Get best combination
    xpbest <- 0
    for (x in 1:ncol(combns)) {
      team = rbind(first2, filter(subs2, element %in% combns[,x]))
      
      if (legal(team)) {
        xp <- team %>% ungroup %>% summarise(xp = sum(xp)) %>% unlist
      } else {
        xp <- 0
      }
      
      if (xp > xpbest) {
        bestTeam <- team
        xpbest <- xp
      }
    }
    
    
    # Create team
    myteam3 <- bestTeam %>%
      mutate(order = case_when(pos == 'Goalkeeper' ~ 1,
                               pos == 'Defender' ~ 2,
                               pos == 'Midfielder' ~ 3,
                               pos == 'Forward' ~ 4)) %>%
      arrange(order)
    
    # Mark captain
    myteam3 <- myteam3 %>% mutate(captain = ifelse(xp == max(myteam3$xp), 1, 0))
    return(myteam3)
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
    mutate(goalprob.y = goalprob.y/(1-prob0),
           goalprob1 = goalprob1/(1-prob0),
           probBrace = probBrace/(1-prob0),
           probHt = probHt/(1-prob0))
  
  # Appearances
  ap <- sample(0:2, 100000, prob=c(x$prob0, x$probless60, x$prob60), replace = TRUE)
  
  # Team clean sheets
  tcs <- sample(0:1, 100000, prob=c(1-x$cs, x$cs), replace = TRUE) * points$cleansheet[match(x$pos.x, points$pos)]
  
  # Player clean sheets
  cs <- ifelse(ap > 1, tcs, 0)
  
  # Assists
  as.1 <- sample(0:1, 100000, prob=c(1-x$probas, x$probas), replace = TRUE) * 3
  as <- ifelse(ap > 0, as.1, 0)
  
  # Goals
  g1 <- sample(0:3, 100000,
              prob=c(1-x$goalprob.y, x$goalprob1, x$probBrace, x$probHt), replace = TRUE) * points$goal[match(x$pos.x, points$pos)]
  g <- ifelse(ap == 0, 0, g1)
  
  # Total points
  p <- cbind(ap, cs, as, g) %>%
    rowSums() * (x$captain + 1)
  
  return(p)
}

