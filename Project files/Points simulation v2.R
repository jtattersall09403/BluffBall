
library(dplyr)
library(reshape2)
library(ggplot2)
library(parallel)
library(data.table)
library(fastLink)

# -------------- Setup ------------
# Get functions
source('GetFPLData.R')
source('getOdds.R')
source('./Dreamteam/Dreamteam - recursive v2.R')

# Get club lookup
clubs <- fread('./Project files/club lookup.csv')

# Get fantasybet prices
fb <- fread('./Project files/Fantasybet - gw32.csv', encoding = "UTF-8") %>%
  mutate(initial = ifelse(grepl(",", Player), substr(Player, nchar(Player), nchar(Player)), ""),
         web_name = ifelse(grepl(",", Player), substr(Player, 1, nchar(Player) - 3), Player)) %>%
  inner_join(clubs, by = c("Club"="club")) %>%
  mutate(pos = case_when(Pos=="GK" ~ "Goalkeeper",
                         Pos=="DF" ~ "Defender",
                         Pos=="MD" ~ "Midfielder",
                         Pos=="FW" ~ "Forward"))

# --------------- Data matching --------------

# Filter fpl data
fpl.4 <- fpl.3 %>%
  mutate(pos = as.character(pos),
         initial = toupper(substr(first_name.x, 1, 1))) %>%
  select(id, web_name, initial, team, pos)

# Make names consistent
right <- fb

# Get left table
left <- fpl.4

# To do - sort out matching. Why doesn't partial match work? Check against the example.
# Link to FPL data
matches.out <- fastLink(
  dfA = left,
  dfB = right, 
  varnames = c("web_name", "initial","team","pos"),
  stringdist.match = c("web_name", "initial"), # Specifies the variables you want to treat as strings for fuzzy matching
  #partial.match = c("web_name"), # Specifes variables where you want the algorithm to check for partial matches
  verbose = T,
  return.all = T
  #threshold.match = .01 # Match probability threshold. The default is .85, and you can play around with different values
)

# Gives the match rate, estimated falst positive rate (FDR) and estimated false negative rate (FNR)
summary(matches.out)

# Extracts the matched data
a <- matches.out$matches$inds.a
b <- matches.out$matches$inds.b

# Compile matched data
left[a, 'matchindex'] <- b
namesmatched <- cbind(fb[b,],"matchindex"=b, "match"=matches.out$posterior)

matched.data <- left_join(left,
                          namesmatched,
                          by="matchindex")

# Keep most likely match for each
dedup <- matched.data %>%
  group_by(id) %>%
  mutate(rank = ifelse(is.na(match),1, rank(match, ties.method='first'))) %>%
  filter(rank == 1)

# Record data
fpl.4 <- dedup %>%
  select(id, now_cost, Pts) %>%
  inner_join(fpl.3, by="id") %>%
  mutate(now_cost=ifelse(is.na(now_cost.x), round(now_cost.y/5)*5 , now_cost.x*10)) %>%
  ungroup
  

# Check who's missing
View(filter(arrange(fpl.4, desc(form)), is.na(Pts)))

# ----------------------- Get dreamteams -----------------------

# Get dreamteams - full hedge
dt <- list()
dt[[1]] <- dreamteam(fpl.4)
# dt[[2]] <- dreamteam(filter(fpl.4, !id %in% dt[[1]]$element))
# dt[[3]] <- dreamteam(filter(fpl.4, !id %in% dt[[1]]$element, !id %in% dt[[2]]$element))

# Try captain hedge
dt[[1]]

dt[[2]] <- dt[[1]] %>%
  mutate(xp = ifelse(captain==1, xp/2, xp), captain = 0) %>%
  mutate(captain = ifelse(row_number(desc(xp)) == 2, 1, 0),
         xp = ifelse(captain==1, xp*2, xp))
  
dt[[3]] <- dt[[1]] %>%
  mutate(xp = ifelse(captain==1, xp/2, xp), captain = 0) %>%
  mutate(captain = ifelse(row_number(desc(xp)) == 3, 1, 0),
         xp = ifelse(captain==1, xp*2, xp))

# Check all expected points
for (i in 1:length(dt)) print(sum(dt[[i]][1:11, 'xp']))

# Match on other details
dt.details <- inner_join(dt[[1]], fpl.3, by = c('element'='id'))
  
# Get dreamteam based on most popular targets. Make a decision on whether you
# should be maximising overall demand, or demand within positions. Probably the former.
# Get player and team data
fpldat <- getFPLSummary()

# Set weight for current ownership
m <- 0

dt_other <- fpldat %>%
  #group_by(pos) %>%
  mutate(trans_balance = transfers_in_event - transfers_out_event) %>%
  mutate(trans_scale = as.numeric(scale(trans_balance)),
         own_scale = as.numeric(scale(as.numeric(selected_by_percent)))) %>%
  ungroup %>%
  mutate(trans_scale = ifelse(is.na(trans_scale), 0, trans_scale),
         own_scale = ifelse(is.na(own_scale), 0, own_scale)) %>%
  mutate(#xpoints = xp,
         xp = trans_scale + (own_scale*m)) %>%
  filter(xp > 0) %>%
  arrange(desc(xp)) %>%
  dreamteam()

# Match names etc
dt_other <- dt_other %>% select(-xp, -web_name) %>%
  inner_join(select(fpl.3, id, web_name, xp, goalprob), by = c("element"="id")) %>%
  mutate(goalprob = ifelse(is.na(goalprob), 0, goalprob),
         xp = ifelse(is.na(xp), 0, xp)) %>%
  mutate(captain = ifelse(goalprob == max(.$goalprob, na.rm = T), 1, 0)) %>%
  mutate(xp = ifelse(captain==1, xp*2, xp)) %>%
  mutate(web_name = ifelse(captain==1, paste(web_name, "(C)"), web_name)) %>%
  select(element, pos, team, web_name, now_cost, xp, captain)
dt_other
sum(dt_other$now_cost)
sum(dt_other$xp[1:11])

# Match on all details
teamdetails <- inner_join(dt_other, fpl.3, by = c('element'='id'))
teamdetails[is.na(teamdetails)] <- 0

# Simulate points for each player
teamsim.1 <- lapply(teamdetails$element[1:11], pointssim, teamdetails, 5000)
teamsim.2 <- lapply(1:length(teamsim.1), function(i) teamsim.1[[i]] * (dt_other$captain+1)[i] ) 
teamsim <- rowSums(do.call(cbind, teamsim.2))

# Visualise probabilities
teamsim %>%
  as.data.frame %>%
  ggplot(aes(x=`.`)) +
  #geom_histogram(fill = 'dodgerblue3', color = 'dodgerblue4', bins = 30)
  geom_density(fill = 'dodgerblue3', color = 'dodgerblue4')

# Get probability of each possible number of points
weights <- as.numeric(table(teamsim)/length(teamsim))

# This generates a score randomly according to the distrubtion of total points
# sample(unique(teamsim[[1]]), 1, prob=weights[[1]], replace = TRUE)

# Sim function
pointsim <- function(y, maxent, n, compdat) {
  
  # Your points
  #dtp = round(sapply(1:maxent, function(i) sample(unique(teamsim[[i]]), 1, prob=weights[[i]], replace = TRUE)), 0)
  
  # Get simulated points
  sim <- do.call(rbind, lapply(dt.details$element[1:11], pointssim, dt.details, 1))
  dtp.tot <- sapply(1:maxent, function(i) sum(sim*(dt[[i]]$captain[1:11]+1)))
  
  # Get profits
  results <- sapply(1:nrow(compdat), function(i) {
    
    # Generate other points.
    pts <- round(sample(unique(teamsim), compdat$entries[i], prob=weights, replace = TRUE),0)
    pts
    
    # Get return
    if (compdat$comps[i]=="double") prizes[[i]] <- c(rep(compdat$fees[i]*2, (compdat$entries[i] + maxent)/2))
    r <- prizes[[i]][rank(-append(dtp.tot,pts), ties.method = "average")[1:maxent]]
    r <- ifelse(is.na(r), 0, r) - compdat$fees[i]
    r <- sum(r)
    r
  })
  
  # Format as data frame
  results = data.frame(t(results))
  
  # Headers
  names(results) <- paste0(compdat$comps, ' x', maxent)
  
  # Progress
  print(paste('Processed', y, 'of', n))
  
  return(results)
}


