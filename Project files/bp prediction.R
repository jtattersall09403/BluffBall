library(dplyr)
library(fplr)
library(ggplot2)
library(parallel)
library(mlbench)
library(caret)

# Get functions
source('GetFPLData.R')
source('getOdds.R')
source('./Dreamteam/Dreamteam - recursive v2.R')

# Get historic predictions and actual
path <- './Project files/Data archive/'

# Files
files <- list.files(path)

# Load past predictions and details
data <- do.call(rbind,
                lapply(files, function(x) dplyr::select(readRDS(paste0(path, x)), id, web_name, xp, event_points, gw)))

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

# Include relevant variables and packages
clusterEvalQ(cl, {
  library(jsonlite)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(engsoccerdata)
  library(devtools)
  library(fplr)
  library(dplyr)
})

clusterExport(cl, "data")

# Get all data for modelling. Now only takes around 1 minute!
modeldata <- parLapply(cl, sort(unique(data$id)), playerDetailed)

# Close cluster
parallel::stopCluster(cl)

# Append
modeldata.2 <- do.call(rbind, modeldata)

# Save
modeldata.2 %>%
  saveRDS('historic data to gw33.rds')

# Player info
players <- players() %>%
  select(id, team_name, position)

# Try looking at top scorers by fixture
modeldata.3 <- modeldata.2 %>%
  filter(minutes > 0) %>% # Only those that played
  group_by(player_id) %>%
  arrange(round) %>%
  mutate(bonus_lag = lag(bonus),
         bonus_lag = ifelse(is.na(bonus_lag), 0, bonus_lag)) %>%
  mutate(totalbonus = cumsum(bonus_lag)) %>%
  mutate(less60 = as.factor(ifelse(minutes < 60, 1, 0))) %>%
  ungroup %>%
  inner_join(data, by = c("player_id"="id", "round"="gw")) %>%
  group_by(fixture) %>%
  mutate(rank = dense_rank(desc(xp))) %>%
  group_by(fixture, opponent_team) %>%
  mutate(bp_scale = scale(totalbonus)) %>%
  ungroup %>%
  inner_join(players, by=c("player_id"="id")) %>%
  mutate(position=as.factor(position))

# Plot
modeldata.3 %>%
  ggplot(aes(x=rank, y=bonus)) +
  geom_point(alpha = 0.2, colour = "dodgerblue4")

# View
modeldata.3 %>% arrange(fixture, rank) %>% View

# Save
modeldata.3 %>%
  saveRDS('bp_data.rds')

# ---------------

# Get data
modeldata.3 <- readRDS('bp_data.rds')

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the glm model
modelglm <- train(bonus ~ position*rank + bp_scale,
                  data=modeldata.3, method="glm", trControl=control)

# modelrf <- train(bonus ~ position*rank2 + position*assists + position*clean_sheets + totalbonus,
#                  data=filter(fpl.all.3, round != 32), method="rf", trControl=control)

# collect resamples
# results <- resamples(list(glm=modelglm, rf = modelrf))
# 
# # summarize the distributions
# summary(results)
# 
# # boxplots of results
# bwplot(results)
# 
# # dot plots of results
# dotplot(results)

# Summary
summary(modelglm)

# Check the results
modelglm$results

# Test set results
test <- modeldata.3
test$xbp <- ifelse(predict(modelglm, newdata=test) < 0, 0, predict(modelglm, newdata=test))

# RMSE = 0.29. Very similar to training data (.3).
caret::RMSE(test$xbp,test$bonus)

# R2 = .83. Similar to training data.
caret::R2(test$xbp,test$bonus)

# Look at the predictions
test %>%
  arrange(fixture, desc(xbp)) %>%
  mutate(xbp = round(xbp, 2)) %>%
  select(fixture, web_name, position, total_points, xp, rank, bonus, xbp) %>%
  View

# Does this ever disagree with the approach of just using their points rank within fixtures?
fpl.all.3 %>%
  group_by(fixture) %>%
  mutate(xrank = dense_rank(desc(xbp))) %>%
  mutate(xrank = ifelse(xrank > 3, 4, xrank)) %>%
  filter(rank2 > xrank) %>%
  View

# Would you ever pick somebody different using this method?
test2 <- fpl.all.3 %>%
  dplyr::select(1:5, raw_points, xbp, bonus) %>%
  group_by(round, position) %>%
  mutate(xp = raw_points + xbp,
         total_points = raw_points + bonus,
         rank_raw = dense_rank(desc(raw_points))) %>%
  mutate(rank_new = dense_rank(desc(xp)),
         rank_actual = dense_rank(desc(total_points))) %>%
  arrange(position, rank_new, round)

View(test2)

# Do you predict actual points any better by adding in bp predictions?
caret::R2(test2$raw_points, test2$total_points)
caret::R2(test2$raw_points, test2$xp)

# Yes. R squared increases slightly, from .97 to .99.

# ------------- Get data for predictr ------------

modeldata.2 <- readRDS('historic data to gw32.rds')

# Player info
players <- fpl.3

# Try looking at top scorers by fixture
modeldata.3 <- modeldata.2 %>%
  inner_join(select(players, id, web_name, team, pos, goalprob), by=c("player_id"="id")) %>%
  filter(round == 32) %>% # Only those that played
  mutate(raw_points = total_points - bonus) %>%
  group_by(player_id) %>%
  arrange(round) %>%
  mutate(bonus_lag = lag(bonus),
         bonus_lag = ifelse(is.na(bonus_lag), 0, bonus_lag)) %>%
  mutate(totalbonus = cumsum(bonus_lag)) %>%
  ungroup %>%
  group_by(fixture) %>%
  mutate(pts_scale = scale(raw_points),
         goal_scale = scale(goals_scored),
         ass_scale = scale(assists)) %>%
  mutate(ass_scale = ifelse(is.na(ass_scale), 0, ass_scale),
         goal_scale = ifelse(is.na(goal_scale), 0, goal_scale)) %>%
  group_by(fixture, opponent_team) %>%
  mutate(totbp_scale = scale(totalbonus),
         tot_pts = total_points,
         bp = bonus) %>%
  ungroup %>%
  mutate(position=as.factor(pos))

# Prep data for predictr
modeldata.4 <- modeldata.3 %>%
  select(player_id, web_name, position, team, round, fixture,
         clean_sheets,
         pts_scale, goal_scale, ass_scale, raw_points, bonus, total_points)

# Create interaction terms
modeldata.5 <- model.matrix(bonus ~ position*clean_sheets, data=modeldata.4) %>%
  as.data.frame %>%
  select(6:8) %>%
  cbind(modeldata.4, .)

# Save
saveRDS(modeldata.5, 'bp_data_v2.rds')

# Write points difference function
pointsdiff <- function(i) {
  # Progress
  print(paste("Gameweek", i))
  
  pointsdiff <- tryCatch({
    # Optimising raw
    dt.raw = modeldata.3 %>%
      mutate(xp = raw_points,
             now_cost = price * 10) %>%
      filter(round == i) %>%
      dreamteam
    
    # Optimising total
    dt.bp = modeldata.3 %>%
      mutate(xp = total_points,
             now_cost = price * 10) %>%
      filter(round == i) %>%
      dreamteam
    
    # Compare
    dt.raw.2 = dt.raw %>%
      inner_join(select(filter(modeldata.3, round == i), id, total_points),
                 by = c("element"="id")) %>%
      mutate(total_points = ifelse(captain == 1, total_points*2, total_points)) 
    
    #dt.bp
    # dt.raw.2
    
    # Return points difference
    return(sum(dt.bp[1:11,'xp']) - dt.raw.2 %>% slice(1:11) %>% summarise(pts = sum(total_points)))
    
  }, error = function(e) {
    return(0)
  })
  
  return(pointsdiff)
}

# Does including bp improve dreamteam?
dt.results <- sapply(22:max(modeldata.3$round), pointsdiff)

# Results
dt.results.2 <- do.call(rbind, dt.results)
dt.results.2
sum(dt.results.2)/length(dt.results.2)

hist(dt.results.2)

# Check out some of the weird ones
i <- 31
which(dt.results.2 == -2)

# Optimising raw
dt.raw <- modeldata.3 %>%
  mutate(xp = raw_points,
         now_cost = price * 10) %>%
  filter(round == i) %>%
  dreamteam

# Optimising total
dt.bp <- modeldata.3 %>%
  mutate(xp = total_points,
         now_cost = price * 10) %>%
  filter(round == i) %>%
  dreamteam

# Compare
dt.raw.2 <- dt.raw %>%
  inner_join(select(filter(modeldata.3, round == i), id, total_points),
             by = c("element"="id")) %>%
  mutate(total_points = ifelse(captain == 1, total_points*2, total_points)) 

dt.bp
dt.raw.2

# Return points difference
sum(dt.bp[1:11,'xp']) - dt.raw.2 %>% slice(1:11) %>% summarise(pts = sum(total_points))

# Even if we can perfectly predict bonus points AND raw points, adding bp into our 
# selection algorithm only adds about 4 points per round to your score.
# Given that we are unlikely to be able to accurately predict either of these things in practice,
# attempting to do so will sometimes make the team better and sometimes worse.
# At best, the average effect of this in the long run will probably be less than +0.5 points per
# round. This is probably not worth it for the extra effort.

# --------------- Using results from predictr --------------

# Get model
model <- readRDS('./Project files/bp_nnet.rds')

# Try looking at top scorers by fixture
modeldata.3 <- readRDS('bp_data_v2.rds') %>%
  inner_join(select(modeldata.2, round, player_id, price), by = c("player_id", "round")) %>%
  mutate(pos = as.factor(position)) %>%
  mutate(id = player_id)

# Try predicting bp from what actually happened
modeldata.3$xbp <- as.numeric(predict(model$nnet, newdata=modeldata.3))
modeldata.3$xbp <- ifelse(modeldata.3$xbp < 0, 0, modeldata.3$xbp)

# Distributions
hist(modeldata.3$xbp)
hist(modeldata.3$bonus)

# Calculate pseudo-xp
modeldata.3$ps_xp <- modeldata.3$raw_points + modeldata.3$xbp

# R squared improves with this over just raw points? Yes, very slightly
caret::R2(modeldata.3$raw_points, modeldata.3$total_points)
caret::R2(modeldata.3$ps_xp, modeldata.3$total_points)

# Test using dreamteam algo

# Write points difference function
pointsdiff2 <- function(i, data) {
  # Progress
  print(paste("Gameweek", i))
  
  data = as.data.frame(data)
  
  pointsdiff <- tryCatch({
    # Optimising raw
    dt.raw = data %>%
      mutate(xp = raw_points,
             now_cost = price * 10) %>%
      filter(round == i) %>%
      dreamteam
    
    # Optimising pseudo xp
    dt.bp = data %>%
      mutate(xp = ps_xp,
             now_cost = price * 10) %>%
      filter(round == i) %>%
      dreamteam
    
    # Compare
    dt.raw.2 = dt.raw %>%
      inner_join(select(filter(data, round == i), id, total_points),
                 by = c("element"="id")) %>%
      mutate(total_points = ifelse(captain == 1, total_points*2, total_points)) 
    
    dt.bp.2 = dt.bp %>%
      inner_join(select(filter(data, round == i), id, total_points),
                 by = c("element"="id")) %>%
      mutate(total_points = ifelse(captain == 1, total_points*2, total_points)) 
    #dt.bp
    # dt.raw.2
    
    # Return points difference
    return(sum(dt.bp.2[1:11,'total_points']) - sum(dt.raw.2[1:11,'total_points']))
    
  }, error = function(e) {
    return(0)
  })
  
  return(pointsdiff)
}

# Does including bp improve dreamteam?
dt.results <- sapply(1:max(modeldata.3$round), pointsdiff2, data = modeldata.3)

# Results
#dt.results.2 <- do.call(rbind, dt.results)
dt.results.2 <- dt.results
dt.results.2
sum(dt.results.2)/length(dt.results.2)

hist(dt.results.2)

# So, if you know everything that happened in the game, maximising raw score and xbp
# rather than just raw score gets you an average of 0.8 extra points per game. 

# Try it out on gameweek 33 with predicted goals, cs, points etc

# ------------ Using player past performance ------------

# Get predictions
mdl33 <- readRDS('./Project files/Data archive/gw33.rds') %>%
  inner_join(select(filter(modeldata.2, round == 33), player_id, event_bonus=bonus),
             by = c("id"="player_id"))

# Get assumed xbp - everyone's is the same, and equal to the mean
xbp <- modeldata.2 %>%
  filter(minutes > 0, round < 33) %>%
  summarise(bonus = mean(bonus, na.rm = TRUE)) %>% unlist
mdl33$xbp <- ifelse(mdl33$prob60 > 0, xbp, 0)

# Get assumed xp
mdl33$xp2 <- mdl33$xp + mdl33$xbp
mdl33$xp2[is.na(mdl33$xp2)] <- 0
mdl33$xbp[is.na(mdl33$xbp)] <- 0

# Check R2
caret::R2(mdl33$xp2, mdl33$event_points) # .22 for total points
caret::R2(mdl33$xbp, mdl33$event_bonus) # .036 for bp
caret::RMSE(mdl33$xbp, mdl33$event_bonus) # .45 RMSE

# Try heuristic bp prediction method
bpdat <- modeldata.2 %>%
  inner_join(select(mdl33, id, pos, web_name), by = c("player_id"="id")) %>%
  filter(round < 33) %>%
  select(player_id, web_name, pos, clean_sheets, assists, goals_scored, bonus) %>%
  mutate(goals_scored = ifelse(goals_scored > 3, 3, goals_scored),
         assists = ifelse(assists > 1, 1, assists)) %>%
  group_by(player_id, web_name, pos, clean_sheets, assists, goals_scored) %>%
  summarise(bonus = mean(bonus)) %>%
  mutate(goals_scored = as.factor(goals_scored))

# Expand goals to dummies
bpdat.goals <- as.data.frame(model.matrix( ~ goals_scored-1, data=bpdat))

# Append
bpdat.2 <- cbind(as.data.frame(bpdat), bpdat.goals) %>%
  select(-goals_scored, -goals_scored0)

# Generalise
bpgen <- bpdat.2 %>%
  group_by(pos, clean_sheets, assists, goals_scored1, goals_scored2, goals_scored3) %>%
  summarise(bonus = mean(bonus)) %>%
  ungroup %>%
  mutate(outcome_id = row_number()) %>%
  select(outcome_id, pos, clean_sheets, assists, goals_scored1, goals_scored2, goals_scored3, bonus)

# Save generalised version
saveRDS(bpgen, 'general bp per outcome full 17-18 season to gw 32.RDS')

# Get which outcomes are covered in player-level data
bpdat.3 <- bpdat.2 %>%
  inner_join(select(bpgen, -bonus), by = c("pos","clean_sheets","assists","goals_scored1","goals_scored2","goals_scored3")) %>%
  mutate(player_outcome = paste(player_id, outcome_id, sep="-"))

# Set bonus equal to the generalised one when missing
bpdat.4 <- inner_join(unique(select(bpdat.3, player_id, web_name, pos)), bpgen, by = "pos") %>%
  mutate(player_outcome = paste(player_id, outcome_id, sep="-")) %>%
  filter(!player_outcome %in% bpdat.3$player_outcome) %>%
  union(bpdat.3)

# Match on this week's probabilities, and calculate probability of each set of outcomes
# Gives raw xbp
mdl33[is.na(mdl33)] <- 0
xbp.raw.1 <- bpdat.4 %>%
  inner_join(select(mdl33, id, now_cost, prob60, cs, probas, goalprob,
                    probBrace, probHt, xp, event_points), by = c("player_id"="id")) %>%
  mutate(goalprob = goalprob - probBrace - probHt) %>%
  mutate(p1 = cs*clean_sheets + (1-cs)*(clean_sheets-1)*-1,
         p2 = probas*assists + (1-probas)*(assists-1)*-1,
         p3 = goalprob*goals_scored1 + (1-goalprob)*(goals_scored1-1)*-1,
         p4 = probBrace*goals_scored2 + (1-probBrace)*(goals_scored2-1)*-1,
         p5 = probHt*goals_scored3 + (1-probHt)*(goals_scored3-1)*-1) %>%
  mutate(p_out = p1 * p2 * p3 * p4 * p5,
         xbp = p_out*bonus)

xbp.raw.1[is.na(xbp.raw.1)] <- 0

xbp.raw <- xbp.raw.1 %>%
  group_by(player_id) %>%
  summarise(xbp = sum(xbp))

# Match onto fpl data and adjust for starting probabilities
mdl33.2 <- mdl33 %>%
  select(-xbp, -xp2) %>%
  inner_join(xbp.raw, by=c("id"="player_id")) %>%
  mutate(xbp = ifelse(prob60 == 0 | chance_of_playing_next_round == 0, 0, xbp)) %>%
  mutate(xbp = ifelse(is.na(xbp), 0, xbp)) %>%
  select(id, web_name, now_cost, pos, team, xp, xbp, event_points, event_bonus) %>%
  mutate(xp2 = xp + xbp) %>%
  mutate(xp2 = ifelse(is.na(xp2), 0, xp2))

# Check performance in this gameweek
caret::R2(mdl33.2$xp2, mdl33.2$event_points) # .22 for total points
caret::R2(mdl33.2$xbp, mdl33.2$event_bonus) # .037 for bp
caret::RMSE(mdl33.2$xbp, mdl33.2$event_bonus) # .47 RMSE

# Would you have picked a better dream team?
dt <- dreamteam(mdl33) %>%
  inner_join(select(mdl33, id, event_points), by = c("element"="id")) %>%
  mutate(event_points = ifelse(captain==1, 2*event_points, event_points)) %>%
  autosub
dt
sum(dt[1:11,'event_points'])
sum(dt[1:11,'xp'])

# With new xbp
dt.xbp <- mdl33.2 %>%
  mutate(xp = xp2) %>%
  dreamteam %>%
  inner_join(select(mdl33, id, event_points), by = c("element"="id")) %>%
  mutate(event_points = ifelse(captain==1, 2*event_points, event_points)) %>%
  autosub
dt.xbp
sum(dt.xbp[1:11,'event_points'])
sum(dt.xbp[1:11,'xp'])

# In this instance, it made your dreamteam worse - basically because it picked Murray over Rodriguez,
# who scored and got 3 bonus points (he had a 35% chance of scoring, vs Murray's 44% chance).
# Might want to monitor this over time.
