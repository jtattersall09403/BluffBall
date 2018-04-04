library(dplyr)
library(fplr)
library(ggplot2)
library(parallel)
library(mlbench)
library(caret)

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
  saveRDS('historic data to gw32.rds')

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

