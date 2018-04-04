library(dplyr)
library(jsonlite)
library(ggplot2)

players <- function() {
  
  # look-up table of player statuses
  status <- data.frame(id = c("a", "d", "i", "s", "u"), player_status = c("Available", "Doubtful", "Injured", 
                                                                          "Suspended", "Unavailable"))
  
  # read in json player data, simplify vectors to make easy transfer to dataframe
  extract <- jsonlite::read_json("https://fantasy.premierleague.com/drf/bootstrap-static", simplifyVector = TRUE)
  
  # extract player data ONLY, convert to tibble format
  data <- tibble::as.tibble(extract$elements)
  
  # replace codes with matching values
  data$team_name <- with(extract$teams, name[match(data$team_code, code)])
  data$position <- with(extract$element_types, singular_name[match(data$element_type, id)])
  data$status <- with(status, player_status[match(data$status, id)])
  
  # convert values to fpl-familiar style
  data$price <- data$now_cost/10
  data$price_change_abs <- data$cost_change_start/10
  data$price_change_round <- data$cost_change_event/10
  
  # convert var names
  data$transfers_out_round <- data$transfers_out_event
  data$transfers_in_round <- data$transfers_in_event
  data$round_points <- data$event_points
  
  # convert var types
  data$value_form <- as.numeric(data$value_form)
  data$value_season <- as.numeric(data$value_season)
  data$selected_by_percent <- as.numeric(data$selected_by_percent)
  data$form <- as.numeric(data$form)
  data$points_per_game <- as.numeric(data$points_per_game)
  data$ep_this <- as.numeric(data$ep_this)
  data$ep_next <- as.numeric(data$ep_next)
  data$influence <- as.numeric(data$influence)
  data$creativity <- as.numeric(data$creativity)
  data$threat <- as.numeric(data$threat)
  data$ict_index <- as.numeric(data$ict_index)
  
  # subset columns
  data <- subset(data, select = c(id, code, first_name, second_name, web_name, team_name, position, status, 
                                  news, price, price_change_abs, price_change_round, chance_of_playing_this_round, chance_of_playing_next_round, 
                                  value_form, value_season, in_dreamteam, dreamteam_count, selected_by_percent, form, transfers_out, transfers_in, 
                                  transfers_out_round, transfers_in_round, total_points, round_points, points_per_game, ep_this, ep_next, 
                                  minutes, goals_scored, assists, clean_sheets, goals_conceded, own_goals, penalties_saved, penalties_missed, 
                                  yellow_cards, red_cards, saves, bonus, bps, influence, creativity, threat, ict_index, ea_index))
  
  return(data)
  
}

playerDetailed <- function(player_id) {
  
  # make the input numeric
  player_id <- as.numeric(player_id)
  
  # get player list
  players <- jsonlite::read_json("https://fantasy.premierleague.com/drf/bootstrap-static", simplifyVector = TRUE)
  
  # check the input is in range, stop if not
  if (!player_id %in% 1:length(players$elements$id)) 
    stop("player_id out of range.")
  
  # read in json player data, simplify vectors to make easy transfer to dataframe
  data <- jsonlite::read_json(paste0("https://fantasy.premierleague.com/drf/element-summary/", player_id), simplifyVector = TRUE)
  
  # extract current seasons data ONLY, convert to tibble format
  data <- tibble::as.tibble(data$history)
  
  if (length(data) < 1) 
    stop("No player data for the current season, yet.")
  
  # replace codes with matching values
  data$opponent_team <- with(players$teams, name[match(data$opponent_team, id)])
  
  # convert values to fpl-familiar style
  data$price <- data$value/10
  
  # convert var types
  data$influence <- as.numeric(data$influence)
  data$creativity <- as.numeric(data$creativity)
  data$threat <- as.numeric(data$threat)
  data$ict_index <- as.numeric(data$ict_index)
  
  # append player id
  data$player_id <- data$element
  
  data <- subset(data, select = -c(element, value))
  
  return(data)
  
}

fpl <- players()

fpl.all <- lapply(sort(fpl$id), function(i) {
  print(paste(i/max(fpl$id)))
  return(playerDetailed(i))
})

# Save
fpl.all.2 <- do.call(rbind, fpl.all)
saveRDS(fpl.all.2, 'fpl_all.rds')

fpl.all.2 <- readRDS('fpl_all.rds')

# Try looking at top scorers by fixture
fpl.all.3 <- fpl.all.2 %>%
  filter(minutes > 0) %>% # Only those that played
  inner_join(dplyr::select(fpl, id, web_name, team_name, position), by= c("player_id"="id")) %>%
  group_by(player_id) %>%
  arrange(round) %>%
  mutate(bonus_lag = lag(bonus),
         bonus_lag = ifelse(is.na(bonus_lag), 0, bonus_lag)) %>%
  mutate(totalbonus = cumsum(bonus_lag)) %>%
  arrange(player_id, round) %>%
  group_by(fixture) %>%
  mutate(raw_points = total_points - bonus) %>%
  mutate(rank = dense_rank(desc(raw_points)), bonus2 = bonus) %>%
  mutate(rank2 = ifelse(rank > 3, 4, rank)) %>%
  mutate(less60 = as.factor(ifelse(minutes < 60, 1, 0))) %>%
  ungroup

# Plot
fpl.all.3 %>%
  ggplot(aes(x=rank2, y=bonus)) +
  geom_point(alpha = 0.2, colour = "dodgerblue4")

# Save
fpl.all.3 %>%
  dplyr::select(id, fixture, round, web_name, position, round, minutes, less60, goals_scored, assists, clean_sheets, totalbonus, raw_points, rank2, bonus) %>%
  saveRDS('bp_data.rds')

# ---------------

# load the library
library(mlbench)
library(caret)

# Get data
fpl.all.3 <- readRDS('bp_data.rds')

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the glm model
modelglm <- train(bonus ~ position*rank2 + position*assists + position*clean_sheets + totalbonus + less60,
                  data=filter(fpl.all.3, round != 32), method="glm", trControl=control)

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
test <- filter(fpl.all.3, round == 32)
test$xbp <- ifelse(predict(modelglm, newdata=test) < 0, 0, predict(modelglm, newdata=test))

# RMSE = 0.29. Very similar to training data (.3).
caret::RMSE(test$xbp,test$bonus)

# R2 = .83. Similar to training data.
caret::R2(test$xbp,test$bonus)

# Look at the predictions
test %>%
  arrange(fixture, desc(xbp)) %>%
  mutate(xbp = round(xbp, 2)) %>%
  View

# Does this ever disagree with the approach of just using their points rank within fixtures?
fpl.all.3$xbp <- ifelse(predict(modelglm, newdata=fpl.all.3) < 0, 0, predict(modelglm, newdata=fpl.all.3))
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

