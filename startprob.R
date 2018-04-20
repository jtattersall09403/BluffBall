# ------------------------------ Predicting probability of starting ------------------------------ #

library(dplyr)
library(reshape2)
library(TTR)
library(parallel)
library(fplr)
library(RcppRoll)
library(pROC)
library(purrr)

source('./GetFPLData.R')

# Get player and team data
data <- fpl %>%
  mutate(id = as.numeric(id))

# Geom series
geomSeries <- function(base, n) {
  base^(1:n)
}

# Get teams data
teams.2 = teams()
players.2 = players()

# Define function to get all detailed player data
details <- function(id) {
  tryCatch({
    # get player details
    x = playerDetailed2(id, plyrs) %>%
      inner_join(dplyr::select(players.2, id, web_name, position, team_name), by = c("player_id"="id")) %>%
      inner_join(dplyr::select(teams.2, name, strength_overall_home:strength_defence_away), by = c("team_name"="name")) %>%
      inner_join(dplyr::select(teams.2, name, strength_overall_home:strength_defence_away), by = c("opponent_team"="name")) %>%
      mutate(team_attack_strength = ifelse(was_home, strength_attack_home.x, strength_attack_away.x),
             opp_defence_strength = ifelse(was_home, strength_defence_away.y, strength_defence_home.y),
             strength_ratio = team_attack_strength/opp_defence_strength,
             position = as.factor(position)) %>%
      group_by(player_id) %>%
      arrange(round) %>%
      mutate(assists_act = assists,
             strength_ratio_act = strength_ratio)
    
    # Change time to time object
    x <- x %>%
      mutate(kickoff_time = as.Date(kickoff_time),
             minutes = as.integer(minutes),
             total_points = as.integer(total_points))
    
    # Set player id
    x$id = x$player_id
    
    # Binary transferred in/out
    x = x %>%
      mutate(trans_in = ifelse(transfers_balance < 0, 0, 1))
    
    # Convert to time series
    xt = xts::xts(x, order.by = x$kickoff_time)
    
    # Get weighted moving average of minutes and points
    n <- ifelse(nrow(x) < 5, nrow(x), 5)
    x$avmins <- WMA(x$minutes, n=n, w = geomSeries(1.3, n))
    x$form2 <- WMA(x$total_points, n=n, w = geomSeries(1.3, n))
    x <- x %>%
      mutate(assists = assists/strength_ratio,
             assists_5 = roll_mean(assists, n, align = "right", fill = NA),
             crosses_5 = roll_mean(open_play_crosses/strength_ratio, n, align = "right", fill = NA),
             bigchance_5 = roll_mean(big_chances_created/strength_ratio, n, align = "right", fill = NA),
             keypass_5 = roll_mean(key_passes/strength_ratio, n, align = "right", fill = NA),
             influence_5 = roll_mean(influence/strength_ratio, n, align = "right", fill = NA),
             creativity_5 = roll_mean(creativity/strength_ratio, n, align = "right", fill = NA),
             threat_5 = roll_mean(threat/strength_ratio, n, align = "right", fill = NA),
             ict_index_5 = roll_mean(ict_index/strength_ratio, n, align = "right", fill = NA))
    
    # Convert back to data frame
    x = as.data.frame(x)
    x = x %>%
      mutate(avmins_lag = lag(avmins, 1)) %>%
      mutate(assists_any = ifelse(assists_act > 0, 1, 0)) %>%
      dplyr::select(id, kickoff_time, minutes, trans_in, total_points, avmins, avmins_lag, form2,
                    round, fixture, web_name, position, team_name, opponent_team,
                    strength_ratio_act, team_attack_strength, opp_defence_strength, influence_5, creativity_5, threat_5, ict_index_5,
                    assists_5, crosses_5, bigchance_5, keypass_5, assists_any, assists_act)
    
    # Return data
    return(x)
  }, error = function(e) {
    return(NA)
  }, finally =  print(paste0(100*round(id/max(data$id),3), '% processed'))
  )
  
}

# Calculate the number of cores
# no_cores <- detectCores() - 1
# 
# # Initiate cluster
# cl <- makeCluster(no_cores)
# 
# # Include relevant variables and packages
# clusterEvalQ(cl, {
#    library(jsonlite)
#    library(dplyr)
#    library(ggplot2)
#    library(ggrepel)
#    library(engsoccerdata)
#    library(devtools)
#    library(fplr)
#    library(dplyr)
#    library(reshape2)
#    library(TTR)
#    library(RcppRoll)
#    geomSeries <- function(base, n) {
#      base^(1:n)
#    }
# })
#   
# clusterExport(cl, c("data", "players.2", "teams.2"))

# Get all data for modelling. Now only takes around 1 minute!
#modeldata <- parLapply(cl, sort(unique(data$id)), details)
plyrs <- jsonlite::read_json("https://fantasy.premierleague.com/drf/bootstrap-static", simplifyVector = TRUE)
modeldata <- lapply(sort(data$id), details, players=plyrs)

# Close cluster
# stopCluster(cl)

modeldata2 <- modeldata[!is.na(modeldata)]

# Prepare data for modelling. Create binary 60+ mins variable, and scale assist predictors within teams
modeldata3 <- do.call(rbind, modeldata2) %>%
  filter(!is.na(avmins)) %>%
  mutate(mins60 = ifelse(minutes >= 60, 1, 0),
         minsany = ifelse(minutes >0, 1, 0),
         trans_in = as.factor(trans_in)) %>%
  group_by(team_name, round) %>%
  mutate(av_threat = mean(threat_5, na.rm = TRUE),
         max_threat = max(threat_5, na.rm = TRUE)) %>%
  mutate_at(vars(influence_5, creativity_5, threat_5, ict_index_5,
                 assists_5, crosses_5, bigchance_5, keypass_5), function(x) as.numeric(scale(x)))

# View(arrange(modeldata3, team_name, round))

saveRDS(modeldata3, './Project files/startprob_modeldata.rds')

# ------------------------------ Train models ------------------------------  #

# Quick visualisation
# modeldata3 %>%
#   sample_n(size = 1000) %>%
#   ggplot(aes(x = avmins, y = mins60)) +
#   geom_point(color = 'dodgerblue4', alpha = 0.5)

# Create training and test data
n <- round(nrow(modeldata3)*2/3,0)
train<- modeldata3[1:n,]
test <- modeldata3[n+1:nrow(modeldata3),]

model <- glm(mins60 ~ avmins_lag + trans_in, family=binomial(link='logit'),data=train)

summary(model)

# Evaluate model
fitted.results <- predict(model,newdata=test,type='response')
test$pred <- fitted.results
test %>% select(minutes, pred) %>% View

fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$mins60, na.rm = T)
print(paste('Accuracy',1-misClasificError))

print(psych::cohen.kappa(table(fitted.results, test$mins60)))
print(roc(fitted.results, test$mins60))

# Save model
saveRDS(model, './startprob.rds')
# model <- readRDS('./startprob.rds')

# ------------------------------ Produce probability of starting next game ----------------------------- #

# Get most recent row for each player from detail table
modelresults <- modeldata3 %>%
  group_by(id) %>%
  mutate(rank = rank(desc(kickoff_time))) %>%
  filter(rank == 1) %>%
  mutate(avmins_lag = avmins) %>%
  select(id, avmins_lag, trans_in, form2) %>%
  ungroup

# Match weighted average minutes and form to fpl data, and rename variables
# Predict next starting probability. Set to zero if injured.
fpl.1 <- fpl %>%
  mutate(id = as.numeric(id)) %>%
  left_join(modelresults, by = 'id')

fpl.1$prob60 <- ifelse(fpl.1$status == 'a', predict(model, newdata = fpl.1, type = 'response'), 0)
fpl.1 <- fpl.1 %>%
  mutate(prob60 = ifelse(prob60 < .15, 0, prob60))

# ------------ Predict assists -------------
model2 <- readRDS('gbm_assist_model.RDS')

# Get most recent row for each player from detail table, and get next fixture team strengths
as_modeldata <- modeldata3 %>%
  group_by(id) %>%
  mutate(rank = rank(desc(kickoff_time))) %>%
  filter(rank == 1) %>%
  mutate(avmins_lag = avmins) %>%
  left_join(select(fix2,-fixture), by = "team_name") %>%
  inner_join(dplyr::select(teams.2, name, strength_overall_home:strength_defence_away), by = c("team_name"="name")) %>%
  inner_join(dplyr::select(teams.2, name, strength_overall_home:strength_defence_away), by = c("opponent_team.y"="name")) %>%
  mutate(team_attack_strength = ifelse(was_home, strength_attack_home.x, strength_attack_away.x),
         opp_defence_strength = ifelse(was_home, strength_defence_away.y, strength_defence_home.y),
         strength_ratio = team_attack_strength/opp_defence_strength,
         position = as.factor(position)) %>%
  select(round, fixture, order_fix, num_fix, id, web_name, position, team_name, opponent_team.y,
         strength_ratio_act=strength_ratio, team_attack_strength,opp_defence_strength, influence_5, creativity_5, threat_5, ict_index_5,
         assists_5, crosses_5, bigchance_5, keypass_5, av_threat, max_threat, assists_any, assists_act) %>%
  ungroup

# Prepare data
as_modeldata.2 <- as_modeldata[complete.cases(as_modeldata),] %>%
  filter(position != 'Goalkeeper')

# Get predictions
p_as <- predict(model2, newdata=as_modeldata.2, type = "prob")

as_modelresults <- cbind(as_modeldata.2, p_as) %>%
  mutate(xpas = ((OneAssist+TwoAssists+ThreeAssists)*3)) %>%
  mutate(probas = OneAssist+TwoAssists+ThreeAssists) %>%
  dplyr::select(id, team_attack_strength, opp_defence_strength, order_fix, num_fix, probas, xpas)

fpl.1 <- fpl.1 %>%
  left_join(as_modelresults, by = c('id','order_fix')) %>%
  mutate_at(vars(probas, xpas), function(x) ifelse(is.na(x), 0, 
                                                   ifelse(.$status != 'a', 0, x)))
