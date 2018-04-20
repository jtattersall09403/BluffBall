# ----------- XGBoost exploration ------------

library(xgboost)
library(caret)
library(dplyr)
library(RcppRoll)
library(zoo)
library(pROC)

source('./GetFPLData.R')
source('./getOdds.R')
source('./Dreamteam/Dreamteam - recursive v2.R')

# ------------ Get data ------------------

# Get fpl summary data
data <- getFPLSummary() %>%
  mutate(id = as.numeric(id))

# Get teams data
teams.2 = teams()
players.2 = players()
players <- jsonlite::read_json("https://fantasy.premierleague.com/drf/bootstrap-static", simplifyVector = TRUE)

# Define function to get all detailed player data
details <- function(id, players) {
  tryCatch({
    # get player details
    x = playerDetailed2(id, players) %>%
      inner_join(dplyr::select(players.2, id, web_name, position, team_name), by = c("player_id"="id")) %>%
      inner_join(dplyr::select(teams.2, name, strength_overall_home:strength_defence_away), by = c("team_name"="name")) %>%
      inner_join(dplyr::select(teams.2, name, strength_overall_home:strength_defence_away), by = c("opponent_team"="name")) %>%
      mutate(team_attack_strength = ifelse(was_home, strength_attack_home.x, strength_attack_away.x),
             team_defence_strength = ifelse(was_home, strength_defence_home.x, strength_defence_away.x), 
             opp_defence_strength = ifelse(was_home, strength_defence_away.y, strength_defence_home.y),
             opp_attack_strength = ifelse(was_home, strength_attack_away.y, strength_attack_home.y),
             strength_ratio_attdef = team_attack_strength/opp_defence_strength,
             strength_ratio_defatt = team_defence_strength/opp_attack_strength,
             position = as.factor(position))
    
    # Get form variables for last 5
    n <- 5
    x <- x %>%
      mutate(goals_5 = rollapplyr(goals_scored/strength_ratio_attdef, n, mean, partial = TRUE, fill = NA),
             assists_5 = rollapplyr(assists/strength_ratio_attdef, n, mean, partial = TRUE, fill = NA),
             crosses_5 = rollapplyr(open_play_crosses/strength_ratio_attdef, n, mean, partial = TRUE, fill = NA),
             bigchance_5 = rollapplyr(big_chances_created/strength_ratio_attdef, n, mean, partial = TRUE, fill = NA),
             keypass_5 = rollapplyr(key_passes/strength_ratio_attdef, n, mean, partial = TRUE, fill = NA),
             creativity_5 = rollapplyr(creativity/strength_ratio_attdef, n, mean, partial = TRUE, fill = NA),
             threat_5 = rollapplyr(threat/strength_ratio_attdef, n, mean, partial = TRUE, fill = NA),
             mins_5 = rollapplyr(minutes, n, mean, partial = TRUE, fill = NA))
    
    # Convert to data frame
    x = as.data.frame(x)
    
    # Return data
    return(x)
  }, error = function(e) {
    return(NA)
  }, finally =  print(paste0(100*round(id/max(data$id),3), '% processed'))
  )
  
}

# Download and combine
modeldata <- lapply(sort(data$id), details, players=players)

# Manipulation
modeldata2 <- do.call(rbind, modeldata[!is.na(modeldata)])

# Prepare data for modelling.
# Create 3 class appearance variable, and scale demand within teams and positions,

modeldata3 <- modeldata2 %>%
  mutate(mins_cat = case_when(minutes < 60 ~ 0,
                              minutes >= 60 ~ 1),
         assists_any = ifelse(assists > 0, 1, 0)) %>%
  group_by(team_name, round) %>%
  mutate(selected_scale = scale(selected),
         trans_scale = scale(transfers_balance),
         av_threat = mean(threat_5, na.rm = TRUE),
         max_threat = max(threat_5, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(trans_scale = ifelse(is.na(trans_scale), 0, trans_scale)) %>%
  mutate(position = as.factor(position)) %>%
  mutate(was_home = as.logical(was_home))
 
# Save
saveRDS(modeldata3, './Project files/xgboost data.rds')

# ----------- prob60 model ---------------

# Get data
modeldata3 <- readRDS('./Project files/xgboost data.rds')

# Set round
round = 34

# Filter to rounds in the past, scale form variables
modeldata4 <- modeldata3 %>%
  filter(round <= round) %>%
  mutate_at(vars(creativity_5, threat_5,
                 assists_5, crosses_5, bigchance_5, keypass_5), function(x) as.numeric(scale(x))) %>%
  group_by(team_name) %>%
  mutate(mins_5 = scale(mins_5)) %>%
  mutate(mins_5 = lag(mins_5)) %>%
  mutate(mins_5 = ifelse(is.na(mins_5), 0, mins_5))

# Get training and test index
trainIndex <- createDataPartition(modeldata4$mins_cat,
                                  p = 0.7,
                                  list = FALSE,
                                  times = 1)

# Create xbg matrix
data_variables <- modeldata4[, c('selected_scale', 'trans_scale', 'mins_5')]
data_label <- modeldata4$mins_cat
data_matrix <- xgb.DMatrix(data = as.matrix(data_variables),
                           label = data_label)

# split train data and make xgb.DMatrix
train_data   <- data_variables[trainIndex,]
train_label  <- data_label[trainIndex]
train_matrix <- xgb.DMatrix(data = as.matrix(train_data), label = train_label)

# split test data and make xgb.DMatrix
test_data  <- data_variables[-trainIndex,]
test_label <- data_label[-trainIndex]
test_matrix <- xgb.DMatrix(data = as.matrix(test_data), label = test_label)

# Set parameters
numberOfClasses <- length(unique(modeldata4$mins_cat))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)

nround    <- 50 # number of XGBoost rounds
cv.nfold  <- 5

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = TRUE,
                   prediction = TRUE)

# Results
OOF_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = train_label + 1)
head(OOF_prediction)

# confusion matrix
confusionMatrix(factor(OOF_prediction$max_prob),
                factor(OOF_prediction$label),
                mode = "everything")

# Train model on full training set for testing against holdout set
bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = nround,
                       verbose = TRUE)

# Predict hold-out test set
test_pred <- predict(bst_model, newdata = test_matrix)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label + 1,
         max_prob = max.col(., "last"))

# confusion matrix of test set
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")

#  ---- Run on full dataset and save model ----
full_matrix <-  xgb.DMatrix(data = as.matrix(data_variables), label = data_label)
bst_model.2 <- xgb.train(params = xgb_params,
                       data = full_matrix,
                       nrounds = nround)

# Create predictions
test_pred <- predict(bst_model.2, newdata = full_matrix)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame()

modeldata5 <- modeldata4 %>%
  ungroup %>%
  cbind(test_prediction) %>%
  as.data.frame %>%
  mutate(prob0=round(X1, 3),
         prob60=round(X2, 3)) %>%
  select(-X1, -X2)

# Check ROC. Improvement on logistic regression: .9367
roc(modeldata5$mins_cat, modeldata5$prob60)

modeldata5 %>%
  filter(team_name=="Liverpool", round == 33) %>%
  arrange(desc(prob60)) %>% View

# get the feature real names
names <-  colnames(data_variables)

# compute feature importance matrix
importance_matrix = xgb.importance(feature_names = names, model = bst_model)
head(importance_matrix)

# plot
gp = xgb.ggplot.importance(importance_matrix)
print(gp) 

# Save
saveRDS(bst_model.2, './Project files/prob60_xbg.rds')

# ------------- Goal model -------------
# Get training data by gameweek, scale form variables
train <- modeldata %>%
  filter(round < gw) %>%
  #mutate_at(vars(creativity_5, threat_5,
  #               assists_5, crosses_5, bigchance_5, keypass_5), function(x) as.numeric(scale(x))) %>%
  group_by(player_id) %>%
  arrange(round) %>%
  mutate(goals90 = 90*cumsum(goals_scored)/cumsum(minutes)) %>%
  mutate(goals90 = ifelse(is.na(goals90), 0, goals90)) %>%
  ungroup

# Lag predictors
train2 <- train %>%
  mutate(assists_any_act = assists_any) %>%
  group_by(player_id) %>%
  arrange(round) %>%
  mutate_at(vars(goals_5, assists_5:assists_any, av_threat, max_threat:goals90), lag) %>%
  filter(!is.na(mins_5)) %>%
  ungroup %>%
  arrange(goals_scored)

# Get prob60 predictions
# Create predictions
prob60 <- predict(prob60model, newdata = as.matrix(select(train2, mins_5, selected_scale, trans_scale)))
p60_pred <- matrix(prob60, nrow = 2,
                   ncol=length(prob60)/2) %>%
  t() %>%
  data.frame()

train2$prob60 <- p60_pred$X2


# -------------- Goal model ----------------

prob60model <- readRDS('./Project files/prob60_xbg.rds')

# Data prep
modeldata <- modeldata3 %>%
  group_by(team_name) %>%
  mutate(mins_5 = scale(mins_5)) %>%
  group_by(team_name) %>%
  mutate(team_create_5 = sum(creativity_5),
         team_chances_5 = sum(bigchance_5),
         team_keypass_5 = sum(keypass_5))
  
# Get training data by gameweek, scale form variables
train <- modeldata %>%
  #mutate_at(vars(creativity_5, threat_5,
  #               assists_5, crosses_5, bigchance_5, keypass_5), function(x) as.numeric(scale(x))) %>%
  group_by(player_id) %>%
  arrange(round) %>%
  mutate(goals90 = 90*cumsum(goals_scored)/cumsum(minutes)) %>%
  mutate(goals90 = ifelse(is.na(goals90), 0, goals90)) %>%
  ungroup

# Lag predictors
train2 <- train %>%
  mutate(assists_any_act = assists_any) %>%
  group_by(player_id) %>%
  arrange(round) %>%
  mutate_at(vars(goals_5, assists_5:assists_any, av_threat, max_threat:goals90), lag) %>%
  filter(!is.na(mins_5)) %>%
  ungroup %>%
  arrange(goals_scored)

# Get prob60 predictions
# Create predictions
prob60 <- predict(prob60model, newdata = as.matrix(select(train2, mins_5, selected_scale, trans_scale)))
p60_pred <- matrix(prob60, nrow = 2,
                   ncol=length(prob60)/2) %>%
  t() %>%
  data.frame()

train2$prob60 <- p60_pred$X2
  
# Create xbg matrix
data_variables <- train2[, c('prob60',
                             'trans_scale',
                             'threat_5',
                             'goals_5',
                             'goals90',
                             'was_home',
                             'strength_ratio_attdef',
                             'team_attack_strength',
                             'opp_defence_strength',
                             'team_keypass_5',
                             'team_chances_5',
                             'team_create_5')] %>%
  cbind(select(as.data.frame(model.matrix(~position, data=train2)), -1))

# Cross validate by gameweek
cv_gm <- lapply(3:33, function(gw) {
  print(paste("Gameweek", gw))
  
  # Convert to matrix
  data_label <- train2$goals_scored[train2$round != gw]
  data_matrix <- xgb.DMatrix(data = as.matrix(data_variables[train2$round != gw,]),
                             label = data_label)
  
  # Set parameters
  xgb_params <- list("objective" = "multi:softprob",
                     "eval_metric" = "mlogloss",
                     "num_class" = length(unique(data_label)))
  
  nround    <- 50 # number of XGBoost rounds
  
  # Train model
  goalmodel <- xgb.train(params = xgb_params,
                         data = data_matrix,
                         nrounds = nround)
  
  # Out of sample results
  test_label <- train2$goals_scored[train2$round == gw]
  test_matrix <- xgb.DMatrix(data = as.matrix(data_variables[train2$round == gw,]),
                             label = test_label)
  outsamp_goal <- predict(goalmodel, newdata = test_matrix)
  outsamp_goalpred <- matrix(outsamp_goal, nrow = length(unique(data_label)),
                            ncol=length(outsamp_goal)/length(unique(data_label))) %>%
    t() %>%
    data.frame() %>%
    mutate(label = test_label + 1,
           max_prob = max.col(., "last"))
  
  act_labs <- model.matrix(~goals_scored -1,
                           data=select(filter(mutate(train2,
                                                     goals_scored = as.factor(goals_scored)),
                                              round == gw),
                                       goals_scored)) %>%
    as.data.frame()
  
  # AUCs
  roc <- sapply(unique(test_label)+1, function(i) roc(act_labs[,i], outsamp_goalpred[,i])$auc)
  
  return(data.frame("AUC"=mean(roc), "AUC.sd"=sd(roc)))
  
})

# Diagnostics
cv_gw <- do.call(rbind, cv_gm)

# AUC fairly stable across gameweeks. All around.8
cv_gw %>%
  ggplot(aes(x=3:33, y=AUC)) +
  geom_point() +
  expand_limits(y=c(0,1)) +
  geom_smooth(span=0.2, se=FALSE)

# Run model on all data and save
# Convert to matrix
data_label <- train2$goals_scored[train2$round != 34]
data_matrix <- xgb.DMatrix(data = as.matrix(data_variables[train2$round != 34,]),
                           label = data_label)

# Set parameters
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = length(unique(data_label)))

nround    <- 50 # number of XGBoost rounds

# Train model
goalmodel <- xgb.train(params = xgb_params,
                       data = data_matrix,
                       nrounds = nround)

# Save
saveRDS(goalmodel, './Project files/goalmodel_xgb.rds')

# -------------- assist model ----------------

prob60model <- readRDS('./Project files/prob60_xbg.rds')
goalmodel <- readRDS('./Project files/goalmodel_xgb.rds')

# Data prep
modeldata <- modeldata3 %>%
  group_by(team_name) %>%
  mutate(mins_5 = scale(mins_5)) %>%
  group_by(team_name) %>%
  mutate(team_create_5 = sum(creativity_5),
         team_chances_5 = sum(bigchance_5),
         team_keypass_5 = sum(keypass_5))

# Get training data by gameweek, scale form variables
train <- modeldata %>%
  #mutate_at(vars(creativity_5, threat_5,
  #               assists_5, crosses_5, bigchance_5, keypass_5), function(x) as.numeric(scale(x))) %>%
  group_by(player_id) %>%
  arrange(round) %>%
  mutate(goals90 = 90*cumsum(goals_scored)/cumsum(minutes)) %>%
  mutate(goals90 = ifelse(is.na(goals90), 0, goals90)) %>%
  group_by(player_id) %>%
  arrange(round) %>%
  mutate(as90 = 90*cumsum(assists)/cumsum(minutes)) %>%
  mutate(as90 = ifelse(is.na(as90), 0, as90)) %>%
  ungroup
  ungroup

# Lag predictors
train2 <- train %>%
  mutate(assists_any_act = assists_any) %>%
  group_by(player_id) %>%
  arrange(round) %>%
  mutate_at(vars(goals_5, assists_5:assists_any, av_threat, max_threat:as90), lag) %>%
  filter(!is.na(mins_5)) %>%
  ungroup %>%
  arrange(goals_scored)

# Get prob60 predictions
# Create predictions
prob60 <- predict(prob60model, newdata = as.matrix(select(train2, mins_5, selected_scale, trans_scale)))
p60_pred <- matrix(prob60, nrow = 2,
                   ncol=length(prob60)/2) %>%
  t() %>%
  data.frame()

train2$prob60 <- p60_pred$X2

# Get goal predictions
# Create predictions
goalprobs <- predict(goalmodel, newdata = as.matrix(train2[, c('prob60',
                                                               'trans_scale',
                                                               'threat_5',
                                                               'goals_5',
                                                               'goals90',
                                                               'was_home',
                                                               'strength_ratio_attdef',
                                                               'team_attack_strength',
                                                               'opp_defence_strength',
                                                               'team_keypass_5',
                                                               'team_chances_5',
                                                               'team_create_5')]))
goalprobs <- matrix(goalprobs, nrow = 5,
                   ncol=length(goalprobs)/5) %>%
  t() %>%
  data.frame()

# Bind predictions
train2 <- cbind(train2, goalprobs) %>%
  as.data.frame %>%
  mutate(goal0=round(X1, 3),
         goal1=round(X2, 3),
         goal2=round(X3, 3),
         goal3=round(X4, 3),
         goal4=round(X5, 3)) %>%
  mutate(xg = goal1*1 + goal2*2 + goal3*3 + goal4*4) %>%
  select(-X1, -X2, -X3, -X4, -X5) %>%
  group_by(team_name, round) %>%
  mutate(team_xg = sum(xg, na.rm = T))

# Create xbg matrix
data_variables <- train2[, c('prob60',
                             'trans_scale',
                             'assists_5',
                             'as90',
                             'team_xg',
                             'was_home',
                             'strength_ratio_attdef',
                             'team_attack_strength',
                             'opp_defence_strength',
                             'crosses_5',
                             'bigchance_5',
                             'keypass_5',
                             'creativity_5')] %>%
  cbind(select(as.data.frame(model.matrix(~position, data=train2)), -1))

# Cross validate by gameweek
cv_gm <- lapply(3:33, function(gw) {
  print(paste("Gameweek", gw))
  
  # Convert to matrix
  data_label <- train2$assists[train2$round != gw]
  data_matrix <- xgb.DMatrix(data = as.matrix(data_variables[train2$round != gw,]),
                             label = data_label)
  
  # Set parameters
  xgb_params <- list("objective" = "multi:softprob",
                     "eval_metric" = "mlogloss",
                     "num_class" = length(unique(data_label)))
  
  nround    <- 50 # number of XGBoost rounds
  
  # Train model
  ass_model <- xgb.train(params = xgb_params,
                         data = data_matrix,
                         nrounds = nround)
  
  # Out of sample results
  test_label <- train2$assists[train2$round == gw]
  test_matrix <- xgb.DMatrix(data = as.matrix(data_variables[train2$round == gw,]),
                             label = test_label)
  outsamp_ass <- predict(ass_model, newdata = test_matrix)
  outsamp_ass_pred <- matrix(outsamp_ass, nrow = length(unique(data_label)),
                             ncol=length(outsamp_ass)/length(unique(data_label))) %>%
    t() %>%
    data.frame() %>%
    mutate(label = test_label + 1,
           max_prob = max.col(., "last"))
  
  act_labs <- model.matrix(~assists -1,
                           data=select(filter(mutate(ungroup(train2),
                                                     assists = as.factor(assists)),
                                              round == gw),
                                       assists)) %>%
    as.data.frame()
  
  # AUCs
  roc <- sapply(unique(test_label)+1, function(i) roc(act_labs[,i], outsamp_ass_pred[,i])$auc)
  
  return(data.frame("AUC"=mean(roc), "AUC.sd"=sd(roc)))
  
})

# Diagnostics
cv_gw <- do.call(rbind, cv_gm)

# AUC fairly stable across gameweeks. All around.8
cv_gw %>%
  ggplot(aes(x=3:33, y=AUC)) +
  geom_point() +
  expand_limits(y=c(0,1)) +
  geom_smooth(span=0.2, se=FALSE)

mean(cv_gw$AUC)
cv_gw

# Run model on all data and save
# Convert to matrix
data_label <- train2$assists[train2$round != 34]
data_matrix <- xgb.DMatrix(data = as.matrix(data_variables[train2$round != 34,]),
                           label = data_label)

# Set parameters
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = length(unique(data_label)))

nround    <- 50 # number of XGBoost rounds

# Train model
ass_model <- xgb.train(params = xgb_params,
                       data = data_matrix,
                       nrounds = nround)

# Save
saveRDS(ass_model, './Project files/assistmodel_xgb.rds')

# ------------- Full model -------------

gw = 8

modeldata3 <- readRDS('./Project files/xgboost data.rds')

# Data prep
modeldata <- modeldata3 %>%
  group_by(team_name) %>%
  mutate(mins_5 = scale(mins_5)) %>%
  group_by(team_name) %>%
  mutate(team_create_5 = sum(creativity_5),
         team_chances_5 = sum(bigchance_5),
         team_keypass_5 = sum(keypass_5))

# Get training data by gameweek, scale form variables
train <- modeldata %>%
  group_by(player_id) %>%
  arrange(round) %>%
  mutate(goals90 = 90*cumsum(goals_scored)/cumsum(minutes)) %>%
  mutate(goals90 = ifelse(is.na(goals90), 0, goals90)) %>%
  group_by(player_id) %>%
  arrange(round) %>%
  mutate(as90 = 90*cumsum(assists)/cumsum(minutes)) %>%
  mutate(as90 = ifelse(is.na(as90), 0, as90)) %>%
  ungroup

# Lag predictors
train1 <- train %>%
  mutate(assists_any_act = assists_any) %>%
  group_by(player_id) %>%
  arrange(round) %>%
  mutate_at(vars(goals_5, assists_5:mins_5, av_threat, max_threat:as90), lag) %>%
  filter(!is.na(mins_5)) %>%
  ungroup %>%
  arrange(goals_scored)


# Cross validate by gameweek. Set up function
gw_roc <- function(gw) {
  print(paste("Gameweek", gw))
  
  # Remove hold-out set
  train2 <- train1 %>% filter(round < 34)
  
  # ---------- Prob60 ------------
  # Create xbg matrix
  data_variables <- train2[, c('mins_5',
                               'trans_scale',
                               'selected_scale')]
  
  # Convert to matrix
  traindata <- data_variables[train2$round != gw,]
  data_label <- train2$mins_cat[train2$round != gw]
  data_matrix <- xgb.DMatrix(data = as.matrix(traindata),
                             label = data_label)
  
  # Set parameters
  xgb_params <- list("objective" = "binary:logistic",
                     "eval_metric" = "logloss")
  
  nround    <- 50 # number of XGBoost rounds
  
  # Train model
  prob60model <- xgb.train(params = xgb_params,
                         data = data_matrix,
                         nrounds = nround)
  
  # Out of sample results
  test_label <- train2$mins_cat[train2$round == gw]
  test_matrix <- xgb.DMatrix(data = as.matrix(data_variables[train2$round == gw,]),
                             label = test_label)
  p60 <- predict(prob60model, newdata = test_matrix)
  p60_pred <- data.frame(X1=p60) %>%
    mutate(label = test_label + 1)
  
  # AUCs
  p60roc <- roc(test_label, p60_pred$X1)$auc
  #p60loss <- logLoss(test_label, p60_pred$X1)
  
  print(paste("Appearance probability time modelled"))
  
  # # ---------- xmins ------------
  # # Get prob60 predictions
  # # Create predictions
  # prob60 <- predict(prob60model, newdata = as.matrix(select(train2, mins_5, selected_scale, trans_scale)))
  # train2$prob60 <- prob60
  # 
  # # Create xbg matrix
  # data_variables <- train2[, c('mins_5',
  #                              'trans_scale',
  #                              'selected_scale')]
  # 
  # # Convert to matrix
  # traindata <- data_variables[train2$round != gw,]
  # data_label <- train2$minutes[train2$round != gw]
  # data_matrix <- xgb.DMatrix(data = as.matrix(traindata),
  #                            label = data_label)
  # 
  # # Set parameters
  # xgb_params <- list(objective = "reg:linear", 
  #                    eval_metric = "rmse")
  # 
  # nround    <- 50 # number of XGBoost rounds
  # 
  # # Train model
  # xminmodel <- xgb.train(params = xgb_params,
  #                          data = data_matrix,
  #                          nrounds = nround)
  # 
  # # Out of sample results
  # test_label <- train2$minutes[train2$round == gw]
  # test_matrix <- xgb.DMatrix(data = as.matrix(data_variables[train2$round == gw,]),
  #                            label = test_label)
  # xmin <- predict(xminmodel, newdata = test_matrix)
  # 
  # # Diagnostics
  # xmin.rmse <- RMSE(test_label, xmin)
  # xmin.r2 <- R2(test_label, xmin)
  # 
  # print(paste("Playing time modelled"))
  
  # --------- Goals --------------
  # Add xmin prediction
  # train2$xmin <- predict(xminmodel, newdata = as.matrix(select(train2, mins_5, selected_scale, trans_scale)))
  
  # Add prob60 predictions
  prob60 <- predict(prob60model, newdata = as.matrix(select(train2, mins_5, selected_scale, trans_scale)))
  train2$prob60 <- prob60
  
  # Create xbg matrix
  data_variables <- train2[, c('prob60',
                               'trans_scale',
                               'threat_5',
                               'goals_5',
                               'goals90',
                               'was_home',
                               'strength_ratio_attdef',
                               'team_attack_strength',
                               'opp_defence_strength',
                               'team_keypass_5',
                               'team_chances_5',
                               'team_create_5')] %>%
    cbind(select(as.data.frame(model.matrix(~position, data=train2)), -1))
  
  # Convert to matrix
  data_label <- train2$goals_scored[train2$round != gw]
  data_matrix <- xgb.DMatrix(data = as.matrix(data_variables[train2$round != gw,]),
                             label = data_label)
  
  # Set parameters
  xgb_params <- list("objective" = "multi:softprob",
                     "eval_metric" = "mlogloss",
                     "num_class" = length(unique(data_label)))
  
  nround    <- 50 # number of XGBoost rounds
  
  # Train model
  goalmodel <- xgb.train(params = xgb_params,
                         data = data_matrix,
                         nrounds = nround)
  
  # Out of sample results
  test_label <- train2$goals_scored[train2$round == gw]
  test_matrix <- xgb.DMatrix(data = as.matrix(data_variables[train2$round == gw,]),
                             label = test_label)
  outsamp_goal <- predict(goalmodel, newdata = test_matrix)
  outsamp_goalpred <- matrix(outsamp_goal, nrow = length(unique(data_label)),
                             ncol=length(outsamp_goal)/length(unique(data_label))) %>%
    t() %>%
    data.frame() %>%
    mutate(label = test_label + 1,
           max_prob = max.col(., "last"))
  
  act_labs <- model.matrix(~goals_scored -1,
                           data=select(filter(mutate(train2,
                                                     goals_scored = as.factor(goals_scored)),
                                              round == gw),
                                       goals_scored)) %>%
    as.data.frame()
  
  # AUCs
  goalroc <- sapply((unique(test_label)+1)[2:length(unique(test_label))], function(i) roc(act_labs[,i], outsamp_goalpred[,i])$auc)
  #goal_loss <- mlogLoss(test_label, as.matrix(outsamp_goalpred[,1:5]))

  
  print(paste("Goals modelled"))
  
  # ------- Assists --------
  
  # Get goal predictions
  # Create predictions
  goalprobs <- predict(goalmodel, newdata = as.matrix(train2[, c('prob60',
                                                                 'trans_scale',
                                                                 'threat_5',
                                                                 'goals_5',
                                                                 'goals90',
                                                                 'was_home',
                                                                 'strength_ratio_attdef',
                                                                 'team_attack_strength',
                                                                 'opp_defence_strength',
                                                                 'team_keypass_5',
                                                                 'team_chances_5',
                                                                 'team_create_5')]))
    
  goalprobs <- matrix(goalprobs, nrow = 5,
                      ncol=length(goalprobs)/5) %>%
    t() %>%
    data.frame()
  
  # Bind predictions
  train2 <- cbind(train2, goalprobs) %>%
    as.data.frame %>%
    mutate(goal0=round(X1, 3),
           goal1=round(X2, 3),
           goal2=round(X3, 3),
           goal3=round(X4, 3),
           goal4=round(X5, 3)) %>%
    mutate(xg = goal1*1 + goal2*2 + goal3*3 + goal4*4) %>%
    select(-X1, -X2, -X3, -X4, -X5) %>%
    group_by(team_name, round) %>%
    mutate(team_xg = sum(xg, na.rm = T))
  
  # Create xbg matrix
  data_variables <- train2[, c('prob60',
                               'trans_scale',
                               'assists_5',
                               'as90',
                               'team_xg',
                               'was_home',
                               'strength_ratio_attdef',
                               'team_attack_strength',
                               'opp_defence_strength',
                               'crosses_5',
                               'bigchance_5',
                               'keypass_5',
                               'creativity_5')] %>%
    cbind(select(as.data.frame(model.matrix(~position, data=train2)), -1))
  
  # Convert to matrix
  data_label <- train2$assists[train2$round != gw]
  data_matrix <- xgb.DMatrix(data = as.matrix(data_variables[train2$round != gw,]),
                             label = data_label)
  
  # Set parameters
  xgb_params <- list("objective" = "multi:softprob",
                     "eval_metric" = "mlogloss",
                     "num_class" = length(unique(data_label)))
  
  nround    <- 50 # number of XGBoost rounds
  
  # Train model
  ass_model <- xgb.train(params = xgb_params,
                         data = data_matrix,
                         nrounds = nround)
  
  # Out of sample results
  test_label <- train2$assists[train2$round == gw]
  test_matrix <- xgb.DMatrix(data = as.matrix(data_variables[train2$round == gw,]),
                             label = test_label)
  outsamp_ass <- predict(ass_model, newdata = test_matrix)
  outsamp_ass_pred <- matrix(outsamp_ass, nrow = length(unique(data_label)),
                             ncol=length(outsamp_ass)/length(unique(data_label))) %>%
    t() %>%
    data.frame() %>%
    mutate(label = test_label + 1,
           max_prob = max.col(., "last"))
  
  act_labs <- model.matrix(~assists -1,
                           data=select(filter(mutate(ungroup(train2),
                                                     assists = as.factor(assists)),
                                              round == gw),
                                       assists)) %>%
    as.data.frame()
  
  # AUCs
  assroc <- sapply((unique(test_label)+1)[2:length(unique(test_label))],
                   function(i) roc(act_labs[,i], outsamp_ass_pred[,i])$auc)
  
  # Full results
  test_label <- train2$assists
  test_matrix <- xgb.DMatrix(data = as.matrix(data_variables),
                             label = test_label)
  outsamp_ass <- predict(ass_model, newdata = test_matrix)
  outsamp_ass_pred <- matrix(outsamp_ass, nrow = length(unique(data_label)),
                             ncol=length(outsamp_ass)/length(unique(data_label))) %>%
    t() %>%
    data.frame() %>%
    mutate(label = test_label + 1,
           max_prob = max.col(., "last"))
  
  #mlogLoss(as.factor(test_label), outsamp_ass_pred[,1:4])
  
  print("Assists modelled")
  
  # ------------- Clean sheets -----------
  # Get opponent's chance of scoring
  teamgoalprobs <- train2 %>%
    group_by(team_name, round) %>%
    summarise(team_goalprob = 1-prod(goal0)) %>%
    select(opponent_team=team_name, round, opp_goalprob=team_goalprob)
  
  # Join to main data
  train2 <- train2 %>%
    inner_join(teamgoalprobs, by=c("opponent_team","round")) %>%
    group_by(team_name, round) %>%
    mutate(team_clean_sheets=max(clean_sheets))
  
  # Team's clean sheet chance = 1-opposition goal chance
  teamcs <- train2 %>%
    group_by(team_name, round) %>%
    summarise(team_goals_conceded = max(goals_conceded),
              team_probcs = mean(1-opp_goalprob)) %>%
    ungroup %>%
    mutate(team_cs = ifelse(team_goals_conceded > 0, 1, 0)) %>%
    select(team_name, round, team_cs, team_goals_conceded, team_probcs) %>%
    unique
  
  # Overall auc
  roc(teamcs$team_cs, teamcs$team_probcs)
  
  # Test set auc
  testcs <- teamcs %>% filter(round==gw)
  roc(testcs$team_cs, testcs$team_probcs)
  
  # Create player-level prediction
  cs <- train2 %>%
    inner_join(select(teamcs, team_name, round, team_probcs),
               by = c("team_name","round")) %>%
    mutate(cs = prob60*team_probcs) %>%
    ungroup %>%
    select(cs) %>%
    unlist
  train2$probcs <- cs
  
  # AUCs
  p60roc <- roc(train2$clean_sheets[train2$round==gw],
                train2$probcs[train2$round==gw])$auc
  
  print(paste("Clean sheets modelled"))
  
  # ------------- Expected points -----------------
  # Bind assist predictions to data
  train2$assist1 <- outsamp_ass_pred$X2
  train2$assist2 <- outsamp_ass_pred$X3
  train2$assist3 <- outsamp_ass_pred$X4
  train2$rawpoints <- train2$total_points - train2$bonus
  
  # Create xbg matrix
  data_variables <- train2[, c('prob60',
                               'probcs',
                               'goal1',
                               'goal2',
                               'goal3',
                               'goal4',
                               'assist1',
                               'assist2',
                               'assist3')] %>%
    cbind(select(as.data.frame(model.matrix(~position, data=train2)), -1))

  # Convert to matrix
  traindata <- data_variables[train2$round != gw,]
  data_label <- train2$rawpoints[train2$round != gw]
  data_matrix <- xgb.DMatrix(data = as.matrix(traindata),
                             label = data_label)

  # Set parameters
  xgb_params <- list(objective = "reg:linear",
                     eval_metric = "rmse")

  nround    <- 50 # number of XGBoost rounds

  # Train model
  xpmodel <- xgb.train(params = xgb_params,
                           data = data_matrix,
                           nrounds = nround)

  # Out of sample results
  test_label <- train2$rawpoints[train2$round == gw]
  test_matrix <- xgb.DMatrix(data = as.matrix(data_variables[train2$round == gw,]),
                             label = test_label)
  xp <- predict(xpmodel, newdata = test_matrix)

  # Diagnostics
  RMSE(test_label, xp)
  R2(test_label, xp)

  print(paste("Expected points modelled"))
  
  # ------------- Return predictions --------------
  test <- train2 %>% filter(round==gw)
  test$xp <- xp
  #View(select(test, team_name, web_name, opponent_team, rawpoints, xp))

  
  return(test)
  
}

# Get cv results
cv_gm <- lapply(2:33, gw_roc)
cv_gw <- do.call(rbind, cv_gm)

# Save
saveRDS(cv_gw, './Project files/xgb_fullmodel_cv_gw.rds')

# ------------ Model performance -------------

cv_gw <- readRDS('./Project files/xgb_fullmodel_cv_gw.rds')

# Match on points lookup
points <- data.frame('position' = sort(unique(cv_gw$position)),
                     'goal' = c(6,6,5,4),
                     'cleansheet' = c(4,4,1,0))

# Calculate xp
# cv_gw.2 <- cv_gw %>%
#   inner_join(points, by = "position") %>%
#   mutate(xpcs = probcs * cleansheet,
#          xpg = goal1*goal + goal2*2*goal + goal3*3*goal,
#          xpas = 3*(assist1 + assist2 + assist3),
#          xpap = prob60 * 2,
#          xp = xpcs + xpg + xpas + xpap) %>%
#   mutate(goalprob = goal1 + goal2 + goal3 + goal4,
#          probas = assist1 + assist2 + assist3,
#          rawpoints = total_points - bonus)

cv_gw.2 <- cv_gw %>%
  mutate(goalprob = goal1 + goal2 + goal3 + goal4,
         probas = assist1 + assist2 + assist3)

# For easy viewing
fpl.xgb.cv <- cv_gw.2 %>%
  ungroup %>%
  filter(minutes > 0) %>%
  select(round, team_name, web_name, position, opponent_team,
         minutes, clean_sheets, assists, goals_scored,
         rawpoints, goal1, goal2, goal3, probas, probcs, xp)

View(fpl.xgb.cv)
View(filter(fpl.xgb.cv, round==33))

# -------------- Diagnostics -------------

RMSE(cv_gw.2$rawpoints, cv_gw.2$xp)
R2(cv_gw.2$rawpoints, cv_gw.2$xp)

results <- lapply(c(2:20, 23:33), function(gw) {
  fpl <- cv_gw.2 %>% filter(round==gw) %>% ungroup %>%
    select(element=player_id, total_points, goalprob)
  
  dt <- cv_gw.2 %>%
    ungroup %>%
    filter(round == gw) %>%
    select(id=player_id,
           pos=position,
           team=team_name,
           web_name,
           now_cost=price,
           xp) %>%
    dreamteam %>%
    inner_join(fpl, by = "element") %>%
    #mutate(captain = ifelse(row_number(desc(goalprob))==1, 1, 0)) %>%
    mutate(total_points = ifelse(captain==1, total_points*2, total_points))
  
  # sum(dt$now_cost)
  # sum(dt$xp)
  # sum(dt$total_points)
  
  dt_other <- cv_gw.2 %>%
    ungroup %>%
    mutate(demand=as.numeric(scale(transfers_balance))) %>%
    as.data.frame %>%
    ungroup %>%
    filter(round == gw) %>%
    select(id=player_id,
           pos=position,
           team=team_name,
           web_name,
           now_cost=price,
           xp=demand) %>%
    dreamteam %>%
    inner_join(fpl, by = "element") %>%
    mutate(total_points = ifelse(captain==1, total_points*2, total_points))
  
  # Human team
  # dt_other
  # sum(dt_other$now_cost)
  # sum(dt_other$total_points)
  
  print(paste("Gameweek", gw))
  
  # Return difference
  return(data.frame(sum(dt$total_points[1:11]), sum(dt_other$total_points[1:11])))
})

xgb.res <- do.call(rbind, results)
xgb.res
names(xgb.res) <- c("Algorithm","Human")

saveRDS(xgb.res, './Project files/xgb vs human.RDS')

xgb.dif <- xgb.res$Algorithm - xgb.res$Human
xgb.dif

sum(xgb.dif > 0)/length(xgb.dif)
sum(xgb.dif)
sum(xgb.dif)/length(xgb.dif)

# If you captain the player with the highest probability of scoring at least 1:
# Beats human team 66% of the time, and by a total of 189 points over 33 gameweeks.
# An average of 6.3 points better per week.

xgb.res %>%
  mutate(gw = row_number()) %>%
  mutate(algo_points = cumsum(Algorithm),
         human_points = cumsum(Human)) %>%
  select(-Algorithm, -Human) %>%
  reshape2::melt(id.vars="gw") %>%
  ggplot(aes(x=gw, y = value, group=variable, colour = variable)) +
  geom_line()

xgb.res %>%
  mutate(gw = row_number()) %>%
  reshape2::melt(id.vars="gw") %>%
  ggplot(aes(x=gw, y = value, group=variable, colour = variable)) +
  geom_line()

# Check specific weeks
gw=26
fpl <- cv_gw.2 %>% filter(round==gw) %>% ungroup %>%
  select(element=player_id, total_points, goalprob)

dt <- cv_gw.2 %>%
  filter(minutes >0) %>%
  ungroup %>%
  filter(round == gw) %>%
  select(id=player_id,
         pos=position,
         team=team_name,
         web_name,
         now_cost=price,
         xp) %>%
  dreamteam %>%
  inner_join(fpl, by = "element") %>%
  mutate(total_points = ifelse(captain==1, total_points*2, total_points))

dt
