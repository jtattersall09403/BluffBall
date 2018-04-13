library(dplyr)
library(jsonlite)
library(ggplot2)
library(RcppRoll)
library(caret)
library(pROC)
library(purrr)

source('fixtures.R')

fpl <- players()

fpl.all <- lapply(sort(fpl$id), function(i) {
  print(paste(i/max(fpl$id)))
  return(playerDetailed(i))
})

# Save
fpl.all.2 <- do.call(rbind, fpl.all)
saveRDS(fpl.all.2, 'fpl_all.rds')

# Get data
fpl.all.2 <- readRDS('fpl_all.rds')
fixtures <- fixtures()
teams <- teams()

# Define number of previuos games to look at
n <- ifelse(max(fpl.all.2$round) > 5, 5, max(fpl.all.2$round))

# Derive predictor variables. Use rounds prior to 33 - 33 will be used for testing.
modeldata.1 <- fpl.all.2 %>%
  inner_join(dplyr::select(fpl, id, web_name, position, team_name), by = c("player_id"="id")) %>%
  inner_join(dplyr::select(teams, name, strength_overall_home:strength_defence_away), by = c("team_name"="name")) %>%
  inner_join(dplyr::select(teams, name, strength_overall_home:strength_defence_away), by = c("opponent_team"="name")) %>%
  mutate(team_attack_strength = ifelse(was_home, strength_attack_home.x, strength_attack_away.x),
         opp_defence_strength = ifelse(was_home, strength_defence_away.y, strength_defence_home.y),
         strength_ratio = team_attack_strength/opp_defence_strength,
         position = as.factor(position)) %>%
  group_by(player_id) %>%
  arrange(round) %>%
  mutate(assists_act = assists,
         strength_ratio_act = strength_ratio) %>%
  mutate_at(vars(strength_ratio,
                 assists,
                 open_play_crosses,
                 big_chances_created,
                 key_passes,
                 influence,
                 creativity,
                 threat,
                 ict_index), lag)

# Get grouped rolling averages by player. Annoyingly hard in dplyr.
getAvs <- function(i) {
  print(paste(i))
  
  modeldata.1 %>%
    ungroup %>%
    filter(player_id == i) %>%
    mutate(assists = assists/strength_ratio,
           assists_5 = roll_mean(assists, n, align = "right", fill = NA),
           crosses_5 = roll_mean(open_play_crosses/strength_ratio, n, align = "right", fill = NA),
           bigchance_5 = roll_mean(big_chances_created/strength_ratio, n, align = "right", fill = NA),
           keypass_5 = roll_mean(key_passes/strength_ratio, n, align = "right", fill = NA),
           influence_5 = roll_mean(influence/strength_ratio, n, align = "right", fill = NA),
           creativity_5 = roll_mean(creativity/strength_ratio, n, align = "right", fill = NA),
           threat_5 = roll_mean(threat/strength_ratio, n, align = "right", fill = NA),
           ict_index_5 = roll_mean(ict_index/strength_ratio, n, align = "right", fill = NA)) %>%
    dplyr::select(player_id, round, assists_5, influence_5, creativity_5, threat_5, ict_index_5, assists_5, crosses_5, bigchance_5, keypass_5) %>%
    as.data.frame %>%
    return
}

rollavs <- lapply(unique(modeldata.1$player_id), getAvs)
modeldata <- do.call(rbind, rollavs) %>%
  inner_join(modeldata.1, by = c("player_id","round")) %>%
  filter(minutes > 0) %>% # Only look at players who played that round
  filter(position != "Goalkeeper") %>% # only look at outfield players. Goalkeepers can have high influence, which messes up the regression
  filter(!is.na(assists_5)) %>% # Exclude rows without enough historical data
  group_by(team_name, round) %>%
  mutate(av_threat = mean(threat_5, na.rm = TRUE),
         max_threat = max(threat_5, na.rm = TRUE)) %>%
  mutate_at(vars(influence_5, creativity_5, threat_5, ict_index_5,
                 assists_5, crosses_5, bigchance_5, keypass_5), function(x) as.numeric(scale(x))) %>%
  ungroup %>%
  mutate(assists_any = ifelse(assists_act > 0, 1, 0)) %>%
  dplyr::select(round, fixture, player_id, web_name, position, team_name, opponent_team,
         strength_ratio_act, team_attack_strength, opp_defence_strength, influence_5, creativity_5, threat_5, ict_index_5,
         assists_5, crosses_5, bigchance_5, keypass_5, av_threat, max_threat, assists_any, assists_act)

# Exploration
modeldata %>%
  group_by(position) %>%
  summarise(assists = sum(assists_act)) %>%
  ggplot(aes(x=position, y=assists)) +
  geom_bar(stat="identity", fill="dodgerblue4") +
  coord_flip()

# How many times did a player get at least one assist?
sum(modeldata$assists_any)/nrow(modeldata) * 100

# Large discrepancies between positions. Try modelling each separately.
model_mid <- filter(modeldata, position == "Midfielder")

View(arrange(modeldata, web_name, round))
View(arrange(modeldata, round, team_name))

# Save
saveRDS(model_mid, 'assists_data_mid.rds')

# ---------- Exploration ------------

# Get model from predictr
CVtune <- readRDS('initState.Rdata')
model <- CVtune$nb
modeldata <- readRDS('assists_data.rds')
saveRDS(model, 'nb_assist_mod_lagged_std.RDS')

# Get all data
testdata <- modeldata %>%
  dplyr::select(-assists)

# Get table of predicted assist probabilities
pred <- predict.train(model, newdata=testdata, type = "prob")
names(pred) <- c("pas_0","pas_1","pas_2","pas_3")

# Bind probabilities to actual
testdata.2 <- testdata %>%
  ungroup %>%
  cbind(pred) %>%
  mutate_at(vars(pas_0:pas_3), round, 3) %>%
  inner_join(dplyr::select(fpl.all.2, round, player_id, total_points, minutes), by = c("player_id","round")) %>%
  mutate(assist_pts = 3 * assists_act,
         xpas = pas_1*3 + pas_2*6 + pas_3*9) %>%
  mutate(xpas = ifelse(minutes == 0, 0, xpas),
         xpas = round(xpas, 3))
View(testdata.2)

# Diagnostics
caret::RMSE(testdata.2$xpas, testdata.2$assist_pts)
testdata.2$x_assists <- as.numeric(predict.train(model, newdata=testdata, type = "raw"))-1

table(testdata.2$assists, testdata.2$x_assists)

# ------- GBM -----------

# Separate training and test set
modeldata.2 <- modeldata[complete.cases(modeldata),] %>%
  mutate(assists_any = ifelse(assists_any==1, "Assist", "NoAssist")) %>%
  mutate(assists_any = as.factor(assists_any)) %>%
  mutate(assists_fac = case_when(assists_act == 0 ~ "NoAssist",
                                 assists_act == 1 ~ "OneAssist",
                                 assists_act == 2 ~ "TwoAssists",
                                 assists_act == 3 ~ "ThreeAssists"),
         assists_fac = as.factor(assists_fac))
trainIndex <- createDataPartition(modeldata.2$assists_any,
                                  p = 1-(33/100),
                                  list = FALSE,
                                  times = 1)

imbal_train <- modeldata.2[trainIndex, c(21, 5, 8:20)]
imbal_test  <- modeldata.2[-trainIndex, c(21, 5, 8:20)]

prop.table(table(imbal_train$assists_any))

# Set up control function for training

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

# Build a standard classifier using a gradient boosted machine
orig_fit <- train(assists_any ~ .,
                  data = imbal_train,
                  method = "gbm",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)

# Build custom AUC function to extract AUC
# from the caret model object

test_roc <- function(model, data) {
  
  roc(data$assists_any,
      predict(model, data, type = "prob")[, "Assist"])
  
}

orig_fit %>%
  test_roc(data = imbal_test) %>%
  auc()

# .67, which sounds reasonable

# Create model weights (they sum to one)

model_weights <- ifelse(imbal_train$assists_any == "Assist",
                        (1/table(imbal_train$assists_any)[1]) * 0.5,
                        (1/table(imbal_train$assists_any)[2]) * 0.5)

# Use the same seed to ensure same cross-validation splits

ctrl$seeds <- orig_fit$control$seeds

# Build weighted model

weighted_fit <- train(assists_any ~ .,
                      data = imbal_train,
                      method = "gbm",
                      verbose = FALSE,
                      weights = model_weights,
                      metric = "ROC",
                      trControl = ctrl)

# Examine results for test set

model_list <- list(original = orig_fit,
                   weighted = weighted_fit)

# Compare original with weighted
test_roc(orig_fit, data = imbal_test)
test_roc(weighted_fit, data = imbal_test)

# Slight improvement by using weights

# Get actual predictions
p_as <- predict(weighted_fit, newdata=modeldata.2, type = "prob")
modeldata.2$p_as <- p_as$Assist
modeldata.2$xpas <- 3*modeldata.2$p_as
modeldata.2$assist_pts <- 3 * modeldata.2$assists_act

# Actually not too bad! Let's save it
saveRDS(weighted_fit, 'gbm_assist_model.RDS')

# Plots
orig <- test_roc(orig_fit, data = imbal_test)
weight <- test_roc(weighted_fit, data = imbal_test)

origdf <- data_frame(tpr = orig$sensitivities,
           fpr = c(1 - orig$specificities),
           model = "orig")
weightdf <- data_frame(tpr = weight$sensitivities,
                     fpr = c(1 - weight$specificities),
                     model = "weight")
results_df_roc <- bind_rows(list(origdf, weightdf))

# Plot
ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


# ------ Multi class version ---------

# Do this, but EDIT to just classify 0, 1 and 2
trainIndex <- createDataPartition(modeldata.2$assists_fac,
                                  p = 1-(33/100),
                                  list = FALSE,
                                  times = 1)

imbal_train <- modeldata.2[trainIndex, c(23, 5, 8:20)]
imbal_test  <- modeldata.2[-trainIndex, c(23, 5, 8:20)]

prop.table(table(imbal_train$assists_fac))

# Set up control function for training

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     summaryFunction = multiClassSummary ,
                     classProbs = TRUE)

# Build a standard classifier using a gradient boosted machine
orig_fit <- train(assists_fac ~ .,
                  data = imbal_train,
                  method = "gbm",
                  verbose = TRUE,
                  metric = "ROC",
                  trControl = ctrl,
                  train.fraction = 0.75)

# Build custom AUC function to extract AUC
# from the caret model object

test_roc <- function(model, data) {
  
  roc(data$assists_any,
      predict(model, data, type = "prob")[, "Assist"])
  
}

orig_fit %>%
  test_roc(data = imbal_test) %>%
  auc()

# 

# Create model weights (they sum to one)
model_weights <- inner_join(imbal_train,
                            as.data.frame((1/table(imbal_train$assists_fac)) * 0.25),
                            by = c("assists_fac"="Var1")) %>%
  dplyr::select(Freq) %>%
  unlist %>%
  as.numeric

# Use the same seed to ensure same cross-validation splits

ctrl$seeds <- orig_fit$control$seeds

# Build weighted model

weighted_fit <- train(assists_fac ~ .,
                      data = imbal_train,
                      method = "gbm",
                      verbose = TRUE,
                      weights = model_weights,
                      metric = "ROC",
                      trControl = ctrl)

# Examine results for test set

model_list <- list(original = orig_fit,
                   weighted = weighted_fit)

# Get actual predictions
p_as <- predict(weighted_fit, newdata=imbal_test, type = "prob")
imbal_test$p_as <- p_as$OneAssist
imbal_test$p_as2 <- p_as$TwoAssists
imbal_test$p_as3 <- p_as$ThreeAssists
imbal_test$name <- modeldata.2$web_name[-trainIndex]
imbal_test$assists_act <- modeldata.2$assists_act[-trainIndex]
imbal_test$xpas <- 3*imbal_test$p_as + 6*imbal_test$p_as2 + 9*imbal_test$p_as3
imbal_test$round <- modeldata.2$round[-trainIndex]
imbal_test$assist_pts <- imbal_test$assists_act * 3

View(imbal_test)

multiclass.roc(as.numeric(unlist(imbal_test$assists_act)), imbal_test$p_as+imbal_test$p_as2+imbal_test$p_as3)

# Actually not too bad! Let's save it
saveRDS(weighted_fit, 'gbm_assist_model.RDS')

