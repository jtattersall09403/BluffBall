# --------- Model performance ----------

library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(reshape2)
library(caret)

# Get data
load('.RData')

# Get actuals
fpldat <- getFPLSummary() %>%
  mutate('player_name' = paste(first_name, second_name)) %>%
  select(id, event_points)

# Match to predicted
mdl <- fpl.3 %>%
  inner_join(fpldat, by = 'id') %>%
  mutate(last_event_points = event_points.x,
         event_points = event_points.y)

# How close was expected points to actual points summed over the whole gameweek?
sum(mdl$xp, na.rm=TRUE)
sum(mdl$event_points)

# Distributions of predicted and actual points
hist(mdl$event_points[mdl$event_points!=0])
hist(mdl$xp[mdl$event_points!=0])

# Excluding zeroes
mdl %>% filter(event_points != 0) %>% select(-web_name) %>% melt(id.vars = 'id') %>%
  ggplot(aes(value, group = variable, fill = variable)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  scale_fill_viridis(discrete = T,  begin = 0.8, end = 0.3)

# Including zeroes
mdl %>% select(-web_name) %>% melt(id.vars = 'id') %>%
  ggplot(aes(value, group = variable, fill = variable)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  scale_fill_viridis(discrete = T,  begin = 0.8, end = 0.3)

# Plot
p <- mdl %>%
  ggplot(aes(x=xp, y = event_points, label = web_name)) +
  geom_point(alpha = 0.5, colour = 'dodgerblue4') +
  geom_smooth(method = 'lm')
p
ggplotly(p)

# Residuals
res <- mdl$xp - mdl$event_points

# Residuals plots
ggplot(data.frame(res), aes(res)) + geom_histogram()
data.frame(res) %>% cbind(mdl) %>% filter(xp !=0) %>% ggplot(aes(res)) + geom_histogram()

data.frame(mdl$xp, res) %>%
  ggplot(aes(mdl.xp, res)) +
  geom_point(colour = 'deeppink4', alpha = 0.5)

# Check residuals if rounded xp used as prediction
p3 <- data.frame(mdl$xp, res, mdl$web_name) %>%
  mutate(res = sqrt(res^2),
         mdl.xp = round(mdl.xp, 0)) %>%
  ggplot(aes(mdl.xp, res, label = mdl.web_name)) +
  geom_point(colour = 'deeppink4', alpha = 0.5)
ggplotly(p3)

data.frame(mdl$xp, res, mdl$web_name) %>%
  mutate(mdl.xp = round(mdl.xp, 0)) %>%
  filter(mdl.xp != 0) %>%
  ggplot(aes(res)) +
  geom_histogram()
  
# RMSE
sqrt(mean(res^2,na.rm=T))

# R square
caret::R2(mdl$xp, mdl$event_points, na.rm = T)

# Record gw
mdl$gw <- 32

# Save data
saveRDS(mdl, './Project files/Data archive/gw32.rds')

# --------------- Dreamteam performance -------------

source('./Dreamteam/Dreamteam - recursive v2.R')

# Get player and team data
fpldat <- getFPLSummary() %>%
  mutate('player_name' = paste(first_name, second_name))

# Select best possible team using your algorithm, with budget available to best human player
dt_act <- fpldat %>%
  mutate(xp = event_points) %>%
  dreamteam(budget = 1000)

dt_act
sum(dt_act[1:11, 'xp'])

# If this continues to be just as good as the best human player/the fpl app dreamteam,
# then no need to change it