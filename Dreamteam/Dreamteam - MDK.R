# ------------- Multi-dimensional knapsack dreamteam approach -----------------

library(FLSSS)
library(dplyr)

load('./.RData')

# Turn teams into dummy variables
# t <- fpl.3 %>%
#   mutate(team = as.factor(team)) %>%
#   select(id, team) %>%
#   model.matrix(object = id ~ team) %>%
#   data.frame() %>%
#   select(-X.Intercept.) %>%
#   mutate(teamArsenal = ifelse(rowSums(.)==0,1,0))

# Prepare dataframe. To-do: add club constraint. Using dummy variables breaks the optimisation through timeouts.
dreamteamdat <- fpl.3 %>%
  mutate(gk = ifelse(pos == 'Goalkeeper', 1, 0),
         def = ifelse(pos == 'Defender', 1, 0),
         mid = ifelse(pos == 'Midfielder', 1, 0),
         fwd = ifelse(pos == 'Forward', 1, 0)) %>%
  #cbind(t) %>%
  select(id,
         gk,
         def,
         mid,
         fwd,
         now_cost,
         xp#,95:115
         ) %>%
  filter(!is.na(xp),
         xp > 0)

# Get maximum xp with no constraints
xpmax <- dreamteamdat %>% arrange(desc(xp)) %>% slice(1:15) %>% summarise(n = sum(xp)) %>% unlist
xpmax

# formulate lbound
#lbound=c(2, 5, 5, 3, -Inf, 0.8*xpmax, rep(0, 20))
lbound=c(2, 5, 5, 3, -Inf, 0.75*xpmax)

# Upper bound
# ubound = c(2, 5, 5, 3, 1000, Inf, rep(3, 20))
ubound = c(2, 5, 5, 3, 1000, Inf)

# Select only data for optimisation
mV <- select(dreamteamdat, -id)

# Solve
rst <- mmFLknapsack(len = 15, mV, lbound, ubound, totalSolutionNeeded = 1, tlimit=240, singleTimeLimit = 120)

# View dreamteam
dt.1 <- fpl.3 %>%
  filter(id %in% dreamteamdat[rst[[1]],'id']) %>%
  mutate(order = case_when(pos == 'Goalkeeper' ~ 1,
                           pos == 'Defender' ~ 2,
                           pos == 'Midfielder' ~ 3,
                           pos == 'Forward' ~ 4)) %>%
  arrange(order) %>%
  select(id,
         web_name,
         pos,
         team,
         now_cost,
         xp)

View(dt.1)

# Show total xp and cost for dreamteam
sum(dt.1$xp)
sum(dt.1$now_cost)

# Get best first 11
mV.2 <- dreamteamdat[rst[[1]],]
rst.2 <- mmFLknapsack(len = 11,
                      select(mV.2, -id),
                      lbound = c(1, 3, 2, 1, -Inf, (11/15)*sum(mV[rst[[1]],'xp'])),
                      ubound = c(1, 5, 5, 3, 1000, Inf),
                      totalSolutionNeeded = 1,
                      tlimit=240,
                      singleTimeLimit = 120)
xptotals <- unlist(lapply(rst.2, function(x) sum(mV.2[x,'xp'])))

# Add relevant variables
dt.2 <- fpl.3 %>%
  filter(id %in% mV.2[rst.2[[which(xptotals == max(xptotals))]],'id']) %>%
  mutate(order = case_when(pos == 'Goalkeeper' ~ 1,
                           pos == 'Defender' ~ 2,
                           pos == 'Midfielder' ~ 3,
                           pos == 'Forward' ~ 4)) %>%
  arrange(order) %>%
  select(id,
         web_name,
         pos,
         team,
         now_cost,
         xp)

# View
View(dt.2)

# Subs
View(dt.1[!dt.1$id %in% dt.2$id,])
