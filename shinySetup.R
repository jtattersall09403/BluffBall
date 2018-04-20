# Shiny setup

library(rvest)
library(dplyr)
library(fastLink)
library(reshape2)
library(fplr)
library(parallel)

source('./GetFPLData.R')
source('./getOdds.R')
source('./Dreamteam/Dreamteam - recursive v2.R')

# Pick up previous data
load('.RData')

# Set upcoming gw
gw <- 34

# ----------------- FPL data ---------------

# Get player and team data
fpldat <- getFPLSummary() %>%
  mutate_at(c(3,7,8), function(x) gsub("é","e",
                               gsub("á", "a",
                                    gsub("õ", "o",
                                         gsub("í","i",
                                              gsub("ã","a",
                                                   gsub("Ö","O",
                                                        gsub("ß","ss",x)))))))) %>%
  mutate('player_name' = paste(first_name, second_name))

# Get teams
teams <- as.character(unique(fpldat$team))
teams[teams == 'Spurs'] <- 'Tottenham'

# Get last week's dreamteam points
n <- length(dt.all)
if(!identical(dt.all[[n]]$element, dt.3$element)) {
  dt.last <- dt.3 %>%
    inner_join(select(fpldat, id, event_points), by = c('element'='id')) %>%
    mutate(event_points = ifelse(captain == 1, event_points * 2, event_points))
  
  # Make auto subs and add to list
  dt.last.2 <- autosub(dt.last)
  dt.all[[n+1]] <- dt.last.2
}

# Show total points
n <- length(dt.all)
sum(dt.all[[n]][1:11,'event_points'])

# --------------------------------------- Fixtures -----------------------------

# Get next set of fixtures
fixtures = fixtures() %>%
  filter(event == gw)

# Get upcoming fixture details by team
fix2 <- fixtures %>%
  select(id, event_day, team_a, team_h) %>%
  melt(id.vars=c('id','event_day')) %>%
  group_by(value) %>%
  mutate(order_fix = row_number(event_day)) %>%
  inner_join(select(.,id, value), by = "id") %>%
  filter(value.y != value.x) %>%
  mutate(was_home = ifelse(variable=="team_a", FALSE, TRUE)) %>%
  group_by(value.x) %>%
  mutate(num_fix = n()) %>%
  select(fixture=id, order_fix, num_fix, team_name=value.x, opponent_team=value.y, was_home)

# -------------------------------------- Goalscorer odds -----------------------

url <- 'http://sports.williamhill.com/bet/en-gb/betting/g/348/Anytime+Goalscorer.html'

# Get anytime goalscorer odds
result <- getOdds(url, teams)

# See the results
View(arrange(result, desc(probability)))

# -------------------------- Score a brace -------------------------------

url <- 'http://sports.williamhill.com/bet/en-gb/betting/g/135015/Player+To+Score+2+Or+More.html'
result2 <- getOdds(url, teams) %>%
  rename(probBrace = probability)

# -------------------------- Score a hattrick -------------------------------

url <- 'http://sports.williamhill.com/bet/en-gb/betting/g/13428/Hat-trick.html'
result3 <- getOdds(url, teams) %>%
  rename(probHt = probability)

# Merge with other goalscorer data
result <- result %>%
  inner_join(result2, by = 'player') %>%
  inner_join(result3, by = 'player')

# ----------------------- Data matching -----------------------

# Get player data
names.split <- do.call(rbind, strsplit(as.character(result$player), ' (?=[^ ]+$)', perl=TRUE)) %>%
  as.data.frame(stringsAsFactors = F) %>%
  rename("first_name" = V1, "second_name" = V2) %>%
  cbind('player_name' = as.character(result$player),
        'goalprob' = result$probability,
        'probBrace' = result$probBrace,
        'probHt' = result$probHt) %>%
  mutate(web_name = second_name)

# Make names consistent
right <- names.split

# remove goalkeepers - won't be in goalscorer odds
left <- fpldat %>% filter(pos != "Goalkeeper")

# To do - sort out matching. Why doesn't partial match work? Check against the example.
# Link to FPL data
matches.out <- fastLink(
  dfA = left,
  dfB = right, 
  varnames = c("player_name","web_name", "first_name","second_name"),
  stringdist.match = c("player_name", "web_name", "first_name","second_name"), # Specifies the variables you want to treat as strings for fuzzy matching
  #partial.match = c("player_name","web_name", "first_name","second_name"), # Specifes variables where you want the algorithm to check for partial matches
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
namesmatched <- cbind(names.split[b,],"matchindex"=b, "match"=matches.out$posterior)

matched.data <- left_join(left,
                          namesmatched,
                          by="matchindex")

# Keep most likely match for each
dedup <- matched.data %>%
  group_by(first_name.y, second_name.y) %>%
  mutate(rank = ifelse(is.na(match),1, rank(match, ties.method='first'))) %>%
  filter(rank == 1) %>%
  rename(web_name = web_name.x, player_name = player_name.x) %>%
  select(-web_name.y, -player_name.y)

# Get goalkeepers
keepers <- fpldat %>%
  filter(pos == 'Goalkeeper') %>%
  mutate(matchindex = as.numeric(NA),
         first_name.y = first_name,
         second_name.y = second_name,
         goalprob = as.numeric(0),
         probBrace = as.numeric(0),
         probHt = as.numeric(0),
         match = as.numeric(NA),
         rank = 1) %>%
  rename(first_name.x = first_name, second_name.x = second_name)

# Append goalkeepers
names(keepers) <- names(dedup)
fpl <- rbind(as.data.frame(dedup), as.data.frame(keepers)) %>%
  mutate(goalprob1 = goalprob - probBrace - probHt)

# Show whose goalscorer odds are missing
fpl %>%
  filter(pos != 'Goalkeeper', (is.na(goalprob))) %>%
  select(web_name, team, goalprob, form) %>%
  arrange(desc(form)) %>% View

# Get those that didn't match and sor tout manually
nonmatched <- fpl %>%
  filter(pos != 'Goalkeeper', (is.na(goalprob))) %>%
  select(-goalprob,
         -probBrace,
         -probHt,
         -match,
         -rank) %>%
  mutate(player=case_when(web_name=="Willian" ~ "Willian",
                          web_name=="Joao Mario" ~ "Joao Mario",
                          id==302 ~ "Ayoze",
                          web_name=="Bernardo Silva" ~ "Bernardo Silva")) %>%
  inner_join(names.split, by=c("player"="player_name")) %>%
  mutate(match = as.numeric(NA),
         rank = 1) %>%
  mutate(web_name = web_name.x) %>%
  select(-first_name, -second_name, -web_name.y, -web_name.x, -player)
  
# Append manual matches to main file
fpl <- fpl %>%
  filter(!id %in% nonmatched$id) %>%
  union(nonmatched)
  
# Show whose goalscorer odds are missing
fpl %>%
  filter(pos != 'Goalkeeper', (is.na(goalprob))) %>%
  select(web_name, team, status, goalprob, form) %>%
  arrange(desc(form)) %>% View

# ------------------------ Clean sheet data ---------------------

url <- 'http://sports.williamhill.com/bet/en-gb/betting/g/158525/To+Keep+a+Clean+Sheet.html'

webpage <- read_html(url)

#Using CSS selectors to scrape the odds
teams_odds <- html_nodes(webpage,'.leftPad')

#Converting the pdds data to text
teams_data <- html_text(teams_odds)

# Filter out headers
teams_data <- teams_data[!grepl('To Keep a Clean Sheet', teams_data)]
teams_data <- grep(paste(teams,collapse="|"), teams_data, value=TRUE)
teams_data <- gsub('\n\t\t\t\t\t\n\t\t\t\t\t\t',
                   '',
                   teams_data)
teams_data <- gsub('\n\t\t\t\t\t\n\t\t\t\t',
                   '',
                   teams_data)

#Using CSS selectors to scrape the ods section
cs_html <- html_nodes(webpage,'.eventprice')

#Converting the odds data to text
cs <- html_text(cs_html)
cs <- gsub('\n\t\t\t\n\t\t\t\n\t\t\t\t\n\t\t\t\t\t',
           '',
           cs)
cs <- gsub('\n\t\t\t\t\n\t\t\t\n\t\t\t\n\t\t',
           '',
           cs)

# Replace 'evens' with 1/1, and missing events with 0
cs <- ifelse(cs == 'EVS', '1/1', cs)
cs <- ifelse(cs == '1/1000','0',cs)

# Divide odds by odds + 1
cs.split <- strsplit(cs, split = "/") %>%
  lapply(as.numeric) %>%
  lapply(function(x) x[1]/x[2]) %>%
  lapply(function(x) round(1-(x/(x+1)), 4)) %>%
  do.call(rbind, .)

# Bind players and odds
cs <- data.frame('team' = as.character(teams_data), 'cs' = cs.split, stringsAsFactors = F) %>%
  group_by(team) %>%
  mutate(order_fix=row_number())

# Keep one probability for each fixture in next gameweek
# cs <- cs[match(unique(cs$team), cs$team),]
cs$team <- ifelse(cs$team == "Tottenham", "Spurs", cs$team)
cs <- inner_join(fix2, cs, by=c("team_name"="team", "order_fix"))

# See the results
View(arrange(cs, desc(cs)))

# Match cs odds to fpl data
fpl <- fpl %>%
  left_join(cs, by = c('team'='team_name'))

# ------------------------ Predict likelihood of playing 60 minutes ------------------------

# And get historic data for each player

source('./startprob.R')


# ------------------------ Deal with double gameweeks -------------------------------


# Some complex probability going on here
fpl.1.1 <- fpl.1 %>%
  group_by(id) %>%
  arrange(order_fix) %>%
  mutate(prev_opp_strength = lag(opp_defence_strength)) %>%
  mutate(goalprob=ifelse(order_fix==2, goalprob*prev_opp_strength/opp_defence_strength, goalprob),
         goalprob1=ifelse(order_fix==2, goalprob1*prev_opp_strength/opp_defence_strength, goalprob1),
         probBrace=ifelse(order_fix==2, probBrace*prev_opp_strength/opp_defence_strength, probBrace),
         probHt=ifelse(order_fix==2, probHt*prev_opp_strength/opp_defence_strength, probHt)) %>%
  mutate(goalprob_new = ifelse(order_fix==2,
                           1-(1-goalprob)*(1-lag(goalprob)),
                           goalprob),
         goalprob1_new = ifelse(order_fix==2,
                            (1-lag(goalprob))*goalprob1 + lag(goalprob1)*(1-goalprob),
                            goalprob1),
         probBrace_new = ifelse(order_fix==2,
                            (1-lag(goalprob))*probBrace +
                              lag(probBrace)*(1-goalprob) +
                              lag(goalprob1)*goalprob1,
                            probBrace),
         probHt_new = ifelse(order_fix==2,
                         (1-lag(goalprob))*probHt +
                           lag(probHt)*(1-goalprob) +
                           lag(goalprob1)*probBrace +
                           lag(probBrace)*goalprob1,
                         probHt),
         cs_new = ifelse(order_fix==2,
                                (1-lag(cs))*cs + lag(cs)*(1-cs),
                                cs),
         cs2_new = ifelse(order_fix==2,
                         lag(cs)*cs,
                         0),
         as_new = ifelse(order_fix==2,
                         (1-lag(probas))*probas + lag(probas)*(1-probas),
                         probas),
         as2_new = ifelse(order_fix==2,
                          lag(probas)*probas,
                          0)
         ) %>%
  mutate(goalprob=goalprob_new,
         goalprob1=goalprob1_new,
         probBrace=probBrace_new,
         probHt=probHt_new,
         cs=cs_new,
         cs2=cs2_new,
         probas=as_new,
         probas2=as2_new) %>%
  group_by(id) %>%
  summarise_at(vars(goalprob:goalprob1, cs, cs2, prob60, probas, probas2, xpas), max)

fpl.2 <- fpldat %>%
  inner_join(fpl.1.1, by = "id")

# ------------------- Expected points -----------------


# Check how many have odds available
print(c(paste0(100*round(sum(!is.na(fpl.2$goalprob))/nrow(fpl.2[fpl.2$pos != 'Goalkeeper',]),3), '% of players have goalscorer odds'),
        paste0(100*round(sum(fpl.2$probas>0)/nrow(fpl.2[fpl.2$pos != 'Goalkeeper',]),3), '% of players have assist predictions > 0'),
        paste0(100*round(sum(!is.na(fpl.2$cs))/nrow(fpl.2),3), '% of players have clean sheet odds'),
        paste0(100*round(sum(!is.na(fpl.2$prob60))/nrow(fpl.2),3), '% of players have a playing time prediction')))

# Show whose goalscorer odds are missing
fpl.2 %>%
  filter(pos != 'Goalkeeper', (is.na(goalprob) | is.na(prob60))) %>%
  select(web_name, team, goalprob, prob60) %>%
  arrange(desc(prob60)) %>% View

# Match on points lookup
points <- data.frame('pos' = sort(unique(fpl$pos)),
                     'goal' = c(6,6,5,4),
                     'cleansheet' = c(4,4,1,0))

# Calculate expected points
fpl.3 <- fpl.2 %>%
  ungroup %>%
  inner_join(points, by='pos') %>%
  mutate(points_per_game = as.numeric(points_per_game)) %>%
  mutate(games = total_points/points_per_game,
         prob60 = prob60) %>%
  mutate(prob0 = .2 * (1-prob60),
         probless60 = .8 * (1-prob60)) %>%
  mutate(prob60 = ifelse(prob60 < 0.15, 0, prob60)) %>%
  mutate(prob60 = ifelse(as.numeric(ep_next) <= 0, 0, prob60)) %>% # Set probability of playing to 0 if no fixture
  mutate(xpas = prob60*(3*probas + 6*probas2),
         xgp1 = prob60*goalprob1 * goal,
         xgp2 = prob60*probBrace * goal * 2,
         xgp3 = prob60*probHt * goal * 3,
         xm = prob60 * 90,
         xgp = xgp1 + xgp2 + xgp3) %>%
  mutate(xm = ifelse(is.na(xm), 0, xm)) %>%
  mutate(xpap = ifelse(xm >= 60, 2, ifelse(xm > 0, 1, 0)),
         xpcs = (prob60 * cs * cleansheet) + (prob60 * cs2 * cleansheet)) %>%
  mutate(xp = ifelse(is.na(xgp),0,xgp) + ifelse(is.na(xpap),0,xpap) + xpcs + ifelse(is.na(xpas),0,xpas)) %>%
  mutate(xp = ifelse(prob60 == 0, 0, xp)) #%>% # Set to 0 if not predicted to play
  #mutate(xp = ifelse(is.na(goalprob), as.numeric(ep_next), xp)) # Set to modelled value if goal odds not present.

# Show ep for those whose goalscorer odds are missing
# fpl.3 %>%
#   filter(pos != 'Goalkeeper', (is.na(goalprob) | is.na(prob60))) %>%
#   select(web_name, team, goalprob, prob60, xp) %>%
#   arrange(desc(prob60)) %>% View

# Get all fpl pairs
fpl.3$dum <- 1
fplsquad <- fpl.3 %>%
  select(dum, id,  first_name, second_name, pos, team, now_cost, xp) %>%
  inner_join(select(fpl.3, id, dum, first_name, second_name, pos, team, now_cost, xp), by = 'dum') %>%
  filter(!(first_name.x == first_name.y & second_name.x == second_name.y)) %>%
  mutate(pos = paste(pos.x, pos.y, sep="-"),
         price = now_cost.x + now_cost.y,
         xp = xp.x + xp.y) %>%
  filter(xp > 2) %>%
  select(id.x,
         id.y,
         second_name.x,
         second_name.y,
         pos, price, xp)

# Remove duplicates
fplsquad <- fplsquad[!duplicated(data.frame(t(apply(fplsquad[,c(1,2)], 1, sort)), fplsquad$price)),]


# Get dreamteam
dt.3 <- dreamteam(fpl.3)

dt.3
sum(dt.3[1:11,'xp'])
sum(dt.3['now_cost'])

# Remove unnecessary objects
rm(list = c('cs',
            'cs_html',
            'cs.split',
            'dedup',
            'dt.1',
            'dt.2',
            'dt.2.1',
            'dt.2.2',
            'dt.2.3',
            'first11',
            'fpl.1',
            'fpl.2',
            'fpldat',
            'keepers',
            'left',
            'matched.data',
            'matches.out',
            'model',
            'modeldata',
            'modeldata2',
            'modeldata3',
            'modelresults',
            'names.split',
            'namesmatched',
            'odds_html',
            'odds.split',
            'players',
            'result',
            'result2',
            'result3',
            'right',
            'subs',
            't',
            't.1',
            't.i',
            'teams_odds',
            'train',
            'webpage',
            'a',
            'b',
            'bank',
            'fitted.results',
            'i',
            'index11',
            'iter_max',
            'misClasificError',
            'n',
            'odds',
            'players_data',
            'teams_data',
            'url',
            'bestTeam',
            'test',
            'data',
            'myteam2',
            'mysquad',
            'squad',
            'cl',
            'double_transfers',
            'dt.f11',
            't.1.1',
            'tmp',
            'autosubs',
            'dt.squad',
            'nonmatched',
            'as_modeldata',
            'as_modeldata.2',
            'as_modelresults',
            'clubs',
            'dt',
            'dt_other',
            'dt.details',
            'dt.last',
            'dt.last.2',
            'fb',
            'fix2',
            'fixtures',
            'fpl.1.1',
            'kap',
            'model2',
            'modelresults',
            'modelresults.2',
            'p_as',
            'players.2',
            'probs',
            'sim',
            't2',
            'teamdetails',
            'teams.2',
            'teamsim.1',
            'teamsim.2',
            'x',
            'xt'))

# Clean up remaining objects
fpl <- fpl %>%
  select(id, web_name, first_name.x, second_name.x, pos, team, now_cost, total_points, cs, goalprob)

# Save everything for shinyApp
save.image()

