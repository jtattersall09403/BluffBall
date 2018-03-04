# Shiny setup

library(rvest)
library(dplyr)
library(fastLink)
library(reshape2)
library(fplr)

source('./GetFPLData.R')
source('./getOdds.R')

# ----------------- FPL data ---------------

# Get player and team data
fpldat <- getFPLSummary() %>%
  mutate('player_name' = paste(first_name, second_name))

# Get teams
teams <- as.character(unique(fpldat$team))
teams[teams == 'Spurs'] <- 'Tottenham'


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
  stringdist.match = c("player_name","web_name", "first_name","second_name"), # Specifies the variables you want to treat as strings for fuzzy matching
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


# ------------------------ Clean sheet data ---------------------

url <- 'http://sports.williamhill.com/bet/en-gb/betting/g/158525/To+Keep+a+Clean+Sheet.html'

webpage <- read_html(url)

#Using CSS selectors to scrap the rankings section
teams_odds <- html_nodes(webpage,'.leftPad')

#Converting the ranking data to text
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
cs <- data.frame('team' = as.character(teams_data), 'cs' = cs.split, stringsAsFactors = F)

# Keep first occurrence of each player (focusing on next gameweek)
cs <- cs[match(unique(cs$team), cs$team),]
cs$team <- ifelse(cs$team == "Tottenham", "Spurs", cs$team)

# See the results
View(arrange(cs, desc(cs)))

# Match cs odds to fpl data
fpl <- fpl %>%
  left_join(cs, by = 'team')

# ------------------------ Predict likelihood of playing 60 minutes ------------------------

# And get historic data for each player

source('./startprob.R')


# ------------------- Expected points -----------------


# Check how many have odds available
fpl.2 <- fpl.1
print(c(paste0(100*round(sum(!is.na(fpl.2$goalprob))/nrow(fpl.2[fpl.2$pos != 'Goalkeeper',]),3), '% of players have goalscorer odds'),
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
  mutate(points_per_game = as.numeric(points_per_game),
         chance_of_playing_next_round = ifelse(is.na(chance_of_playing_next_round), 100, chance_of_playing_next_round)) %>%
  mutate(games = total_points/points_per_game,
         prob60 = prob60 * chance_of_playing_next_round/100) %>%
  mutate(probas = as.numeric(assists)/as.numeric(games),
         prob0 = .2 * (1-prob60),
         probless60 = .8 * (1-prob60)) %>%
  mutate(prob60 = ifelse(prob60 < 0.08, 0, prob60)) %>%
  mutate(xgp1 = goalprob1 * goal,
         xgp2 = probBrace * goal * 2,
         xgp3 = probHt * goal * 3,
         xm = prob60 * 90,
         xgp = xgp1 + xgp2 + xgp3) %>%
  mutate(xm = ifelse(is.na(xm), 0, xm)) %>%
  mutate(xpap = ifelse(xm >= 60, 2, ifelse(xm > 0, 1, 0)),
         xpcs = prob60 * cs * cleansheet,
         xpas = prob60 * probas * 3) %>%
  mutate(xp = ifelse(is.na(xgp),0,xgp) + ifelse(is.na(xpap),0,xpap) + xpcs, ifelse(is.na(xpas),0,xpas))

# Get all fpl pairs
fpl.3$dum <- 1
fplsquad <- fpl.3 %>%
  select(dum, id,  first_name.x, second_name.x, pos, team, now_cost, xp) %>%
  inner_join(select(fpl.3, id, dum, first_name.x, second_name.x, pos, team, now_cost, xp), by = 'dum') %>%
  filter(!(first_name.x.x == first_name.x.y & second_name.x.x == second_name.x.y)) %>%
  mutate(pos = paste(pos.x, pos.y, sep="-"),
         price = now_cost.x + now_cost.y,
         xp = xp.x + xp.y) %>%
  select(id.x,
         id.y,
         first_name.x.x,
         second_name.x.x,
         first_name.x.y,
         second_name.x.y,
         pos, price, xp)

# Get dreamteam
source('./Dreamteam/Dreamteam - recursive.R')

# Set up table format string
strformat <- "function( nRow, aData) {ind = 2; $('td:eq('+ind+')', nRow).html( parseFloat(aData[ind]).toFixed(2) );}"

# Save everything for shinyApp
save.image()

