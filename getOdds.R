
getOdds <- function (url, teams) {

  webpage <- read_html(url)
  
  #Using CSS selectors to scrap the rankings section
  players <- html_nodes(webpage,'.leftPad')
  
  #Converting the ranking data to text
  players_data <- html_text(players)
  players_data <- players_data[!grepl("Goalscorer Markets", players_data)]
  players_data <- players_data[!grepl("\\d", players_data)]
  players_data <- players_data[!players_data %in% teams]
  players_data <- substr(players_data, 1, nchar(players_data) - 2)
  players_data <- players_data[players_data != '']
  players_data <- players_data[!grepl("\\n", players_data)]
  
  #Using CSS selectors to scrap the rankings section
  odds_html <- html_nodes(webpage,'.eventprice')
  
  #Converting the ranking data to text
  odds <- html_text(odds_html)
  odds <- gsub('\n\t\t\t\n\t\t\t\n\t\t\t\t\n\t\t\t\t\t',
               '',
               odds)
  odds <- gsub('\n\t\t\t\t\n\t\t\t\n\t\t\t\n\t\t',
               '',
               odds)
  
  # Replace 'evens' with 1/1
  odds <- ifelse(odds == 'EVS', '1/1', odds)
  odds <- ifelse(odds == '1/1000','0',odds)
  
  # Divide odds by odds + 1
  odds.split <- strsplit(odds, split = "/") %>%
    lapply(as.numeric) %>%
    lapply(function(x) x[1]/x[2]) %>%
    lapply(function(x) round(1-(x/(x+1)), 4)) %>%
    do.call(rbind, .)
  
  # Bind players and odds
  result <- data.frame('player' = players_data, 'probability' = odds.split)
  
  # Keep first occurrence of each player (focusing on next gameweek)
  result <- result[match(unique(result$player), result$player),]

  return(result)
  
}

getLineups <- function (url, fpl.2) {
  
  webpage <- read_html(url)
  
  #Using CSS selectors to scrap the rankings section
  players <- html_nodes(webpage,'.player-name')
  teams <- html_nodes(webpage, 'h2')
  
  #Converting the data
  teams_data <- data.frame(team=html_text(teams), stringsAsFactors = F) %>%
    mutate(team = case_when(team=="Tottenham Hotspur" ~ "Spurs",
                            team=="Huddersfield Town" ~ "Huddersfield",
                            team=="Brighton and Hove Albion" ~ "Brighton",
                            team=="Leicester City" ~ "Leicester",
                            team=="Manchester City" ~ "Man City",
                            team=="Manchester United" ~ "Man Utd",
                            team=="Newcastle United" ~ "Newcastle",
                            team=="Stoke City" ~ "Stoke",
                            team=="Swansea City" ~ "Swansea",
                            team=="West Bromwich Albion" ~ "West Brom",
                            team=="West Ham United" ~ "West Ham",
                            TRUE ~ team)) %>%
    mutate(teamid = row_number())
  
  # Player names and teams
  players_data <- data.frame(teamid = rep(1:20, each=11),
                             web_name=html_text(players)) %>%
    mutate(web_name = gsub("é","e",
                         gsub("á", "a",
                              gsub("õ", "o",
                                   gsub("í","i",
                                        gsub("ã","a",
                                             gsub("Ö","O",
                                                  gsub("ß","ss",web_name)))))))) %>%
    inner_join(teams_data, by = "teamid") %>%
    mutate(web_name = gsub("Ayoze Perez", "Ayoze", web_name),
           web_name = gsub("Mat Ryan", "Ryan", web_name),
           web_name = gsub("S Cook", "Steve Cook", web_name),
           web_name = gsub("Cook", "Lewis Cook", web_name)) %>%
    mutate(player_name = web_name,
           second_name = web_name) %>%
    select(-teamid)
  
  # --------------- Data matching --------------
  
  # Filter fpl data
  fpl_formatch <- fpl.2 %>%
    select(id, web_name, player_name, second_name, team)
  
  # Make names consistent
  right <- fpl_formatch
  
  # Get left table
  left <- players_data
  
  # To do - sort out matching. Why doesn't partial match work? Check against the example.
  # Link to FPL data
  matches.out <- fastLink(
    dfA = left,
    dfB = right, 
    varnames = c("web_name","team"),
    stringdist.match = c("web_name"), # Specifies the variables you want to treat as strings for fuzzy matching
    #partial.match = c("web_name"), # Specifes variables where you want the algorithm to check for partial matches
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
  namesmatched <- cbind(fpl_formatch[b,],"matchindex"=b, "match"=matches.out$posterior)
  
  matched.data <- left_join(left,
                            namesmatched,
                            by="matchindex")
  
  # Keep most likely match for each
  dedup <- matched.data %>%
    group_by(id) %>%
    mutate(rank = ifelse(is.na(match), 1, rank(match, ties.method='first'))) %>%
    filter(rank == 1)
  
  # Return player id's and whether they're in the starting lineup
  result <- dedup %>% ungroup %>% select(id) %>% mutate(pred_lineup=1)
  
  return(result)
  
}
