
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
