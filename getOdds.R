
getOdds <- function (url) {

  webpage <- read_html(url)
  
  #Using CSS selectors to scrap the rankings section
  players <- html_nodes(webpage,'.odds-label')
  
  #Converting the ranking data to text
  players_data <- html_text(players)
  
  #Using CSS selectors to scrap the rankings section
  odds_html <- html_nodes(webpage,'.odds-value')
  
  #Converting the ranking data to text
  odds <- html_text(odds_html)
  
  # Replace 'evens' with 1/1
  odds <- ifelse(odds == 'evens', '1/1', odds)
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
  result$player <- substr(as.character(result$player), start = 1, stop = nchar(as.character(result$player))-1)
  
  return(result)
  
}
