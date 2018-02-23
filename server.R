# ------------- BluffBall - use bookie's odds to optimise fantasy premier league performance ----------------

library(shiny)
library(rvest)
library(dplyr)
library(fastLink)
library(reshape2)
library(fplr)
library(reshape2)
library(TTR)

source('GetFPLData.R')
source('getOdds.R')

# Pick up previous data


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  fpl <- eventReactive(input$odds_scrape, {
    
    # ----------------- FPL data ---------------
    
    # Get player and team data
    fpldat <- getFPLSummary() %>%
      mutate('player_name' = paste(first_name, second_name))
    
    # Record progress
    showNotification("Fantasy premier league data downloaded", duration = 10)
    
    # Get teams
    teams <- as.character(unique(fpldat$team))
    teams[teams == 'Spurs'] <- 'Tottenham'
    
    
    # -------------------------------------- Goalscorer odds -----------------------
    
    url <- 'http://sports.williamhill.com/bet/en-gb/betting/g/348/Anytime+Goalscorer.html'
    
    # Get anytime goalscorer odds
    result <- getOdds(url, teams)
    
    # See the results
    #View(arrange(result, desc(probability)))
    
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
    
    # Record progress
    showNotification("Goalscorer and clean sheet odds downloaded", duration = 10)
    
    # ----------------------- Data matching -----------------------
    
    # Record progress
    id <- showNotification("Matching FPL data to odds...", duration = 20)
    
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
      #partial.match = c("web_name", "first_name","second_name"), # Specifes variables where you want the algorithm to check for partial matches
      verbose = T,
      return.all = T
      #threshold.match = .01 # Match probability threshold. The default is .85, and you can play around with different values
    )
    
    # Gives the match rate, estimated falst positive rate (FDR) and estimated false negative rate (FNR)
    #summary(matches.out)
    
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
    
    # Match cs odds to fpl data
    fpl <- fpl %>%
      left_join(cs, by = 'team')
    
    # Record progress
    showNotification("Done!", duration = 20)
    
    return(fpl)
    
  })
  
  # Create goalscorer odds table
  output$goalscorer_tab <- renderDataTable({
    
    df <- fpl()
    table <- select(df, web_name, goalprob) %>%
      arrange(desc(goalprob))
    
  }, options = list(pageLength = 10, scrolly = 150))
  
  
  # ---------------------- Model likelihood of starting ------------------------
  fpl.1 <- eventReactive(input$start_model, {
    
    # Get player and team data
    data <- getFPLSummary() %>%
      mutate(id = as.numeric(id))
    
    # Geom series
    geomSeries <- function(base, n) {
      base^(1:n)
    }
    
    
    # Define function to get all detailed player data
    details <- function(id) {
      tryCatch({
        # get player details
        x = playerDetailed(id) %>%
          select(kickoff_time, minutes, total_points, transfers_balance, selected)
        
        # Change time to time object
        x <- x %>%
          mutate(kickoff_time = as.Date(kickoff_time),
                 minutes = as.integer(minutes),
                 total_points = as.integer(total_points))
        
        # Set player id
        x$id = id
        
        # Binary transferred in/out
        x = x %>%
          mutate(trans_in = ifelse(transfers_balance < 0, 0, 1))
        
        # Convert to time series
        xt = xts::xts(x, order.by = x$kickoff_time)
        
        # Get weighted moving average of minutes and points
        n <- ifelse(nrow(x) < 5, nrow(x), 5)
        x$avmins <- WMA(x$minutes, n=n, w = geomSeries(1.3, n))
        x$form2 <- WMA(x$total_points, n=n, w = geomSeries(1.3, n))
        
        # Convert back to data frame
        x = as.data.frame(x)
        x = x %>%
          mutate(avmins_lag = lag(avmins, 1)) %>%
          select(id, kickoff_time, minutes, trans_in, total_points, avmins, avmins_lag, form2)
        
        # Return data
        return(x)
      }, error = function(e) {
        return(NA)
      }, finally =  print(paste0(100*round(id/max(data$id),3), '% processed'))
      )
      
    }
    
    # Get all data for modelling
    modeldata <- lapply(data$id, details)
    modeldata2 <- modeldata[!is.na(modeldata)]
    modeldata3 <- do.call(rbind, modeldata2) %>%
      filter(!is.na(avmins)) %>%
      mutate(mins60 = ifelse(minutes >= 60, 1, 0),
             minsany = ifelse(minutes >0, 1, 0),
             trans_in = as.factor(trans_in))
    
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
    
    # Save model
    saveRDS(model, './startprob.rds')
    
    # ------------------------------ Produce probability of starting next game ----------------------------- #
    
    # Get most recent row for each player from detail table
    modelresults <- modeldata3 %>%
      group_by(id) %>%
      mutate(rank = rank(desc(kickoff_time))) %>%
      filter(rank == 1) %>%
      mutate(avmins_lag = avmins) %>%
      select(id, avmins_lag, trans_in, form2)
    
    # Match weighted average minutes and form to fpl data, and rename variables
    fpl.1 <- fpl() %>%
      mutate(id = as.numeric(id)) %>%
      left_join(modelresults, by = 'id')
    
    # Predict next starting probability
    fpl.1$prob60 <- predict(model, newdata = fpl.1, type = 'response')
    
  })
  
})
