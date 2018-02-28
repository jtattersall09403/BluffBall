# ------------- BluffBall - use bookie's odds to optimise fantasy premier league performance ----------------

library(shiny)
library(rvest)
library(dplyr)
library(fastLink)
library(reshape2)
library(fplr)
library(reshape2)
library(TTR)
library(DT)
library(ggplot2)
library(plotly)
library(ggforce)

source('GetFPLData.R')
source('getOdds.R')

# Pick up previous data
load('.Rdata')

# Define server logic
shinyServer(function(input, output) {
  
  # Create goalscorer odds table
  output$goalscorer_tab <- DT::renderDataTable({
    
    # Create table
    table <- fpl %>%
      mutate(goalprob = format(round(100*goalprob, 1), digits = 2)) %>%
      select('Player' = web_name, 'Probability of scoring (%)' = goalprob) %>%
      arrange(desc(`Probability of scoring (%)`))
    
  }, options = list(pageLength = 10))
  
  
  # Create goalscorer odds table
  output$cs_tab <- DT::renderDataTable({
    
    # Create table
    table <- fpl %>%
      mutate(cs = format(round(100*cs, 1), nsmall = 1)) %>%
      select('Team' = team, 'Clean sheet probability (%)' = cs) %>%
      unique %>%
      arrange(desc(`Clean sheet probability (%)`))
    
  }, options = list(pageLength = 10))
  
  # Get current team
  myteam2 <- eventReactive(input$import_team, {
    # ------------------------ Current team -----------------------
    
    showNotification('Downloading current team...')
    
    # Get current squad
    #  4880044, 1978879
    myteam.a <- userPicks(user_id = input$teamid, gameweek = as.integer(input$gw))
    myteam <- myteam.a %>%
      dplyr::select(position, player_name, price, element) %>%
      mutate(price = price * 10)
    
    # Link expected points
    myteam2 <- myteam %>%
      left_join(select(fpl.3, id, first_name.x, web_name, pos, team, cs, goalprob, xp), by = c('element'='id')) %>%
      group_by(player_name) %>%
      mutate(rank = rank(desc(xp), ties.method='first')) %>%
      mutate(position = as.numeric(position),
             price = as.numeric(price)) %>%
      filter(rank == 1) %>%
      select(-rank)
    
    return(myteam2)
    
  })
  
  # Display team table
  output$myteam_tab <- DT::renderDataTable({
    
    myteam2() %>%
      mutate(cs = format(round(100*cs, 1), nsmall = 1),
             goalprob = format(round(100*goalprob, 1), nsmall = 1),
             xp = format(round(xp, 1), nsmall = 1)) %>%
      select("Player" = player_name,
             "Team" = team,
             "Position" = pos,
             "Price" = price,
             "Clean sheet (%)" = cs,
             "Goal (%)" = goalprob,
             "Expected points" = xp)
    
  }, options = list(pageLength = 11))
  
  # Display team as formation
  output$current_team <- renderPlot({
    teamvis(myteam2())
  })
  
  # Get best possible team
  myteam3 <- reactive({
    
    showNotification('Optimising team...')
    
    # Optimise team
    myteam3 <- getBestTeam(myteam2())
    
  })
  
  # View optimised team
  output$myteam3_tab <- DT::renderDataTable({
    myteam3() %>%
      ungroup %>%
      mutate(xp = ifelse(captain == 1, xp * 2, xp)) %>%
      mutate(cs = format(round(100*cs, 1), nsmall = 1),
             goalprob = format(round(100*goalprob, 1), nsmall = 1),
             xp = format(round(xp, 1), nsmall = 1),
             player_name = ifelse(captain == 1, paste0(player_name, " (C)"), player_name)) %>%
      select("Player" = player_name,
             "Team" = team,
             "Position" = pos,
             "Price" = price,
             "Clean sheet (%)" = cs,
             "Goal (%)" = goalprob,
             "Expected points" = xp)
      
  }, options = list(pageLength = 11))
  
  # Display team as formation
  output$first_team <- renderPlot({
    teamvis(myteam3())
  })
  
  # Get current team total xp
  output$totxp_1 <- renderText({
    
    # Total xp
    myteam2 <- myteam2()
    totxp <- myteam2[1:11,] %>% mutate(xp = ifelse(xp == max(myteam2$xp), xp *2, xp)) %>% ungroup %>% summarise(xp = sum(xp)) %>% unlist %>% round(0)
    return(paste(totxp))
    
  })
  
  # Get best team total xp
  output$totxp_2 <- renderText({
    
    # Total xp
    totxp <- myteam3() %>% mutate(xp = ifelse(captain == 1, xp *2, xp)) %>% ungroup %>% summarise(xp = sum(xp)) %>% unlist %>% round(0)
    return(paste(totxp))
    
  })
  
  
  # Get single transfers
  single_trans <- eventReactive(input$optimise, {
    
    myteam2 <- myteam2()
    single_trans <- myteam2 %>%
      inner_join(select(fpl.3, id, web_name, pos, now_cost, team, goalprob, xp), by = c('pos' = 'pos')) %>%
      filter(xp.y > xp.x,
             now_cost < price + input$bank,
             !id %in% myteam2$element) %>%
      mutate(xpdiff = xp.y - xp.x - ifelse(input$ft > 0, 0, 4)) %>%
      arrange(desc(xpdiff))
    
    return(single_trans)
  })
  
  # Display single transfers
  output$single_trans <- DT::renderDataTable({
    
    df <-  single_trans()
    
    df %>% 
      group_by(player_name) %>%
      mutate(rank = row_number()) %>%
      filter(rank <= 3) %>%
      select(-rank) %>%
      mutate('Transfer out' = player_name,
             'Original xp' = round(xp.x,1),
             'Transfer in' = web_name.y,
             'New xp' = round(xp.y,1),
             'Difference' = round(xpdiff,1)) %>%
      select(`Transfer out`, `Original xp`, `Transfer in`, `New xp`, `Difference`)
    })
  
  # Get double transfers
  double_trans <- eventReactive(input$optimise, {
    
    myteam2 <- myteam2()
    
    # Get all squad pairs
    myteam2$dum <- 1
    squad <- myteam2 %>%
      select(dum,  first_name.x, player_name, pos, team, price, xp) %>%
      inner_join(myteam2, by = 'dum') %>%
      filter(!(first_name.x.x == first_name.x.y & player_name.x == player_name.y)) %>%
      mutate(pos = paste(pos.x, pos.y, sep="-"),
             price = price.x + price.y,
             xp = xp.x + xp.y) %>%
      select(first_name.x.x,
             player_name.x,
             first_name.x.y,
             player_name.y,
             pos, price, xp)
    
    # Remove duplicates
    squad <- squad[!duplicated(data.frame(t(apply(squad[,c(2,4)], 1, sort)), squad$price)),]
    
    # Join squad pairs to fpl pairs
    double_transfers <- inner_join(squad, fplsquad, by = 'pos') %>%
      mutate(xpdiff = xp.y - xp.x - (8-(4*(input$ft)))) %>%
      filter(price.x + bank >= price.y,
             !id.x %in% myteam2$element,
             !id.y %in% myteam2$element) %>%
      group_by(pos) %>%
      mutate(rank = rank(desc(xpdiff), ties.method = 'first')) %>%
      filter(rank <= 5) %>%
      arrange(desc(xpdiff))
    
    # Remove duplicates
    double_transfers <- double_transfers[!duplicated(data.frame(t(apply(double_transfers[,c('second_name.x.x','second_name.x.y')], 1, sort)), double_transfers$price.x)),]
    
    return(double_transfers)
  })
  
  # Display single transfers
  output$double_trans <- DT::renderDataTable({
    
    df <-  double_trans()
    
    df %>% 
      group_by(player_name.x, player_name.y) %>%
      mutate(rank = row_number()) %>%
      ungroup %>%
      filter(rank <= 3) %>%
      select(-rank) %>%
      mutate('Transfer out' = paste(player_name.x, player_name.y, sep = ', '),
             'Original xp' = round(xp.x,1),
             'Transfer in' = paste(second_name.x.x, second_name.x.y, sep = ', '),
             'New xp' = round(xp.y,1),
             'Difference' = round(xpdiff,1)) %>%
      select(`Transfer out`, `Original xp`, `Transfer in`, `New xp`, `Difference`)
  })
  
  
})
