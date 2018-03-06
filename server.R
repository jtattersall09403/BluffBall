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
library(Cairo)
options(shiny.usecairo=T, digits = 2)

# Pick up previous data
load('.RData')
strformat <- "function( nRow, aData) {ind = 2; $('td:eq('+ind+')', nRow).html( parseFloat(aData[ind]).toFixed(2) );}"

# Get functions
source('GetFPLData.R')
source('getOdds.R')

# Define server logic
shinyServer(function(input, output) {
  
  # Create goalscorer odds table
  output$goalscorer_tab <- DT::renderDataTable({
    
    # Create table
    fpl %>%
      filter(!is.na(goalprob)) %>%
      arrange(desc(goalprob)) %>%
      mutate(goalprob = round(100*goalprob, 1)) %>%
      select('Player' = web_name, 'Probability of scoring (%)' = goalprob)
    
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  
  # Create clean sheet odds table
  output$cs_tab <- DT::renderDataTable({
    
    # Create table
    fpl %>%
      arrange(desc(cs)) %>%
      mutate(cs = round(100*cs, 1)) %>%
      select('Team' = team, 'Clean sheet probability (%)' = cs) %>%
      unique
    
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # Players with no odds
  output$no_odds <- DT::renderDataTable({
    fpl.3 %>%
      filter((is.na(goalprob) |
             is.na(cs)),
             news == '',
             prob60 > 0.1) %>%
      arrange(desc(total_points)) %>%
      mutate(goalprob = round(100*goalprob, 0),
             cs = round(100*cs, 0)) %>%
      select('Player' = web_name,
             'Team' = team,
             'Position' = pos,
             'Team' = team,
             'Total fpl points' = total_points)
  })
  
  # All point projections
  output$point_projections <- DT::renderDataTable({
    
    # Show which players don't have odds
    showModal(modalDialog(
      title = "Odds missing",
      h2("Players with odds unavailable"),
      p("The bookies haven't published odds yet for the players below. Depending on how good these players are, ",
        "this may affect the quality of BluffBall's recommendations."),
      DT::dataTableOutput('no_odds'),
      easyClose = TRUE,
      footer = modalButton("Dismiss")
    ))
    
    # Create table
    fpl.3 %>%
      arrange(desc(xp)) %>%
      mutate(xp = round(xp, 1),
             goalprob = round(100*goalprob, 0),
             cs = round(100*cs, 0),
             prob60 = round(100*prob60,0),
             now_cost = now_cost/10) %>%
      select('Name' = web_name,
             'Position' = pos,
             'Team' = team,
             'Price' = now_cost,
             'Goal (%)' = goalprob,
             'Clean sheet (%)' = cs,
             'Probability playing 60+ mins (%)' = prob60,
             'Expected points' = xp)
    
  }, options = list(pageLength = 50, scrollX = TRUE))
  
  # ------------------------ Current team -----------------------
  # Get current team
  myteam2 <- eventReactive(input$import_team, {
    
    showNotification('Downloading current team...')
    
    # Get current squad
    #  4880044, 1978879
    teamid <- substr(input$teamid, 34, 40)
    myteam.a <- userPicks(user_id = teamid, gameweek = as.integer(input$gw))
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
    
    myteam2 <- myteam2()
    myteam2 %>%
      mutate(xp = ifelse(xp == max(myteam2$xp), xp *2, xp)) %>%
      mutate(cs = round(100*cs, 1),
             goalprob = round(100*goalprob, 1),
             xp = round(xp, 1),
             price = price/10) %>%
      select("Player" = player_name,
             "Team" = team,
             "Position" = pos,
             "Price" = price,
             "Clean sheet (%)" = cs,
             "Goal (%)" = goalprob,
             "Expected points" = xp)
    
  }, options = list(pageLength = 11, scrollX = TRUE))
  
  # Display team as formation
  output$current_team <- renderPlot({
    myteam2 <- myteam2()
    myteam2 %>%
      ungroup %>%
      mutate(player_name = ifelse(xp == max(myteam2$xp), paste(player_name, '(C)'), player_name),
             xp = ifelse(xp == max(myteam2$xp), xp * 2, xp)) %>%
      teamvis()
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
      mutate(cs = round(100*cs, 1),
             goalprob = round(100*goalprob, 1),
             xp = round(xp, 1),
             price = price/10,
             player_name = ifelse(captain == 1, paste0(player_name, " (C)"), player_name)) %>%
      select("Player" = player_name,
             "Team" = team,
             "Position" = pos,
             "Price" = price,
             "Clean sheet (%)" = cs,
             "Goal (%)" = goalprob,
             "Expected points" = xp)
      
  }, options = list(pageLength = 11, scrollX = TRUE))
  
  # Display team as formation
  output$first_team <- renderPlot({
    myteam3() %>%
      ungroup %>%
      mutate(xp = ifelse(captain == 1, xp * 2, xp)) %>%
      mutate(player_name = ifelse(captain == 1, paste0(player_name, " (C)"), player_name)) %>%
      teamvis()
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
  
  # ------------------------ Transfers -----------------------
  
  # Get single transfers
  single_trans <- eventReactive(input$optimise, {
    
    showNotification('Getting transfers team...', duration = 10)
    
    myteam2 <- myteam2()
    single_trans <- myteam2 %>%
      inner_join(select(fpl.3, id, web_name, pos, now_cost, team, goalprob, xp), by = c('pos' = 'pos')) %>%
      filter(xp.y > xp.x,
             now_cost < price + input$bank*10,
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
             'Difference' = round(xpdiff,1),
             'Price difference' = (now_cost-price)/10) %>%
      select(`Transfer out`, `Original xp`, `Transfer in`, `New xp`, `Difference`, `Price difference`)
    }, options = list(scrollX = TRUE))
  
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
      filter(price.x + input$bank*10 >= price.y,
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
  }, options = list(scrollX = TRUE))
  
  # ------------------------ Making transfers -----------------
  
  # Display current squad so you can select who to transfer out
  output$currentsquad <- renderDT({
    myteam2 <- myteam2()
    myteam2 %>%
      as.data.frame %>%
      mutate(xp = round(xp,2),
             price = price/10) %>%
      select('Player' = player_name,
             'Team' = team,
             'Position' = pos,
             'Price' = price,
             'Xp' = xp)
    
  }, options = list(pageLength = 15, scrollX = TRUE))
  
  # Get reactive bank values
  bankval <- reactive({
    input$bank + sum(unlist(myteam2()[input$currentsquad_rows_selected, 'price']))/10
  })
  
  # Display bank value
  output$bankval <- renderText({
    b <- bankval() - sum(unlist(player_pool()[input$playerpool_rows_selected, 'now_cost']))
    paste0('£', round(b,1), 'm')
  })
  
  # Get ineligible teams
  teams.full <- reactive({
    
    if(!is.null(input$currentsquad_rows_selected)) {
      myteam2()[-input$currentsquad_rows_selected,] %>%
        group_by(team) %>%
        summarise(num = n()) %>%
        filter(num > 2) %>%
        select(team) %>%
        unlist
    } else {
      paste('Empty')
    }
      
    
  })
  
  # Get player pool
  player_pool <- reactive({
    
    # Current squad and bank
    myteam2 <- myteam2()
    teams.full <- teams.full()
    bank <- bankval()
    
    # Get rows selected in transfer_out table
    rws = input$currentsquad_rows_selected
    
    # Filter data
    fpl.3 %>%
      as.data.frame %>%
      mutate(now_cost = now_cost/10) %>%
      filter(pos %in% unlist(myteam2[rws,'pos']),
             !team %in% teams.full,
             now_cost <= bank) %>%
      arrange(desc(xp))
  })
  
  # Display potential transfers in
  output$playerpool <- DT::renderDataTable({
    
    # Display potential transfers in same position
    player_pool() %>%
      mutate(xp = round(xp,2)) %>%
      select('Player' = player_name,
             'Team' = team,
             'Position' = pos,
             'Price' = now_cost,
             'Xp' = xp)
    
  }, options = list(pageLength = 15, scrollX = TRUE))
  
  # Get new team
  newteam <- eventReactive(input$transfers, {
    
    myteam2 <- myteam2()
    player_pool <- player_pool()
    trans_out <- unlist(myteam2[input$currentsquad_rows_selected,'element'])
    trans_in <- unlist(player_pool[input$playerpool_rows_selected, 'id'])
      
    # Ids after transfers
    new_ids <- append(myteam2$element[!myteam2$element %in% trans_out], trans_in)
    
    # Get team after transfers
    newteam <- fpl.3[fpl.3$id %in% new_ids,] %>%
      mutate(position = row_number(), 'player_name'= web_name) %>%
      select(position, player_name, 'price' = now_cost, id, first_name.x, web_name, pos, team, goalprob, xp)
    
    return(newteam)
  })
  
  # Get new first 11
  newfirst11 <- reactive({
    
    # Get new team and bank
    newteam <- newteam()
    myteam2 <- myteam2()
    
    b <- sum(myteam2$price) + input$bank * 10 - sum(newteam$price)
    
    # Check players in each position
    team2 <- newteam %>%
      group_by(pos) %>%
      summarise(num = n()) %>%
      dcast( . ~ pos, value.var = 'num')
    
    # Check all positions present
    legal <- TRUE
    if (sum(points$pos %in% names(team2)) == 4) {
      
      # Check correct number in each position
      if (team2$Goalkeeper != 2 |
          team2$Defender != 5 |
          team2$Midfielder != 5 |
          team2$Forward != 3) {legal <- FALSE}
      
    } else {
      legal <- FALSE 
    }
    
    if(squadlegal(newteam) & b >= 0 & legal == TRUE) {
      
      # Get best team
      newteam %>% rename(element = id) %>% getBestTeam() %>%
        mutate(xp = ifelse(captain == 1, xp * 2, xp),
               player_name = ifelse(captain == 1, paste(player_name, '(C)'), player_name))
      
    } else {
      # Display error message
      showModal(modalDialog(
        title = "Invalid team selected",
        p("The squad you've chosen is too expensive, contains the wrong number of players in each position, or has too many players from a single team.",
          "Select different players to transfer in and try again."),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      
      NULL
    }
  })
  
  # Display new team
  output$newteamplot <- renderPlot({
    
    newfirst11 <- newfirst11()
    
    if (!is.null(newfirst11)) {
      teamvis(newfirst11)
    } else {
      ggplot()
    }

    
  })
  
  # Display new team xp
  output$newxp <- renderText(format(round(sum(newfirst11()$xp), 0), nsmall = 0))
  
  # Display new team cost
  output$newcost <- renderText(paste0('£', sum(newteam()$price)/10, 'm'))
  
  # ------------------------ Dream team -----------------------
  
  # Display dream team in table
  output$dreamteam <- DT::renderDataTable({
    dt.3 %>%
      select(-rank, -captain)
  }, options = list(pageLength = 11, scrollX = TRUE))
  
  # Display dreamteam on pitch
  output$dreamteam_vis <- renderPlot({
    dt.3 %>%
      rename(player_name = web_name) %>%
      teamvis
  })
  
  # Get dreamteam xp
  output$dtxp <- renderText({
    paste(round(sum(dt.3[1:11,'xp']), 0))
  })
  
  # Get dreamteam cost
  output$dtcost <- renderText({
    paste0('£', sum(dt.3[,'now_cost'])/10, ' million')
  })
  
  
})
