# ---------- Recursive dreamteam approach -----------

dreamteam <- function(data, budget = 1000)  {
  dt.1 <- data %>%
    arrange(desc(xp)) %>%
    group_by(pos) %>%
    mutate(rank = row_number()) %>%
    filter((pos=='Goalkeeper' & rank <= 2) |
           (pos=='Defender' & rank <= 5) |
           (pos=='Midfielder' & rank <= 5) |
           (pos=='Forward' & rank <= 3)) %>%
    arrange(pos) %>%
    select(id, pos, team, web_name, now_cost, xp)
  
  #View(dt.1)
  
  # Show total xp and total cost
  sum(dt.1$xp)
  sum(dt.1$now_cost)
  
  # Record bank
  i = 1
  bank <- budget - sum(dt.1$now_cost)
  bank
  
  while (bank < 0 | squadlegal(dt.1) == F) {
    
    #Which teams are ineligible for transfers in?
    t.i <- dt.1 %>%
      group_by(team) %>%
      summarise(num = n()) %>%
      filter(num >= 3) %>%
      arrange(desc(num))
    
    # Find least bad transfer
      t <- dt.1 %>%
        ungroup %>%
        inner_join(select(data, id, web_name, pos, team, now_cost, xp), by = 'pos') %>%
        filter(!id.y %in% dt.1$id) %>%
        mutate(pricediff = now_cost.x - now_cost.y,
               xpdiff = xp.x - xp.y)
      
      # If too many players from one team, transfer them out first. Otherwise, make least bad transfer.
      if (squadlegal(dt.1)) {
        t <- t %>%
          filter(pricediff > 0, !team.y %in% t.i$team) %>%
          arrange(xpdiff) %>%
          slice(1) %>%
          select(id.x, id.y)
      } else {
        t <- t %>%
          filter(team.x == t.i$team[1]) %>%
          #filter(pricediff + bank > 0, !team.y %in% t.i$team) %>%
          filter(!team.y %in% t.i$team) %>%
          arrange(xpdiff) %>%
          slice(1) %>%
          select(id.x, id.y)
      }
  
      
      # Update team
      dt.1 <- dt.1 %>%
        filter(id != t$id.x) %>%
        union(select(filter(data, id == t$id.y), pos, team, id, web_name, now_cost, xp)) %>%
        arrange(pos)
      
      # Update bank
      bank <- budget - sum(dt.1$now_cost)
      
      # Repeat until affordable 
      print(paste0('Iteration ', i, ', Out: ', data$web_name[data$id == t$id.x], ', In: ', data$web_name[data$id == t$id.y], ', Bank:', bank))
      i = i + 1
    
  }
  
  
  # ---------------------- Transfers in without breaking rules --------------------
  
  # Which teams are ineligible for transfers in?
  t.i <- dt.1 %>%
    group_by(team) %>%
    summarise(num = n()) %>%
    filter(num >= 3)
  
  # Check transfers
  t.1 <- dt.1 %>%
    ungroup %>%
    inner_join(select(data, id, team, web_name, pos, now_cost, xp), by = 'pos') %>%
    filter(!id.y %in% dt.1$id,
           !team.y %in% t.i$team) %>%
    mutate(pricediff = now_cost.x - now_cost.y,
           xpdiff = xp.x - xp.y) %>%
    filter(pricediff + bank > 0,
           xpdiff < 0)
  
  while (nrow(t.1) > 0) {
    t <- t.1 %>% arrange(xpdiff) %>%
      slice(1) %>%
      select(id.x, id.y)
    
    # Update team
    dt.1 <- dt.1 %>%
      filter(id != t$id.x) %>%
      union(select(filter(data, id == t$id.y), pos, team, id, web_name, now_cost, xp)) %>%
      arrange(pos)
    
    # Update bank
    bank <- budget - sum(dt.1$now_cost)
    
    # Update teams ineligible for transfers
    t.i <- dt.1 %>%
      group_by(team) %>%
      summarise(num = n()) %>%
      filter(num >= 3)
    
    # Check transfers
    t.1 <- dt.1 %>%
      ungroup %>%
      inner_join(select(data, id, team, web_name, pos, now_cost, xp), by = 'pos') %>%
      filter(!id.y %in% dt.1$id,
             !team.y %in% t.i$team) %>%
      mutate(pricediff = now_cost.x - now_cost.y,
             xpdiff = xp.x - xp.y) %>%
      filter(pricediff + bank > 0,
             xpdiff < 0)
    
    # Repeat until optimised 
    print(paste0('Iteration ', i, ', Out: ', data$web_name[data$id == t$id.x], ', In: ', data$web_name[data$id == t$id.y], ', Bank:', bank))
    i = i + 1  
  }
  
  # ----------- Get best first 11 -------------
  
  dt.2 <- dt.1 %>%
    group_by(pos) %>%
    mutate(rank = row_number()) %>%
    mutate(position = ifelse(((pos == 'Goalkeeper' | pos == 'Forward') & rank > 1) |
                               (pos == 'Midfielder' & rank == 5), 12, as.integer(pos)),
           captain = 0) %>%
    arrange(position) %>%
    rename(element = id)
  
  first11 <- getBestTeam(dt.2) %>% select(-order, -position)
  
  dt.2.1 <- rbind(first11, select(filter(dt.2, !element %in% first11$element), element, pos, team, web_name, now_cost, xp, rank, captain)) %>%
    mutate(web_name = ifelse(captain ==1, paste0(web_name, ' (C)'), web_name),
           xp = ifelse(captain == 1, round(xp * 2,1), round(xp,1)))
  
  # ----------- Finally, sell subs and use extra funds to make one last single transfer --------
  # ----------- Based on what you would do as a human -----------
  
  # Count players per team
  t.i <- dt.2.1 %>%
    group_by(team) %>%
    summarise(num = n()) %>%
    mutate(n = 3-num)
  
  # Transfer subs out for cheapest alternative
  subs <- filter(dt.2.1, !element %in% first11$element) %>%
    inner_join(select(data, id, team, web_name, pos, now_cost, xp), by = 'pos') %>%
    inner_join(t.i, by = c('team.y'='team')) %>%
    mutate(pricediff = now_cost.y - now_cost.x) %>%
    filter(pricediff < 0, # Only interested in cheaper transfers
           xp.y > 0, # Not interested in anyone who won't get any points
           !id %in% dt.2.1$element) %>% # Remove players already in squad
    mutate(xp_rate = xp.y * pricediff) %>%
    # Filter to eligible incoming players according to team rules
    arrange(pricediff, desc(xp.y)) %>%
    group_by(team.y) %>%
    mutate(rank = row_number()) %>%
    filter(team.x == team.y | rank <= n) %>%
    # de-dupe by incoming player
    arrange(pricediff) %>%
    group_by(id) %>%
    mutate(rank = row_number()) %>%
    filter(rank == 1) %>%
    # de-dupe by outgoing player
    arrange(pricediff, desc(xp.y)) %>%
    group_by(element) %>%
    mutate(rank = row_number()) %>%
    filter(rank == 1) %>%
    ungroup %>%
    mutate(captain = 0)
  
  # Record iterations and update bank
  sapply(1:nrow(subs), function(x) {
    bank <<- bank - subs$pricediff[x]
    print(paste0('Iteration ', i, ', Out: ', data$web_name[data$id == subs$element[x]], ', In: ', data$web_name[data$id == subs$id[x]], ', Bank:', bank))
    i <<- i + 1  
  })
  
  # Update squad
  dt.2.2 <- dt.2.1 %>%
    filter(!element %in% subs$element) %>%
    select(-rank) %>%
    as.data.frame %>%
    rbind(select(subs, 'element'=id, pos, 'team' = team.y, 'web_name' = web_name.y, 'now_cost'=now_cost.y, 'xp'=xp.y, captain))
  
  #  ----------------- Find best single transfer you can now afford for your first 11 ------------
  
  # Initialise loop
  t <- data.frame(dummy = 1)
  
  while(nrow(t) > 0) {
    # Record number in each position
    dt.squad <- dt.2.2 %>%
      mutate(dum = c(rep(1,11), rep(2,4))) %>%
      group_by(dum, pos) %>%
      mutate(numpos = n()) %>%
      mutate(minperpos = case_when(pos == 'Goalkeeper' ~ 1,
                                   pos == 'Defender' ~ 3,
                                   pos == 'Midfielder' ~ 2,
                                   pos == 'Forward' ~ 1),
             minpos = ifelse(numpos == minperpos, 1, 0))
    
    # First 11
    dt.f11 <- dt.squad[1:11,]
    
    # Get effective xp - for first team players, this is their xp. For subs, its the xp of the worst first team
    # player they could sub in for.
    dt.squad$minxp <- apply(dt.squad, 1, function(x) min(dt.f11$xp[dt.f11$minpos==0 | dt.f11$pos == x['pos']]))
    dt.2.2$xp_eff <- ifelse(dt.squad$element %in% dt.f11$element, dt.squad$xp, dt.squad$minxp)
    
    # Which teams are ineligible for transfers in?
    t.i <- dt.2.2 %>%
      group_by(team) %>%
      summarise(num = n()) %>%
      filter(num >= 3)
    
    # Check transfers
    t.1 <- dt.2.2[1:11,] %>%
      ungroup %>%
      inner_join(select(data, id, team, web_name, pos, now_cost, xp), by = 'pos') %>%
      filter(!id %in% dt.2.2$element) %>%
      mutate(pricediff = now_cost.x - now_cost.y,
             xpdiff = xp_eff - xp.y) %>%
      filter(pricediff + bank > 0,
             xpdiff < 0) %>%
      arrange(xpdiff)
    
    # Check if best is legal
    if(nrow(t.1) > 0 & !t.1$team.y[1] %in% t.i$team) {
      t <- t.1 %>%
        slice(1) %>%
        select(element, id)
    } else {
      
      # Right. This bit checks what the best transfer you might want to make is.
      # If it's legal, it makes it. If it isn't (because of squad limitations), it checks who you would
      # need to sell to free up a squad spot for it. It looks at who you could bring in for them,
      # and the total xp cost/benefit and cash cost/benefit of doing so. If it's a good trade, it makes it.
      tmp <- t.1 %>%
        mutate(legal = ifelse(team.y %in% t.i$team | team.y==team.x, 0, 1)) %>%
        inner_join(dt.2.2, by = c('team.y'='team')) %>%
        inner_join(select(data, id, team, web_name, pos, now_cost, xp), by = c('pos.y'='pos')) %>%
        filter(!id.y %in% dt.2.2$element,
               !team %in% t.i$team) %>%
        mutate(pricediff = now_cost.x + now_cost.x.x - now_cost.y - now_cost.y.y,
               xpdiff.y = ifelse(legal==1, xpdiff, xpdiff + xp_eff.y - xp.y.y)) %>%
        arrange(xpdiff.y) %>%
        filter(xpdiff.y < 0, pricediff + bank > 0) %>%
        slice(1) %>%
        select(legal, element.x, id.x, element.y, id.y)
      
      if (nrow(tmp) > 0) {
        if (tmp$legal == 1) {
          t <- data.frame(element = tmp$element.x, id = tmp$id.x)
        } else {
          t <- data.frame(element = c(tmp$element.x, tmp$element.y),
                          id = c(tmp$id.x, tmp$id.y))
        }
      } else {
        t <- slice(tmp, 0)
      }
      
    }
    
    # Drop effective xp column
    dt.2.2$xp_eff <- NULL
    
    # Update team
    if(nrow(t) > 0) {
      dt.2.3 <- dt.2.2 %>%
        select(-captain) %>%
        filter(!element %in% t$element) %>%
        union(select(filter(data, id %in% t$id), 'element'=id, pos, team, web_name, now_cost, xp)) %>%
        mutate(position = row_number())
    } else {
      dt.2.3 <- dt.2.2 %>% mutate(position = row_number())
    }
    
    
    # Update bank
    bank <- budget - sum(dt.2.3$now_cost)
    print(paste0('Iteration ', i, ', Out: ', data$web_name[data$id %in% t$element], ', In: ', data$web_name[data$id %in% t$id], ', Bank:', bank))
    
    # Get final dreamteam
    first11 <- getBestTeam(dt.2.3) %>% select(-position, -order)
    dt.2.2 <- rbind(first11, select(filter(mutate(dt.2.3, captain = 0), !element %in% first11$element), element, pos, team, web_name, now_cost, xp, captain))
    
  }
  
  # Update dreamteam object
  dt.3 <- dt.2.2
  
  return(dt.3)
}
