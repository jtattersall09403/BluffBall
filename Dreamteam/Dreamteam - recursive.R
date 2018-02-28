# ---------- Recursive dreamteam approach -----------

dt.1 <- fpl.3 %>%
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
bank <- 1000 - sum(dt.1$now_cost)
bank

while (bank < 0 | squadlegal(dt.1) == F) {
  # Find least bad transfer
  t <- dt.1 %>%
    ungroup %>%
    inner_join(select(fpl.3, id, web_name, pos, now_cost, xp), by = 'pos') %>%
    filter(!id.y %in% dt.1$id) %>%
    mutate(pricediff = now_cost.x - now_cost.y,
           xpdiff = xp.x - xp.y) %>%
    filter(pricediff > 0) %>%
    arrange(xpdiff) %>%
    slice(1) %>%
    select(id.x, id.y)
  
  # Update team
  dt.1 <- dt.1 %>%
    filter(id != t$id.x) %>%
    union(select(filter(fpl.3, id == t$id.y), pos, team, id, web_name, now_cost, xp)) %>%
    arrange(pos)
  
  # Update bank
  bank <- 1000 - sum(dt.1$now_cost)
  
  # Repeat until affordable 
  print(paste0('Iteration ', i, ', Out: ', fpl.3$web_name[fpl.3$id == t$id.x], ', In: ', fpl.3$web_name[fpl.3$id == t$id.y], ', Bank:', bank))
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
  inner_join(select(fpl.3, id, team, web_name, pos, now_cost, xp), by = 'pos') %>%
  filter(!id.y %in% dt.1$id,
         !team.y %in% t.i) %>%
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
    union(select(filter(fpl.3, id == t$id.y), pos, team, id, web_name, now_cost, xp)) %>%
    arrange(pos)
  
  # Update bank
  bank <- 1000 - sum(dt.1$now_cost)
  
  # Update teams ineligible for transfers
  t.i <- dt.1 %>%
    group_by(team) %>%
    summarise(num = n()) %>%
    filter(num >= 3)
  
  # Check transfers
  t.1 <- dt.1 %>%
    ungroup %>%
    inner_join(select(fpl.3, id, team, web_name, pos, now_cost, xp), by = 'pos') %>%
    filter(!id.y %in% dt.1$id,
           !team.y %in% t.i) %>%
    mutate(pricediff = now_cost.x - now_cost.y,
           xpdiff = xp.x - xp.y) %>%
    filter(pricediff + bank > 0,
           xpdiff < 0)
  
  # Repeat until optimised 
  print(paste0('Iteration ', i, ', Out: ', fpl.3$web_name[fpl.3$id == t$id.x], ', In: ', fpl.3$web_name[fpl.3$id == t$id.y], ', Bank:', bank))
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

dt.3 <- rbind(first11, select(filter(dt.2, !element %in% first11$element), element, pos, team, web_name, now_cost, xp, rank, captain)) %>%
  mutate(web_name = ifelse(captain ==1, paste0(web_name, ' (C)'), web_name),
         xp = ifelse(captain == 1, round(xp * 2,1), round(xp,1)))

# ---------- Results ----------
View(dt.3)

# Print remaining funds and expected points
bank
sum(dt.3$xp)

# Total xp
dt.3.xp <- sum(dt.3[1:11,'xp'])
dt.3.xp

