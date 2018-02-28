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

View(dt.1)

# Show total xp and total cost
sum(dt.1$xp)
sum(dt.1$now_cost)

# Record bank
i = 1
bank <- 1000 - sum(dt.1$now_cost)
bank

while (bank < 0) {
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

View(dt.1)

# Print remaining funds and expected points
bank
sum(dt.1$xp)

# Once over
dt.1 %>%
  ungroup %>%
  inner_join(select(fpl.3, id, web_name, pos, now_cost, xp), by = 'pos') %>%
  filter(!id.y %in% dt.1$id) %>%
  mutate(pricediff = now_cost.x - now_cost.y,
         xpdiff = xp.x - xp.y) %>%
  filter(pricediff + bank > 0,
         xpdiff < 0) %>%
  arrange(xpdiff) %>% View

