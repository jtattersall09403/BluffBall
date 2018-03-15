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
           !team.y %in% t.i$team) %>%
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
  inner_join(select(fpl.3, id, team, web_name, pos, now_cost, xp), by = 'pos') %>%
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
  print(paste0('Iteration ', i, ', Out: ', fpl.3$web_name[fpl.3$id == subs$element[x]], ', In: ', fpl.3$web_name[fpl.3$id == subs$id[x]], ', Bank:', bank))
  i <<- i + 1  
})

# Update squad
dt.2.2 <- dt.2.1 %>%
  filter(!element %in% subs$element) %>%
  select(-rank) %>%
  as.data.frame %>%
  rbind(select(subs, 'element'=id, pos, 'team' = team.y, 'web_name' = web_name.y, 'now_cost'=now_cost.y, 'xp'=xp.y, captain))

#  ----------------- Find best single transfer you can now afford for your first 11 ------------
# Which teams are ineligible for transfers in?
# Should really do this iteratively in case selling the subs makes you load sof money
t.i <- dt.2.2 %>%
  group_by(team) %>%
  summarise(num = n()) %>%
  filter(num >= 3)

# Check transfers
t.1 <- dt.2.2[1:11,] %>%
  ungroup %>%
  inner_join(select(fpl.3, id, team, web_name, pos, now_cost, xp), by = 'pos') %>% View
  filter(!id %in% dt.2.2$element,
         !team.y %in% t.i$team) %>%
  mutate(pricediff = now_cost.x - now_cost.y,
         xpdiff = xp.x - xp.y) %>%
  filter(pricediff + bank > 0,
         xpdiff < 0)

# Best
t <- t.1 %>% arrange(xpdiff) %>%
  slice(1) %>%
  select(element, id)

# Update team
if(nrow(t) > 0) {
  dt.2.3 <- dt.2.2 %>%
    select(-captain) %>%
    filter(element != t$element) %>%
    rbind(select(filter(fpl.3, id == t$id), 'element'=id, pos, team, web_name, now_cost, xp)) %>%
    mutate(position = row_number())
} else {
  dt.2.3 <- dt.2.2 %>% mutate(position = row_number())
}


# Update bank
bank <- 1000 - sum(dt.2.3$now_cost)
print(paste0('Iteration ', i, ', Out: ', fpl.3$web_name[fpl.3$id == t$element], ', In: ', fpl.3$web_name[fpl.3$id == t$id], ', Bank:', bank))

# Get final dreamteam
first11 <- getBestTeam(dt.2.3) %>% select(-position, -order)
dt.3 <- rbind(first11, select(filter(mutate(dt.2.3, captain = 0), !element %in% first11$element), element, pos, team, web_name, now_cost, xp, captain))
  

# ---------- Results ----------
View(dt.3)

# Print remaining funds and expected points
bank
sum(dt.3$xp)

# Total xp
dt.3.xp <- sum(dt.3[1:11,'xp'])
dt.3.xp


# mysquad <- dt.3
# # Get all squad pairs
# mysquad$dum <- 1
# squad <- mysquad %>%
#   inner_join(mysquad, by = 'dum') %>%
#   filter(!(web_name.x == web_name.y)) %>%
#   mutate(pos = paste(pos.x, pos.y, sep="-"),
#          price = now_cost.x + now_cost.y,
#          xp = xp.x + xp.y) %>%
#   select(web_name.x,
#          web_name.y,
#          pos, price, xp)
# 
# # Remove duplicates
# squad <- squad[!duplicated(data.frame(t(apply(squad[,c(2,4)], 1, sort)), squad$price)),]
# 
# # Join squad pairs to fpl pairs
# double_transfers <- inner_join(squad, fplsquad, by = 'pos') %>%
#   mutate(xpdiff = xp.y - xp.x) %>%
#   filter(price.x + bank >= price.y,
#          !id.x %in% myteam2$element,
#          !id.y %in% myteam2$element) %>%
#   group_by(pos) %>%
#   mutate(rank = rank(desc(xpdiff), ties.method = 'first')) %>%
#   filter(xpdiff >0) %>%
#   #filter(rank <= 5) %>%
#   arrange(desc(xpdiff))
# 
# # Remove duplicates
# double_transfers <- double_transfers[!duplicated(data.frame(t(apply(double_transfers[,c('second_name.x.x','second_name.x.y')], 1, sort)), double_transfers$price.x)),]
# 
# View(double_transfers)
