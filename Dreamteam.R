# Messing around with dream team
# Research into optimisation options not as positive as hoped. Brute force approach??

### How about recursively applying the double transfer algorithm?
# Get all squad pairs
myteam2$dum <- 1
squad <- myteam2 %>%
  select(dum,  element, first_name.x, player_name, pos, team, price, xp) %>%
  inner_join(myteam2, by = 'dum') %>%
  filter(!(first_name.x.x == first_name.x.y & player_name.x == player_name.y)) %>%
  mutate(pos = paste(pos.x, pos.y, sep="-"),
         price = price.x + price.y,
         xp = xp.x + xp.y) %>%
  select(element.x, element.y,
         first_name.x.x,
         player_name.x,
         first_name.x.y,
         player_name.y,
         pos, price, xp)

# Remove duplicates
squad <- squad[!duplicated(data.frame(t(apply(squad[,c(2,4)], 1, sort)), squad$price)),]

# Join squad pairs to fpl pairs
double_transfers <- inner_join(squad, fplsquad, by = 'pos') %>%
  mutate(xpdiff = xp.y - xp.x) %>%
  filter(price.x + bank >= price.y,
         !id.x %in% myteam2$element,
         !id.y %in% myteam2$element) %>%
  group_by(pos) %>%
  mutate(rank = rank(desc(xpdiff), ties.method = 'first')) %>%
  filter(rank <= 5) %>%
  arrange(desc(xpdiff))

# Remove duplicates
double_transfers <- double_transfers[!duplicated(data.frame(t(apply(double_transfers[,c('second_name.x.x','second_name.x.y')], 1, sort)), double_transfers$price.x)),]

# Choose transfers out and in
trans_out <- c(double_transfers$element.x[1], double_transfers$element.y[1])
trans_in <- c(double_transfers$id.x[1], double_transfers$id.y[1])

# Get team - make sure to update amount in bank
myteam_1trans <- transfers(myteam2, fpl.3, trans_in, trans_out)

