library(data.table)
library(dplyr)

source('./Project files/Points simulation.R')

# Competitions
comps <- c('slam', 'boss', 'strike', 'coin', 'double', 'scramble')
entries <- c(632, 1, 1, 13, 1, 1)
fees <- c(11, 55, 3.3, 1.1, 11, 5.5)
prizes <- list(c(5100,
                 3600,
                 2425,
                 1800,
                 1237.5,
                 937.5,
                 700,
                 532.5,
                 450,
                 380,
                 rep(280,5),
                 rep(187.5,5),
                 rep(137.5,5),
                 rep(122.5,5),
                 rep(97.5,10),
                 rep(80,10),
                 rep(70,10),
                 rep(62.5,10),
                 rep(55,20)),
               c(300),
               c(75),
               c(25, 15, 10),
               c(50),
               c(15))

compdat <- data.frame(comps, entries, fees, stringsAsFactors = FALSE)

# Get index combinations
indices <- do.call(CJ, replicate(length(comps), 0:1, FALSE))  %>%
  mutate(sel = rowSums(select(., 1:length(comps)))) %>%
  filter(sel > 0) %>%
  select(-sel) %>%
  sweep(MARGIN=2, 1:length(comps), `*`)

# Run simulations. Takes around 0.5 seconds per sim
#sim <- compsim(400, compdat, teamsimpoints, prizes, weights)
sim <- compsim(1000, compdat, prizes, teamsimpoints, weights)

# Get rosums of different combinations
sim2 <- as.data.frame(t(sim))

# Get summary information for different portfolios
results <- do.call(rbind, lapply(1:nrow(indices), function(i) {
  ind = indices[i,]
  x = rowSums(dplyr::select(as.data.frame(sim2), ind[ind != 0]))
  data.frame(comps = paste(comps[ind[ind != 0]], collapse = ', '),
             out_ind = i,
             min = min(x),
             lower = as.numeric(quantile(x, .25)),
             median = median(x),
             upper = as.numeric(quantile(x, .75)),
             max = max(x),
             min_75 = as.numeric(quantile(x, .125)),
             max_75 = as.numeric(quantile(x, .875)),
             greater_0 = round(sum(x > 0)/length(x),2),
             expected = mean(x),
             xroi = round(mean(x)/(-1*min(x)),2),
             stringsAsFactors = FALSE)
}))

# See the results
View(arrange(results, desc(round(greater_0,2)), desc(min), desc(expected)))

# Experiment with overall rating
res2 <- results %>%
  mutate(scale_exp = as.numeric(scale(expected))*1.2,
         scale_min = as.numeric(scale(min))) %>%
  as.data.frame() %>%
  filter(greater_0 >= 0.9*max(results$greater_0))

res2$overall = rowMeans(select(res2, scale_exp, scale_min))
View(arrange(res2, desc(overall)))

# Save results
saveRDS(results, './Project files/fantasybet_gw32.rds')

# ---------------

# Full outcome distributions
outcomes <- lapply(1:nrow(indices), function(i) {
  ind = indices[i,]
  x = rowSums(dplyr::select(as.data.frame(sim2), ind[ind != 0]))
})

# Distributions
data.frame(profit = outcomes[[60]]) %>% ggplot(aes(profit)) +
  theme_minimal() +
  geom_histogram(colour = 'dodgerblue4', fill = 'dodgerblue', binwidth = 20)
