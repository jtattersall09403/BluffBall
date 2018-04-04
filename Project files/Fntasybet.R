library(data.table)
library(dplyr)

source('./Project files/Points simulation.R')

# # Set up profit function
# getprofit <- function(index) {
#   
#   # Remove zeroes from index
#   index <- index[index != 0]
#   
#   # Get data
#   x = compdat[index,]
#   p = prizes[index]
#     
#   # Get returns
#   entries = x$entries + 1
#   ret <- lapply(1:length(p), function (i) c(p[[i]] - x$fees[i], rep(-1*x$fees[i], entries[i] - length(p[[i]]))))
#   
#   # All outcomes
#   outcomes = expand.grid(ret)
#   outcomes$profit = rowSums(outcomes)
#   
#   return(outcomes)
#   
# }

# Competitions
comps <- c('slam', 'boss', 'strike', 'coin', 'double', 'scramble')
entries <- c(128, 1, 1, 7, 1, 1)
fees <- c(11, 55, 3.3, 1.1, 11, 5.5)
prizes <- list(c(5500,
                 3937.5,
                 2812.5,
                 2125,
                 1437.5,
                 1175,
                 875,
                 725,
                 625,
                 537.5,
                 rep(400, 5),
                 rep(350, 5),
                 rep(300, 5)),
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
sim <- compsim(400, compdat, teamsimpoints, prizes, weights)

# Get rosums of different combinations
sim2 <- t(sim)

lapply(1:nrow(indices), function(i) {
  ind = indices[i,]
  res = rowSums(dplyr::select(as.data.frame(sim2), ind[ind != 0]))
})

# Get outcomes - deprecated
# outcomes <- apply(indices, 1, function(index) getprofit(index))

# Record these and sort by mean, min, ROI etc
results <-do.call(rbind, lapply(1:length(outcomes), function(i) {
  x = outcomes[[i]]$profit
  ind = indices[i,]
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
View(results)

# ---------------

# Distributions
hist(outcomes[[42]]$profit)
View(outcomes[[42]]$profit)

# Select portfolio and target profit
percentile <- ecdf(sort(outcomes[[2]]$profit))
percentile(0)
percentile(1)
percentile(-25)
percentile(100)
