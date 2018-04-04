library(data.table)
library(dplyr)
library(RSelenium)
library(XML)

source('./Project files/Points simulation v2.R')
source('C:/Users/jackt/Documents/R/win-library/3.4/RSelenium/examples/serverUtils/checkForServer.R')
source('C:/Users/jackt/Documents/R/win-library/3.4/RSelenium/examples/serverUtils/startServer.R')

# Make sure you have server installed
checkForServer()

# use default server 
# Run this in cmd
# cd C:\Users\jackt\Documents\R\win-library\3.4\RSelenium\bin
# java -jar selenium-server-standalone.jar -port 4445

remDr <- remoteDriver(remoteServerAddr = "localhost"
                      , port = 4445
                      , browserName = "firefox"
)

remDr$open

# Competitions
comps <- c('slam', 'strike', 'coin', 'double')
entries <- c(2400, 20, 50, 4)
fees <- c(11, 3.3, 1.1, 11)
prizes <- list(c(10009.52,
                 2051.12,
                 1394.76,
                 1025.56,
                 738.4,
                 492.27,
                 328.18,
                 164.09,
                 123.06,
                 82.04,
                 rep(77,20),
                 rep(33,104),
                 rep(11,329)),
               c(33.75, 18.75, 13.13, 9.38),
               c(17.5,
                 11,
                 7.25,
                 5.38,
                 3.75,
                 2.88,
                 2.25),
               c(22,22))

compdat <- data.frame(comps, entries, fees, stringsAsFactors = FALSE)

# Set parameters
n <- 2000
maxent <- 3

# Simulate
results <- do.call(cbind, lapply(1:maxent,function(maxent) 
  do.call(rbind, lapply(1:n, pointsim, maxent = maxent, n = n, compdat = compdat))))

# Order columns
results <- results[ , order(names(results))]

# All possible indices
ncomp <- nrow(compdat)
combn <- lapply(0:(ncomp-1), function(i) c(0, (1+i*maxent):(maxent+i*maxent)))
indices <- expand.grid(combn) %>% slice(2:nrow(.))

# All possible rowsums
profitdat <- lapply(1:nrow(indices), function(i) {
  #index <- indices[i,]*(1:ncol(results))
  index <- unlist(indices[i,])
  index <- index[index != 0]
  r <- rowSums(select(results, index))
  result <- data.frame(comps = paste(names(results)[index], collapse = ', '),
                       ind = i,
                       min = min(r),
                       lower = round(quantile(r, .25),2),
                       median = median(r),
                       upper = round(quantile(r, .75),2),
                       exp = round(mean(r),2),
                       max = max(r),
                       greater_0 = sum(r > 0)/length(r),
                       xroi = round(-1*mean(r)/min(r),2),
                       stringsAsFactors = FALSE)
  result
})

# Combine
profitdat.2 <- do.call(rbind, profitdat)

# See the results
options(digits=2)
View(arrange(profitdat.2, desc(round(greater_0,2)), desc(min), desc(exp)))

# Experiment with overall rating
res2 <- profitdat.2 %>%
  mutate(scale_exp = as.numeric(scale(exp))*1.2,
         scale_min = as.numeric(scale(min))) %>%
  as.data.frame() #%>%
  filter(greater_0 >= 0.9*max(results$greater_0))

res2$overall = rowMeans(select(res2, scale_exp, scale_min))
View(arrange(res2, desc(overall)))

# Save results
saveRDS(results, './Project files/fantasybet_gw32.rds')

# ---------------

# Full outcome distributions
outcomes <- lapply(1:nrow(indices), function(i) {
  ind = unlist(indices[i,])
  x = rowSums(dplyr::select(as.data.frame(results), ind[ind != 0]))
})

# Distributions
data.frame(profit = outcomes[[81]]) %>% ggplot(aes(profit)) +
  theme_minimal() +
  geom_histogram(colour = 'dodgerblue4', fill = 'dodgerblue', binwidth = 20)

View(outcomes[[132]])
