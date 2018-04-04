library(FLSSS)
library(dplyr)
library(microbenchmark)

# data
data <- data.frame(id = 1:15,
                   pos = as.factor(c(1,1,
                           rep(2,5),
                           rep(3,5),
                           rep(4,3)))) %>%
  mutate(now_cost = round(rnorm(15, 66, 15), 1),
         xp = round(now_cost*runif(15, min = 0.05, max = 0.1), 2))

# Check
data

getBestTeam <- function(data, pos = "pos") {
  # formulate multidimensional vector
  mV=as.data.frame(model.matrix(as.formula(paste("~", pos, "-1")), data))
  
  # formulate lbound ubound
  lbound=c(1, 3, 2, 1)
  
  # try if a portfolio with total expect value no less than 75% of the maximum
  # can be found. Change "0.75" manually or in loop to detect the optimal
  ubound=c(1, 5, 5, 3)
  
  # singleTimeLimit and tlimit shall be changed for heavier task. The 5s is
  # merely for passing R package publish requirement
  rst=mmFLknapsack(11, mV, lbound, ubound, totalSolutionNeeded = 1000, tlimit=5, randomizeTargetOrder = FALSE)
  
  # Which has higest xp?
  index <- which.max(lapply(rst, function(x) sum(data[x, 'xp'])))
  rst[[index]]
} 

# Test
tests <- sapply(1:1000, function(i) {
  # data
  data = data.frame(id = 1:15,
                     pos = as.factor(c(1,1,
                                       rep(2,5),
                                       rep(3,5),
                                       rep(4,3)))) %>%
    mutate(now_cost = round(rnorm(15, 66, 15), 1),
           xp = round(now_cost*runif(15, min = 0.05, max = 0.1), 2))
  
  f11 = arrange(data[getBestTeam(data),], pos)
  subs = data[!data$id %in% f11$id,]
  res = subs %>% left_join(f11, by = "pos")
  if (i %% 20 == 0) print(paste(i))
  
  return(sum(res$xp.x > res$xp.y))
  
})

# How many of these have a bench player that should be subbed in?
sum(tests > 0)

# An example
f11 <- data[getBestTeam(data),]
rbind(arrange(f11, pos), data[!data$id %in% f11$id,])

# Check speed
microbenchmark(getBestTeam(data), times = 1000)

# Average speed is 4 milisecons. Significantly faster than current version!
# Appears from tests that it gives at least position-level optimal results.


