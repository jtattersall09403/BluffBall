
library(dplyr)
library(reshape2)
library(ggplot2)
library(parallel)

# Get functions
source('GetFPLData.R')
source('getOdds.R')

# Full team details
teamdetails <- inner_join(dt.3, fpl.3, by = c('element'='id'))

# Simulate points for each player
teamsim <- lapply(teamdetails$element[1:11], pointssim, teamdetails) %>%
  do.call(cbind, .)

# Get total team points
teamsimpoints <- teamsim %>%
  rowSums() %>%
  as.data.frame %>%
  rename(points = '.')

# Visualise probabilities
teamsimpoints %>%
  ggplot(aes(x=points)) +
  geom_histogram(fill = 'dodgerblue3', color = 'dodgerblue4', bins = 30)

# # Stats
mean(teamsimpoints$points)
percentile = ecdf(teamsimpoints$points)
# 
# Use the below to see likelihood of achieving a certain score or higher
(1-percentile(120))*100

# Get probability of each possible number of points
weights <- as.numeric(table(teamsimpoints)/nrow(teamsimpoints))

# This generates a score randomly according to the distrubtion of total points
sample(unique(teamsimpoints$points), 1, prob=weights, replace = TRUE)

# Function to simulate your rank in all competitions
# compsim <- function(n, compdat, teamsimpoints, prizes, weights) {
# 
#   # Calculate the number of cores
#   no_cores <- detectCores() - 1
# 
#   # Initiate cluster
#   cl <- makeCluster(no_cores)
# 
#   # Export objects
#   clusterEvalQ(cl, {
#     library(dplyr)
#     library(reshape2)
#     library(ggplot2)})
# 
#   clusterExport(cl, "compdat")
#   clusterExport(cl, "teamsimpoints")
#   clusterExport(cl, "prizes")
#   clusterExport(cl, "weights")
# 
#   # Simulation
#   results <- parSapply(cl, 1:n, function(y) {
#     # Your points
#     dtp <- sample(unique(teamsimpoints$points), 1, prob=weights, replace = TRUE)
# 
#     # Points of other entries
#     #i = 1
#     t(sapply(1:nrow(compdat), function(i) {
#       p = prizes
#       x = compdat[i,]
#       pts = sample(unique(teamsimpoints$points), compdat$entries[i], prob=weights, replace = TRUE)
# 
#       # Competition results
#       res = data.frame(pts = pts) %>%
#         mutate(dt = 0) %>%
#         rbind(c(dtp, 1)) %>%
#         mutate(rank = row_number(desc(pts)))
# 
#       # Get rank
#       r = res$rank[res$dt==1]
# 
#       # Return
#       return(c(p[[i]] - x$fees, rep(-1*x$fees, 1 + x$entries - length(p[[i]])))[r])
#     }))
# 
#     #print(paste("Simulation", y, "of", n))
#   })
# 
#   stopCluster(cl)
# 
#   return(results)
# }

compsim <- function(n, compdat, prizes, teamsimpoints, weights) {
  # Simulate
  # n <- 1000
  
  results <- sapply(1:n, function(y) {
    # Your points
    dtp = sample(unique(teamsimpoints$points), 1, prob=weights, replace = TRUE)
    
    results <- sapply(1:nrow(compdat), function(i) {
      
      # Generate other points
      pts <- sample(unique(teamsimpoints$points), compdat$entries[i], prob=weights, replace = TRUE)
      
      # Get return
      r <- prizes[[i]][rank(-append(dtp,pts), ties.method = "first")[1]]
      r <- ifelse(is.na(r), 0, r) - compdat$fees[i]
      r
    })
    
    if(y %% 10 == 0) print(paste('Processed', y, 'of', n))
    
    return(results)
  })
  
  return(results)
}


# # Turn into columns
# results.2 <- as.data.frame(t(results))
# 
# # Then get summary informaiton for different combinations
# results.2$profit = rowSums(results.2)
# 
# # Summary info if you entered all of them
# hist(results.2$profit)
# mean(results.2$profit)
# median(results.2$profit)
# quantile(results.2$profit)

