library(data.table)
library(dplyr)

# Setup
compdat = data.frame(comps=c('test1', 'test2', 'test3'),
                     entries = c(1000, 5, 10),
                     fees = c(11, 3.3, 5))

prizes <- list(c(5000,3000,1000,500,400,300, rep(100, 100), rep(11, 200)),
               c(75, 50, 30),
               c(20, 10, 5))

# Sim function
pointsim <- function(y, maxent, n, compdat) {
  
  # Your points
  dtp = sapply(1:maxent, function(i) round(rnorm(1, 70 - (i*5), 15),0))
  
  results <- sapply(1:nrow(compdat), function(i) {
    
    # Generate other points
    pts <- round(rnorm(compdat$entries[i], 65, 15),0)
    
    # Get return
    r <- prizes[[i]][rank(-append(dtp,pts), ties.method = "first")[1:maxent]]
    r <- ifelse(is.na(r), 0, r) - compdat$fees[i]
    r <- sum(r)
    r
  })
  
  # Format as data frame
  results = data.frame(t(results))
  
  # Headers
  names(results) <- paste0(compdat$comps, ' x', maxent)
  
  if(y %% 50 == 0) print(paste('Processed', y, 'of', n))
  
  return(results)
}

# Set parameters
n <- 1000
maxent <- 3

# Simulate
results <- do.call(cbind, lapply(1:maxent,function(maxent) 
  do.call(rbind, lapply(1:n, pointsim, maxent = maxent, n = n, compdat = compdat))))

# Order columns
results <- results[ , order(names(results))]

# All possible indices
ncomp <- nrow(compdat)
combn <- lapply(0:(ncomp-1), function(i) c(0, (1+i*ncomp):(ncomp+i*ncomp)))
indices <- expand.grid(combn) %>% slice(2:nrow(.))

# All possible rowsums
profitdat <- lapply(1:nrow(indices), function(i) {
  #index <- indices[i,]*(1:ncol(results))
  index <- unlist(indices[i,])
  index <- index[index != 0]
  r <- rowSums(select(results, index))
  result <- data.frame(comps = paste(names(results)[index], collapse = ', '),
                       min = min(r),
                       lower = quantile(r, .25),
                       median = median(r),
                       upper = quantile(r, .75),
                       exp = mean(r),
                       max = max(r),
                       greater_0 = sum(r > 0)/length(r),
                       xroi = round(-1*mean(r)/min(r),2),
                       stringsAsFactors = FALSE)
  result
})

# Combine
profitdat.2 <- do.call(rbind, profitdat)

# Summary info if you entered all of them
hist(results.2$profit)

