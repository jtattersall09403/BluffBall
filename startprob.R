# ------------------------------ Predicting probability of starting ------------------------------ #

library(dplyr)
library(reshape2)
library(TTR)
library(parallel)

# Get player and team data
data <- fpl %>%
  mutate(id = as.numeric(id))

# Geom series
geomSeries <- function(base, n) {
  base^(1:n)
}


# Define function to get all detailed player data
details <- function(id) {
  tryCatch({
    # get player details
    x = playerDetailed(id) %>%
      select(kickoff_time, minutes, total_points, transfers_balance, selected)
    
    # Change time to time object
    x <- x %>%
      mutate(kickoff_time = as.Date(kickoff_time),
             minutes = as.integer(minutes),
             total_points = as.integer(total_points))
    
    # Set player id
    x$id = id
    
    # Binary transferred in/out
    x = x %>%
      mutate(trans_in = ifelse(transfers_balance < 0, 0, 1))
    
    # Convert to time series
    xt = xts::xts(x, order.by = x$kickoff_time)
    
    # Get weighted moving average of minutes and points
    n <- ifelse(nrow(x) < 5, nrow(x), 5)
    x$avmins <- WMA(x$minutes, n=n, w = geomSeries(1.3, n))
    x$form2 <- WMA(x$total_points, n=n, w = geomSeries(1.3, n))
    
    # Convert back to data frame
    x = as.data.frame(x)
    x = x %>%
      mutate(avmins_lag = lag(avmins, 1)) %>%
      select(id, kickoff_time, minutes, trans_in, total_points, avmins, avmins_lag, form2)
    
    # Return data
    return(x)
  }, error = function(e) {
    return(NA)
  }, finally =  print(paste0(100*round(id/max(data$id),3), '% processed'))
  )
  
}

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

# Include relevant variables and packages
clusterEvalQ(cl, {
   library(jsonlite)
   library(dplyr)
   library(ggplot2)
   library(ggrepel)
   library(engsoccerdata)
   library(devtools)
   library(fplr)
   library(dplyr)
   library(reshape2)
   library(TTR)
   geomSeries <- function(base, n) {
     base^(1:n)
   }
})
  
clusterExport(cl, "data")

# Get all data for modelling. Now only takes around 1 minute!
modeldata <- parLapply(cl, sort(data$id), details)

# Close cluster
stopCluster(cl)

modeldata2 <- modeldata[!is.na(modeldata)]
modeldata3 <- do.call(rbind, modeldata2) %>%
  filter(!is.na(avmins)) %>%
  mutate(mins60 = ifelse(minutes >= 60, 1, 0),
         minsany = ifelse(minutes >0, 1, 0),
         trans_in = as.factor(trans_in))

# ------------------------------ Train models ------------------------------  #

# Quick visualisation
# modeldata3 %>%
#   sample_n(size = 1000) %>%
#   ggplot(aes(x = avmins, y = mins60)) +
#   geom_point(color = 'dodgerblue4', alpha = 0.5)

# Create training and test data
n <- round(nrow(modeldata3)*2/3,0)
train<- modeldata3[1:n,]
test <- modeldata3[n+1:nrow(modeldata3),]

model <- glm(mins60 ~ avmins_lag + trans_in, family=binomial(link='logit'),data=train)

summary(model)

# Evaluate model
fitted.results <- predict(model,newdata=test,type='response')
test$pred <- fitted.results
test %>% select(minutes, pred) %>% View

fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$mins60, na.rm = T)
print(paste('Accuracy',1-misClasificError))

# Save model
# saveRDS(model, './startprob.rds')

# ------------------------------ Produce probability of starting next game ----------------------------- #

# Get most recent row for each player from detail table
modelresults <- modeldata3 %>%
  group_by(id) %>%
  mutate(rank = rank(desc(kickoff_time))) %>%
  filter(rank == 1) %>%
  mutate(avmins_lag = avmins) %>%
  select(id, avmins_lag, trans_in, form2)

# Match weighted average minutes and form to fpl data, and rename variables
# Predict next starting probability. Set to zero if injured.
fpl.1 <- fpl %>%
  mutate(id = as.numeric(id)) %>%
  left_join(modelresults, by = 'id')

fpl.1$prob60 <- ifelse(fpl.1$status == 'a', predict(model, newdata = fpl.1, type = 'response'), 0)
  


  

