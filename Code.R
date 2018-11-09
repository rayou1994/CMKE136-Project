setwd("E:/Raymond/Documents/CKME 136/flights")
flights <- read.csv(file="flights.csv", header=TRUE)
dim(flights)
# Remove entries that are cancelled and diverted flights.
flights1 <- flights[flights$CANCELLED==0&flights$DIVERTED==0,]
# Creating an indicator variable for flights that were delayed.
flights1$DELAYED <- flights1$AIR_SYSTEM_DELAY+flights1$SECURITY_DELAY+flights1$AIRLINE_DELAY+flights1$LATE_AIRCRAFT_DELAY+flights1$WEATHER_DELAY>0
flights1$DELAYED <- !is.na(flights1$DELAYED)
# Subtracting the delays not related to airline delay from the arrival delay.
flights1[flights1$DELAYED==TRUE,]$ARRIVAL_DELAY <- flights1[flights1$DELAYED==TRUE,]$ARRIVAL_DELAY-flights1[flights1$DELAYED==TRUE,]$AIR_SYSTEM_DELAY-flights1[flights1$DELAYED==TRUE,]$SECURITY_DELAY-flights1[flights1$DELAYED==TRUE,]$LATE_AIRCRAFT_DELAY-flights1[flights1$DELAYED==TRUE,]$WEATHER_DELAY

# So for initial results we will just use a 70/30 split in the data for training and testing.
dim(flights1)
set.seed(100)
sample1 <- sample(1:5714008, 5714008)
traindf <- sample1[1:3999805]
testdf <- sample1[3999806:5714008]

# Fitting the models with the training data. The first model is without the indicator variable whether a flight is delayed or not, while the second model includes it.
lm1 <- lm(ARRIVAL_DELAY~AIRLINE+factor(DAY_OF_WEEK)+DISTANCE, data=flights1, subset=traindf)
summary(lm1)
lm2 <- lm(ARRIVAL_DELAY~AIRLINE+factor(DAY_OF_WEEK)+DISTANCE+DELAYED, data=flights1, subset=traindf)
summary(lm2)

# Function to find the root mean squared error of the predicted values.
RMSE <- function(actual, fitted) {
  sqrt(mean((actual-fitted)^2))
}

# Predicting the testing set using the fitted models
predict1 <- predict(lm1,newdata=flights1[testdf,],type="response")
RMSE(flights1[testdf,]$ARRIVAL_DELAY,predict1)

predict2 <- predict(lm2,newdata=flights1[testdf,],type="response")
RMSE(flights1[testdf,]$ARRIVAL_DELAY,predict2)
