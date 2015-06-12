library(lubridate)
library(caret)

training <- read.csv('train.csv')
training$datetime <- strptime(training$datetime, "%Y-%m-%d %H:%M:%S")
training$datetime <- as.POSIXct(training$datetime, format="%H:%M:%S")
training$weather <- as.factor(training$weather)
training$season <- as.factor(training$season)
training$holiday <- as.factor(training$holiday)
training$workingday <- as.factor(training$workingday)
training$weekday <- as.factor(weekdays(training$datetime))
training$weekday <- factor(training$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
training$hour <- as.integer(substr(training$datetime, 12,13))
training$hour <- as.factor(training$hour)

testing <- read.csv('test.csv')
testing$datetime <- strptime(testing$datetime, "%Y-%m-%d %H:%M:%S")
testing$datetime <- as.POSIXct(testing$datetime, format="%H:%M:%S")
testing$season <- as.factor(testing$season)
testing$weather <- as.factor(testing$weather)
testing$holiday <- as.factor(testing$holiday)
testing$workingday <- as.factor(testing$workingday)
testing$weekday <- as.factor(weekdays(testing$datetime))
testing$weekday <- factor(testing$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
testing$hour <- as.integer(substr(testing$datetime, 12,13))
testing$hour <- as.factor(testing$hour)

traincounter <- numeric(0)
j <- 1
for(i in 2:dim(training)[1]) {
  if(months(training[i,1]) != months(training[i-1,1])) {
    length(traincounter) <- length(traincounter) + 1
    traincounter[j] <- i
    j = j + 1
  }
}
traincounter <- c(1, traincounter, dim(training)[1]+1)

testcounter <- numeric(0)
j <- 1
for(i in 2:dim(testing)[1]) {
  if(months(testing[i,1]) != months(testing[i-1,1])) {
    length(testcounter) <- length(testcounter) + 1
    testcounter[j] <- i
    j = j + 1
  }
}
testcounter <- c(1, testcounter, dim(testing)[1]+1)

pred <- NA
for (k in 1:(length(traincounter)-1)) {
  trainR <- training[traincounter[k]:(traincounter[k+1]-1), -c(10, 12)]
  fitR1 <- train(registered ~ hour + holiday + workingday + weekday + weather + atemp + temp, data=trainR, method="rf", ntree=1001)
  fitR2 <- train(registered ~ hour + holiday + workingday + weekday + weather + atemp + temp, data=trainR, method="glm")
  predR1 <- predict(fitR1, trainR)
  predR2 <- predict(fitR2, trainR)
  predR <- data.frame(predR1, predR2, registered=trainR$registered)
  combfitR <- train(registered ~ ., method="gam", data=predR)
  
  trainC <- training[traincounter[k]:(traincounter[k+1]-1), -c(11, 12)]
  fitC1 <- train(casual ~ hour + holiday + workingday + weekday + weather + atemp + temp, data=trainC, method="rf", ntree=1001)
  fitC2 <- train(casual ~ hour + holiday + workingday + weekday + weather + atemp + temp, data=trainC, method="glm")
  predC1 <- predict(fitR1, trainC)
  predC2 <- predict(fitR2, trainC)
  predC <- data.frame(predC1, predC2, casual=trainC$casual)
  combfitC <- train(casual ~ ., method="gam", data=predC)
  
  testR <- testing[testcounter[k]:(testcounter[k+1]-1),]
  predtestR1 <- predict(fitR1, testR)
  predtestR2 <- predict(fitR2, testR)
  predtestR <- data.frame(predR1=predtestR1, predR2=predtestR2)
  combpredR <- predict(combfitR, predtestR)
  for (l in 1:length(combpredR)) {
    if (combpredR[l] < 0) {
      combpredR[l] <- 0
    }
  }
  
  testC <- testing[testcounter[k]:(testcounter[k+1]-1),]
  predtestC1 <- predict(fitC1, testC)
  predtestC2 <- predict(fitC2, testC)
  predtestC <- data.frame(predC1=predtestC1, predC2=predtestC2)
  combpredC <- predict(combfitC, predtestC)
  for (l in 1:length(combpredC)) {
    if (combpredC[l] < 0) {
      combpredC[l] <- 0
    }
  }
  
  combpred <- combpredR + combpredC
  pred <- c(pred, combpred)
}
pred <- pred[!is.na(pred)]

result <- data.frame(testing$datetime, pred)
names(result) <- c("datetime", "count")
write.csv(result, file = "BikeshareResult.csv",row.names=FALSE)