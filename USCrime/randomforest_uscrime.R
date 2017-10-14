## Using random forest model on uscrime data set
# First, read in the data

data <- read.table("uscrime.txt", header = TRUE)

## Crime is the response, the other variables are predictors

install.packages("randomForest")
library("randomForest")

rf.data <- randomForest(Crime~., data = data, mtry = 4, importance = TRUE)
rf.data

# Call:
#   randomForest(formula = Crime ~ ., data = data, mtry = 4, importance = TRUE) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 4
# 
# Mean of squared residuals: 84028.76
# % Var explained: 42.6

## Calculate prediction and sum of squared residuals
yhat.rf <- predict(rf.data)
SSR <- sum((yhat.rf - data$Crime)^2)

## Plot yhat to observed value
plot(yhat.rf, data$Crime)

## Plot the residuals
plot(data$Crime, (yhat.rf - data$Crime))
