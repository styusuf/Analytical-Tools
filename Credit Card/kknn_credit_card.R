#First, load the kknn library (which contains the kknn function) and read in the data
#
library(kknn)
data <- read.table("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

head(data)

# Create a funciton to calculate the accuracy of model with k = X neighbors

check_accuracy = function(X) {
  
  predicted <- rep(0, nrow(data)) # predictions: start with a vector of all 0s
  
  # for each row, build a model that estimates its response based on the other rows
  
  for(i in 1:nrow(data)){
  
  model=kknn(V11~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10, data[-i,], data[i,], k=X, scale=TRUE) #make sure data is scaled
  
  # record whether record is at least 0.5 (round to one) or at less than 0.5 (round to zero)
  
  predicted[i] <- as.integer(fitted(model)+0.50) # round to 0 or 1
  
  }
  
  accuracy = sum(predicted == data[,11]) / nrow(data)
  return(accuracy)
}

# call the function and run function for K = [1:20]

acc = rep(0,20)

for (X in 1:20){
  acc[X] = check_accuracy(X)
}

#report accuracy
acc

