## Install packages for tree
install.packages("tree")
library(tree)

## Load the data set and make tree

data <- read.table("uscrime.txt", header=TRUE)

tree.data <- tree(Crime~., data = data)
summary(tree.data)

# Regression tree:
#   tree(formula = Crime ~ ., data = data)
# Variables actually used in tree construction:
#   [1] "Po1" "Pop" "LF"  "NW" 
# Number of terminal nodes:  7 
# Residual mean deviance:  47390 = 1896000 / 40 
# Distribution of residuals:
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -573.900  -98.300   -1.545    0.000  110.600  490.100 


## We will be branching on the four factors above: 'Po1', 'Pop', 'LF', 'NW'

tree.data$frame

## Breakdown of the branches with their leaves

# var  n        dev      yval splits.cutleft splits.cutright
# 1     Po1 47 6880927.66  905.0851          <7.65           >7.65
# 2     Pop 23  779243.48  669.6087          <22.5           >22.5
# 4      LF 12  243811.00  550.5000        <0.5675         >0.5675
# 8  <leaf>  7   48518.86  466.8571                               
# 9  <leaf>  5   77757.20  667.6000                               
# 5  <leaf> 11  179470.73  799.5455                               
# 3      NW 24 3604162.50 1130.7500          <7.65           >7.65
# 6     Pop 10  557574.90  886.9000          <21.5           >21.5
# 12 <leaf>  5  146390.80 1049.2000                               
# 13 <leaf>  5  147771.20  724.6000                               
# 7     Po1 14 2027224.93 1304.9286          <9.65           >9.65
# 14 <leaf>  6  170828.00 1041.0000                               
# 15 <leaf>  8 1124984.88 1502.8750  

# Plot the regression tree
plot(tree.data)
text(tree.data)


# Use cross-validation on each leaf to get devariance number. Then prune tree to pick the 2 best leaves
cv.data <- cv.tree(tree.data)
plot(cv.data$size, cv.data$dev, type='b')

num <- 2
prune.data <- prune.tree(tree.data, best = num)

# Plot the pruned tress

plot(prune.data)
text(prune.data)
prune.data$where

# Calculate yhat 
yhat <- predict(prune.data)

plot(data$Crime, yhat)
abline(0,1)

