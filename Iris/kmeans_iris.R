# install and call packages

install.packages("kmeans")
library("kmeans")

# read data

data <- read.csv("iris.data", header = TRUE, col.names = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width", "Species"))

table(data[,5], data[,5])

#Just by viewing the data, we can see that Petal Length and Width 
#are very similar between the same species. However, length and width 
#vary drastically when comparing across species

#use ggplot to visualize data
library(ggplot2)
ggplot(data, aes(Sepal.Length, Sepal.Width, color=Species)) + geom_point()
ggplot(data, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

# Again, the visualization show that Iris-setosa species 
# are distinctly different from the other two. Iris-versicolor and iris-virsicolor are similar
# and tend to overlap more in their length and width values.

# Now run the kmeans model on the data. Since, we already know that there are 3 clusters (3 species)
# we would expect that k = 3 would be the best solution but we would try k = 2 to k = 5
# and see what our model predictions would be.

#first, let's scale the data

scdata <- data

for  (i in 1:4){
  scdata[,i] <- data[,i]-max(data[,i]) / max(data[,i])-min(data[,i])
}

irisCluster2 = kmeans(scdata[,1:4], 2, nstart = 20)
irisCluster3 = kmeans(scdata[,1:4], 3, nstart = 20)
irisCluster4 = kmeans(scdata[,1:4], 4, nstart = 20)
irisCluster5 = kmeans(scdata[,1:4], 5, nstart = 20)

# calculate distance between data point and its cluster center when k = 2
csum2 <- 0

for(i in 1:nrow(data)){
  csum2 = csum2 + dist(rbind(data[i,1:4], irisCluster2$centers[irisCluster2$cluster[i],]))
}

csum2[1]

# calculate distance between data point and its cluster center when k = 3

csum3 <- 0

for(i in 1:nrow(data)){
  csum3 = csum3 + dist(rbind(data[i,1:4], irisCluster3$centers[irisCluster3$cluster[i],]))
}

csum3[1]

# calculate distance between data point and its cluster center when k = 4

csum4 <- 0

for(i in 1:nrow(data)){
  csum4 = csum4 + dist(rbind(data[i,1:4], irisCluster4$centers[irisCluster4$cluster[i],]))
}

csum4[1]

# calculate distance between data point and its cluster center when k = 5

csum5 <- 0

for(i in 1:nrow(data)){
  csum5 = csum5 + dist(rbind(data[i,1:4], irisCluster5$centers[irisCluster5$cluster[i],]))
}

csum5[1]

## Looking at the distance between data point and its cluster points, there doesn't seem to
## to be a lot of difference. It makes sense that distance would decrease as cluster number increases
## but i didn't expect that the decrease would be this minimal.
