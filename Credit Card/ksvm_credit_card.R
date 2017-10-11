install.packages("kernlab")
library("kernlab")
data = read.table(file = "credit_card_data-headers.txt", header = TRUE, sep = "")
# call ksvm. Vanilladot is a simple linear kernel. 
model <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type="C-svc",kernel="vanilladot",C=100,scaled=TRUE)
# calculate a1...am
a <- colSums(data[model@SVindex,1:10] * model@coef[[1]])
coef <- model@coef
# calculate a0
a0 <- sum(a*data[1,1:10]) - model@b
a0
# see what the model predicts
pred <- predict(model,data[,1:10])
pred
# see what fraction of the model’s predictions match the actual classification
acc = sum(pred == data[,11]) / nrow(data)

