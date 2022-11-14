#Exercise 1

data = read.csv("optdigits.csv", header = F)
n = dim(data)[1]
set.seed(12345)

id=sample(1:n, floor(n*0.5))
train=data[id,]

id1=setdiff(1:n, id)
set.seed(12345)

id2=sample(id1, floor(n*0.25))
valid=data[id2,]

id3=setdiff(id1,id2)
test=data[id3,]

#Exercise 2
library(kknn)
factor = as.factor()
k_test <- kknn(as.factor(V65)~., train = train, test = test, k = 30, kernel = "rectangular")
k_train <- kknn(as.factor(V65)~., train = train, test = train, k = 30, kernel = "rectangular")
k_valid <- kknn(as.factor(V65)~., train = train, test = valid, k = 30, kernel = "rectangular")

summary(k_nearest)
table(k_test$fitted.values, as.factor(test$V65))
train_table = table(k_train$fitted.values, train$V65)
table(k_valid$fitted.values, valid$V65)
missclass = function(X, X1) {
  n = length(X)
  return (1 - sum(diag(table(X1, X)))/n)
}
missclass(k_test$fitted.values, as.factor(test$V65))
missclass(train$V65, k_train$fitted.values)
missclass(k_valid$fitted.values, valid$V65)
#Det verkar rimligt. x2 simon tar den

#Exercise 3.a
#We found that 9, 7 and 4 hardest to classify.
#We found that 0, 2 were easiest to find horunge.

#Exercise 3.b Reshape
simon = do.call(rbind, train[9, ])

heatmap(as.matrix(train_table))
train_table[9, 9]

for (i in 1:10){
  my_heatmap <- matrix(as.numeric(train[i,1:64]), nrow=8,ncol=8, byrow = T)
  
  heatmap(my_heatmap, Colv = NA, Rowv = NA, main = i-1)
}
#Många är väldigt lika vet inte om vi ordnat rätt. 


#Exercise 4
train_miss_error <- numeric(30)
val_miss_error <- numeric(30)


for (i in 1:30){
  k_test <- kknn(as.factor(V65)~., train = train, test = test, k = i, kernel = "rectangular")
  k_train <- kknn(as.factor(V65)~., train = train, test = train, k = i, kernel = "rectangular")
  
  train_miss_error[i] <- missclass(k_train$fitted.values, train$V65)
  val_miss_error[i] <- missclass(k_valid$fitted.values, valid$V65)
  
}

plot(c(1:30), train_miss_error, ylab = "train_miss_error", xlab = "k", col="pink")
points(c(1:30), val_miss_error, col="green")
#Simon tänker att när de rosa cirklarna möter de gröna så ah rvi best k värde.

k_test <- kknn(as.factor(V65)~., train = train, test = test, k = 9, kernel = "rectangular")
k_train <- kknn(as.factor(V65)~., train = train, test = train, k = 9, kernel = "rectangular")
k_valid <- kknn(as.factor(V65)~., train = train, test = valid, k = 9, kernel = "rectangular")

missclass(k_test$fitted.values, as.factor(test$V65))
missclass(train$V65, k_train$fitted.values)
missclass(k_valid$fitted.values, valid$V65)
