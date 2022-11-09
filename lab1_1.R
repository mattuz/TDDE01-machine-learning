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
table(k_train$fitted.values, train$V65)
table(k_valid$fitted.values, valid$V65)
missclass = function(X, X1) {
  n = length(X)
  return (1 - sum(diag(table(X1, X)))/n)
}
missclass(k_test$fitted.values, as.factor(test$V65))
missclass(train$V65, k_train$fitted.values)
missclass(k_valid$fitted.values, valid$V65)
#Det verkar rimligt. x2 simon tar den

#Exercise 3

