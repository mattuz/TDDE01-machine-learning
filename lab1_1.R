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
install.packages('kknn')
library(kknn)

k_test <- kknn(as.factor(V65)~., train = train, test = test, k = 30, kernel = "rectangular")
k_train <- kknn(as.factor(V65)~., train = train, test = train, k = 30, kernel = "rectangular")
k_valid <- kknn(as.factor(V65)~., train = train, test = valid, k = 30, kernel = "rectangular")

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
prob_eight <- k_train$prob[, 9]
sort(prob_eight)[1:3]
head(sort(prob_eight), 3)
prob_eight <- prob_eight[prob_eight > 0]
print(prob_eight)
ordered_high <- order(prob_eight, decreasing = T)

ordered_high <- ordered_high[1:2]
ordered_low <- order(prob_eight, decreasing = F)
ordered_low <- ordered_low[1:3]
ordered_matrix <- c(ordered_high, ordered_low)
for (i in ordered_matrix){
  my_heatmap <- matrix(as.numeric(train[i,1:64]), nrow=8,ncol=8, byrow = T)

  heatmap(my_heatmap, Colv = NA, Rowv = NA)
}
#M?nga ?r v?ldigt lika vet inte om vi ordnat r?tt. 


#Exercise 4
train_miss_error <- numeric(30)
val_miss_error <- numeric(30)


for (i in 1:30){
  k_valid <- kknn(as.factor(V65)~., train = train, test = valid, k = i, kernel = "rectangular")
  k_train <- kknn(as.factor(V65)~., train = train, test = train, k = i, kernel = "rectangular")
  
  train_miss_error[i] <- missclass(k_train$fitted.values, train$V65)
  val_miss_error[i] <- missclass(k_valid$fitted.values, valid$V65)
  
}

plot(c(1:30), train_miss_error, ylab = "train_miss_error", xlab = "k", col="pink")
points(c(1:30), val_miss_error, col="green")

#Simon t?nker att n?r de rosa cirklarna m?ter de gr?na s? ah rvi best k v?rde.
# 3am 4 have the lowest validation error. The best k value is the lowest validaiton error.
k_test <- kknn(as.factor(V65)~., train = train, test = test, k = 9, kernel = "rectangular")
k_train <- kknn(as.factor(V65)~., train = train, test = train, k = 9, kernel = "rectangular")
k_valid <- kknn(as.factor(V65)~., train = train, test = valid, k = 9, kernel = "rectangular")

missclass(k_test$fitted.values, as.factor(test$V65))
missclass(train$V65, k_train$fitted.values)
missclass(k_valid$fitted.values, valid$V65)

#Exercise 5
cross.entropy <- function(p, phat){
  x <- 0
  for (i in 1:length(p)){
    x <- x + (p[i] * log(phat[i]))
  }
  return(-x)
}

train_miss_error <- as.vector(matrix(0,ncol = 30))
val_miss_error <- as.vector(matrix(0,ncol = 30))

entropy_error <- as.vector(matrix(0,ncol = 30))

for (i in 1:30){
  k_valid <- kknn(as.factor(V65)~., train = train, test = valid, k = i, kernel = "rectangular")
  k_train <- kknn(as.factor(V65)~., train = train, test = train, k = i, kernel = "rectangular")
  
  
  for (j in 0:9){
    cross_val <- valid$V65 == j
    cross_train <- train$V65 == j
    
    TRUE_val <- (which(cross_val, useNames = T))
    prob_val <- k_valid$prob[cross_val, as.character(j)] + 1e-15
    
    TRUE_train <- (which(cross_train, useNames = T))
    prob_train <- k_train$prob[cross_train, as.character(j)] + 1e-15
    
    
    val_miss_error[i] <- cross.entropy(TRUE_val, prob_val)
    train_miss_error[i] <- cross.entropy(TRUE_train, prob_train)
    
    
    
    entropy_error[i] <- abs(val_miss_error[i]-train_miss_error[i])
  }
  
}

print(entropy_error)
plot(c(1:30), entropy_error, ylab = "cross_entropy_error", xlab = "k", col="blue")
which.min(entropy_error)



#Assigment 2


data = read.csv("parkinsons.csv", header = F)
n = dim(data)[1]
set.seed(12345)

id=sample(1:n, floor(n*0.6))
train=data[id,]

id3=setdiff(id1,id2)
test=data[id3,]

