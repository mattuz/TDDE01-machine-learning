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
print(prob_eight)
prob_eight = prob_eight()
ordered_high <- order(prob_eight, decreasing = T)
print(ordered_high)
ordered_high <- ordered_high[1:2]


ordered_low <- order(prob_eight, decreasing = F)

ordered_index <- which(prob_eight > 0)
print(ordered_index)
ordered_matrix <- c(ordered_high, ordered_low)
print(ordered_matrix)
print(train$V65)

my_heatmap <- matrix(as.numeric(train[13,1:64]), nrow=8,ncol=8, byrow = T)
heatmap(my_heatmap, Colv = NA, Rowv = NA)
print(as.numeric(train[13, 65]))

for (i in ordered_index){
  my_heatmap <- matrix(as.numeric(train[i,1:64]), nrow=8,ncol=8, byrow = T)
  heatmap(my_heatmap, Colv = NA, Rowv = NA, main =train[i, 65])
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
#Excerice 1

parkin = read.csv("parkinsons.csv", header = T)
parkin_corr <- data.frame(parkin[c(5,7:22)]) #Remove unused voice characteristics
parkin_scaled <- as.data.frame(scale(parkin_corr))

n = dim(parkin_corr)[1]
set.seed(12345)

id=sample(1:n, floor(n*0.6))
train=parkin_scaled[id,]
test=parkin_scaled[-id,]

#Excercise 2

fit = lm(motor_UPDRS ~ ., data = train)
sum = summary(fit)
mean(sum$residuals^2)
print(sum)
# The variables p-values that are higher than 0.05 does not contribute at all.

#Exercise 3

#Loglikelihood
log_likelihood <- function(theta,Y, sigma,train){

  mu = theta[0]
  sigma2 = theta[1] 
  train <- as.matrix(train)
  n <- dim(train)[1]    
  theta<-as.matrix(theta)
  mu <- as.matrix(mu)
  Y <- as.matrix(Y)
  
  
  sum4 <- sum((train%*%-mu)^2) 
  sum1 <- n*log(sigma2)/2
  sum2 <- n*log(2*pi)/2
  sum3 <- sum4/(2*sigma2)
  
  return (-sum1-sum2-sum3)
}


ridge <- function(train, theta, lambda, Y, sigma){
  log_like <- log_likelihood(theta=theta,Y=Y,sigma=sigma,train=train)
  ridge <- -log_lik + lambda*sum(theta[0]^2)
  return(ridge)
  
}                                     


ridgeOpt <- function(lambda, train, motomoto){
  opt <- optim(par = c(0, 0), fn = ridge, lambda = lambda, train = train, Y = motomoto, method = "BFGS")
  return(opt)
} 

DF <- function(lambda, X){
  #From the course formula
  X <- as.matrix(X)
  Xt <- t(X)
  n <- dim(X)[2]
  I <- diag(n)
  P <- X%*%solve((Xt%*%X + lambda%*%I))%*%Xt
  return(t(P))
} 

#Exercise 4
Xtrain<-as.matrix(train[2:17])
motomoto<-as.matrix(train[1])
Xtest=as.matrix(test[2:17])
Ytest<-as.matrix(test[1])

for (lambda in c(1, 100, 1000)){
  ridge_opt = ridgeOpt(lambda,Xtrain, motomoto)
  print(ridge_opt)
  
} 


#Assignment 3.


prime = read.csv("pima-indians-diabetes.csv", header = F)

set.seed(12345)

#ex 1
coloor <- function(x){
  if (x==1){
    c = "red"
  } else{
    c="green"
  } 
  return(c)
}  

#seems to be diff, no obvious correlation between the variables.
#Making a linear regresssion would not provide a meaningful line. 
coloors = sapply(prime$V9, coloor)
plot( prime$V2, prime$V8, xlab = "Plasma", ylab = "Age", main = "doabets", col = coloors)

#ex 2

glm.fits = glm(V9~ V2 + V8, prime, family = "binomial" )
prob=predict(glm.fits, type="response")
pred=ifelse(prob>0.5, 'red','green')

table(pred, prime$V9)

miss <- missclass(pred, prime$V9)

plot(prime$V2, prime$V8,col=pred, xlab = "Plasma glucose levels", ylab = "Age", main = paste("Model predictions for Diabetes \n Missclass_Error", toString(miss), sep=" = ") ) 
#It seems to be an okey classifacation with about 1/4 error but not perfect 
#There seems to be a division when you reach above 150 in plasma-glucose levels. But this threshold seems to lower 
#as you age. An older person is more likely to have diabetes even with lower plasma-glucose levels. 

#ex 3

r=.5
glm.fits = glm(V9~ V2 + V8, prime, family = "binomial" )
hej = glm.fits$coefficients
prob=predict(glm.fits, type="response")
pred=ifelse(prob>0.5, 'red','green')

w9 = hej[1]
w2 = hej[2]
w8 = hej[3]

x8 = c(seq(0,100,0.1))
x2 = (log(-r/(r-1)) - w9 - w8*x8)/w2

plot(prime$V2, prime$V8,col=pred, ylab = "Age", xlab= "Plasma", main = paste("Missclass Error", toString(miss), sep=" = ")) 
lines(x2,x8,col="blue")

#Obviously it very good


#Ex 4

for(r in c(.2,.5, .8)) {
  pred=ifelse(prob>r, 'red','green')
  table(pred, prime$V9)
  
  miss <- missclass(pred, prime$V9)
  
  x2 = (log(-r/(r-1)) - w9 - w8*x8)/w2
  
  plot(prime$V2, prime$V8,col=pred, ylab = "Age", xlab= "Plasma", main = paste("Missclass_Error", toString(miss), sep=" = ")) 
  lines(x2,x8,col="blue")
}
#With r=.2 and r=.8, the missclassification error increased. r=.2 was clearly worse (37% error). 



#Ex 5 . 𝑧𝑧1 = 𝑥𝑥14,𝑧𝑧2 = 𝑥𝑥13𝑥𝑥2, 𝑧𝑧3 = 𝑥𝑥12𝑥𝑥22, 𝑧𝑧4 = 𝑥𝑥1𝑥𝑥23, 𝑧𝑧5 = 𝑥𝑥24 ,

expanded <- prime

expanded$z1 <- expanded$V2 ** 4
expanded$z2 <- expanded$V2 ** 3 * expanded$V8
expanded$z3 <- expanded$V2 ** 2 * expanded$V8 ** 2
expanded$z4 <- expanded$V2 * expanded$V8 ** 3
expanded$z5 <- expanded$V8 ** 4

glm.fits = glm(V9~ V2 + V8 + z1 + z2 + z3 + z4 + z5, expanded, family = "binomial" )


for(r in c(.2,.5, .8)) {
  prob=predict(glm.fits, type="response")
  pred=ifelse(prob>r, 'red','green')
  table(pred, expanded$V9)
  
  miss <- missclass(pred, prime$V9)
  
  x2 = (log(-r/(r-1)) - w9 - w8*x8)/w2
  
  plot(expanded$V2, expanded$V8,col=pred, ylab = "Age", xlab= "Plasma", main = paste("Missclass_Error", toString(miss), sep=" = ")) 
}

#This model has a lower missclass error, so the quality of this model is slightly better than the previous one. 
#The linearity of the model is however gone, since we've added non-linear variables. This causes the 
#plotted colors to look more like a "slice"
