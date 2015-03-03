####Benjamin Arancibia
####IS 621 Business Analytics and Data Mining

#Learning - perceptron

public.data.set <- read.csv("/users/bcarancibia/CUNY_IS_621/Week2/data/test-public.csv", header = TRUE)
training.data.set <- read.csv("/users/bcarancibia/CUNY_IS_621/Week2/data/coin-training-data.csv",header=T,stringsAsFactors=F)

private.data.set <- read.csv("/users/bcarancibia/CUNY_IS_621/Week2/data/test-private.csv", header = TRUE)


#### Assign initial values ####

w <- c(0,0,0,0)

training.data.set$id <- rep(1,length(training.data.set$id))
colnames(training.data.set)[1] <- "intercept"

training.data.set$class <- ifelse(training.data.set$answer=="cent",1,-1)

inputs <- as.matrix(training.data.set[,1:4])
inputs2 <- as.matrix(public.data.set[,1:4])
inputs3 <- as.matrix(private.data.set[,1:4])
inputs4 <- as.matrix(public.data.set[,2:4])

#### Functions ####

# Perceptron Update Function

eps <- sqrt(sum((beta0-beta)^2))/sqrt(sum(beta0^2))

# Perceptron Assignment Function



# Perceptron Check Function (return count)
perceptron.check <- function(){
  
}


#### Implementation ####

while(perceptron.check()!=0){
  # Select the first observation and update weights
  w <- perceptron.update()
  
  
}


# Args:
#   x: an m by n matrix representing the data under study
#   y: objective vector of m elements
#   w: initial guess for the weight vector, n elements
#   a: learning rate, should be in (0,1]
#   verbose: echo information w at each iteration (default: False)
perceptron <- function(x, y, w, a, verbose=F) {
  hyp <- function(w, x) {
    p <- w %*% x
    sign(p - abs(p)) + 1
  }
  
  iter <- function(x, y, w, a) {
    w + a * (y - hyp(w, x)) * x
  }
  
  wold <- matrix(w, nrow=1)
  nrows <- dim(x)[1]
  done <- FALSE
  iteration <- 0
  
  while (! done) {
    if (verbose) {
      print(paste("Iteration", iteration, sep=" "), quote=F)
      print(wold)
    }
    iteration <- iteration + 1
    count <- 0
    for (i in 1:nrows) {
      wnew = iter(x[i,], y[i], wold, a)
      if (all(wnew == wold)) {
        count <- count + 1
      }
      wold <- wnew
    }
    if (count == nrows) {
      done <- TRUE
    }
  }
  wnew
}

# Example
x <- matrix(c(1, 2, 1, 2, 2, 1, 4, 1, 1, 5, 1, 1), nrow=4, byrow=T)
y <- c(-1,1,1,1)
w <- c(0,0,0,0)
a <- 1
perceptron(inputs, y, w, a, verbose=T)



#2 Design Multivariant Solution

# a) It is reasonable to assume that the dataset is independent. A coin mass, thickness, and diameter are independent from coin to coin, 
# meaning that the variables of one coin do not effect the variables of another coin. 


# b)
#cent
mu <- c(2.5, 19.05, 1.52)
Sigma <- matrix(c(0.0025, 0, 0, 0, 0.1452, 0, 0, 0, 0.0009), ncol=3) # covariance matrix of X

plot1 <- rmvnorm(n=500, mean=c(2.5, 19.05, 1.52), sigma=Sigma)
colMeans(plot1)
var(plot1)

plot(plot1)

#dime
mu2 <- c(2.268, 17.91, 1.35)
Sigma2 <- matrix(c(0.0021, 0, 0, 0, 0.1283 , 0, 0, 0, 0.0007), ncol=3) # covariance matrix of X

plot2 <- rmvnorm(n=500, mean=c(2.268, 17.91, 1.35), sigma=Sigma2)
colMeans(plot2)
var(plot2)

plot(plot2)


#c)
df <- public.data.set[-1]
df$answer <- factor(df$answer, levels=c(0,1), labels=c("cent", "dime"))

train <- sample(nrow(df), 0.7*nrow(df))
df.train <- df[train,]
df.validate <- df[-train,]
table(df.train$answer)
table(df.validate$answer)

fit.logit <- glm(answer~. , data=df.train, family=binomial())
summary(fit.logit)

prob <- predict(fit.logit, df.validate, type="response")
logit.pred <- factor(prob > .5, levels=c(FALSE, TRUE), labels=c("cent", "dime"))
logit.perf <- table(df.validate$answer, logit.pred,dnn=c("Actual", "Predicted"))

logit.perf

#My regression was not so great at predicting. 




#d)
#private
df2 <- private.data.set[-1]
df2$answer <- factor(df2$answer, levels=c(0,1), labels=c("cent", "dime"))

train2 <- sample(nrow(df2), 0.7*nrow(df2))
df2.train <- df[train2,]
df2.validate <- df[-train2,]
table(df2.train$answer)
table(df2.validate$answer)

fit.logit2 <- glm(answer~. , data=df2.train, family=binomial())
summary(fit.logit2)

prob2 <- predict(fit.logit2, df2.validate, type="response")
logit.pred2 <- factor(prob > .5, levels=c(FALSE, TRUE), labels=c("cent", "dime"))
logit.perf2 <- table(df2.validate$answer, logit.pred,dnn=c("Actual", "Predicted"))

logit.perf2

