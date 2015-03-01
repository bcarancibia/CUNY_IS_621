####Benjamin Arancibia
####IS 621 Business Analytics and Data Mining

#Learning - perceptron

#training.data.set <- read.csv("/users/bcarancibia/CUNY_IS_621/Week2/data/test-public.csv", header = TRUE)
training.data.set <- read.csv("/users/bcarancibia/CUNY_IS_621/Week2/data/coin-training-data.csv",header=T,stringsAsFactors=F)

#test.data.set <- read.csv("/users/bcarancibia/CUNY_IS_621/Week2/data/test-private.csv", header = TRUE)


#### Assign initial values ####

w <- c(0,0,0,0)

training.data.set$id <- rep(1,length(training.data.set$id))
colnames(training.data.set)[1] <- "intercept"

training.data.set$class <- ifelse(training.data.set$answer=="cent",1,-1)

inputs <- as.matrix(training.data.set[,1:4])

#### Functions ####

# Perceptron Update Function



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
y <- c(-1, 1, -1,1)
a <- 0.5
perceptron(inputs, y, w, a, verbose=T)


#2 Design Multivariant Solution

# a) It is reasonable to assume that the dataset is independent. A coin mass, thickness, and diameter are independent from coin to coin, 
# meaning that the variables of one coin do not effect the variables of another coin. 


# b)

# Some variable definitions cent
mu1 <- 2.5 # expected value of mass
mu2 <- 19.05	# expected value of diameter
mu3 <- 1.52 #thickness
sig1 <- 0.0025	# variance of mass
sig2 <- 0.1452 # variance of diameter
sig3 <- 0.0009
rho <- 0.5	# corr(x, y)

# Some additional variables for x-axis and y-axis 
xm <- -3
xp <- 3
ym <- -3
yp <- 3

x <- seq(xm, xp, length= as.integer((xp + abs(xm)) * 10))  # vector series x
y <- seq(ym, yp, length= as.integer((yp + abs(ym)) * 10))  # vector series y

# Core function
bivariate <- function(x,y){
  term1 <- 1 / (2 * pi * sig1 * sig2 * sqrt(1 - rho^2))
  term2 <- (x - mu1)^2 / sig1^2
  term3 <- -(2 * rho * (x - mu1)*(y - mu2))/(sig1 * sig2)
  term4 <- (y - mu2)^2 / sig2^2
  z <- term2 + term3 + term4
  term5 <- term1 * exp((-z / (2 *(1 - rho^2))))
  return (term5)
}

# Computes the density values
z <- outer(x,y,bivariate)

# Plot
persp(x, y, z, main = "Bivariate Normal Distribution",
      sub = bquote(bold(mu[1])==.(mu1)~", "~sigma[1]==.(sig1)~", "~mu[2]==.(mu2)~
                     ", "~sigma[2]==.(sig2)~", "~rho==.(rho)),
      col="orchid2", theta = 55, phi = 30, r = 40, d = 0.1, expand = 0.5,
      ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks=5)