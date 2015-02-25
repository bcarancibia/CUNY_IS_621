####Benjamin Arancibia
####IS 621 Business Analytics and Data Mining

#Learning - perceptron

training.data.set <- read.csv("/users/bcarancibia/CUNY_IS_621/Week2/data/test-public.csv", header = TRUE)
test.data.set <- read.csv("/users/bcarancibia/CUNY_IS_621/Week2/data/test-private.csv", header = TRUE)

#0 = cent 1 = dime in training data set

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
y <- c(1, 0, 1, 0)
w <- c(0, 2, 1)
a <- 0.5
perceptron(x, y, w, a, verbose=T)


#2 Design Multivariant Solution
