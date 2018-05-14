#' Simulate a dataset
#'
#' @param N number of observations
#' @param k optional parameter setting degree to which prison/x is deterministic


simsort <- function(N,k=.75){
  X = runif(N, 0, 3)
  A = sample(c(1:3), size = N, replace = T)
  mu = k - (1-k)*as.numeric( (A==1 & X < 1) | (A==2 & X >=1 & X < 2) | (A==3 & X >=2)  )
  Y = rbinom(N, 1, mu)
  data.frame(y = Y, A= A, x = X)
}

simsort2 <- function(N){
  X = runif(N, 0, 3)
  A = sample(c(1:3), size = N, replace = T, prob = c(.5,.3,.2))
  mu = .75 - .5*as.numeric( (A==1 & X < 1) | (A==2 & X >=1 & X < 2) | (A==3 & X >=2)  )
  Y = rbinom(N, 1, mu)
  data.frame(y = Y, A= A, x = X)
}

simsort3 <- function(N){
  X1 = runif(N, 0, 3); X2 = rnorm(N)
  pi = cbind(.5*expit(X2), .5*expit(X1), 1 - .5*expit(X2) - .5*expit(X1))
  A = apply(pi, 1, function(x) sample(c(1:3), size = 1, prob = x))
  mu = runif(N,.5,1) - .5*as.numeric( (A==1 & X1 < 1) | (A==2 & X1 >=1 & X1 < 2) | (A==3 & X1 >=2)  )
  Y = rbinom(N, 1, mu)
  data.frame(y = Y, A= A, x1 = X1, x2 = X2)
}

simsort4 <- function(N){
  x <- runif(N,0,1)
  A <- c(sapply(x, function(y) rbinom(1,1,prob=c(y/2,1-y/2))))
  #Y <- A
  Y <- as.numeric(A + x > 1.5)
  data.frame(y = Y, A = A, x = x)
}
