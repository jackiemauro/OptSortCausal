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
