# attempt to set up a simulation for the approximate constrained procedure
# not going to attempt to set up something where i know the exact answer
# will instead output true mu,pi terms, pass through optimizer and add noise
# need to figure out mu and pi expectations though
# need to add unconstrained optimization, compare using estimated and true f

rm(list = ls())
library(ggplot2)
library(matlabr)

# helper functions
# should i be adding an error term to f to get f.hat rather than estimating on noisy mu's?
simFunc <- function(N = 5000){
  # constraint should not be binding
  # psi = 0.25 is the true minimal E(Y^a)
  meanx = c(0,0,0)
  x = matrix(unlist(lapply(meanx, function(x) rnorm(N,x,1))), nrow = N, byrow =T)

  # probability assigned to prison a
  pi1 = .5*as.numeric(x[,1]>1) + .2
  pi2 = .5*as.numeric(x[,1]< -1) + .2
  pi = cbind(pi1, pi2, 1-pi1-pi2)
  a = apply(pi, 1, function(x) sample(c(1:3), size = 1, prob = x))

  # probability prison a is your best prison
  pi.star1 = .5*as.numeric(x[,2]>1) + .2
  pi.star2 = .5*as.numeric(x[,2]< -1) + .2
  pi.star = cbind(pi.star1, pi.star2, 1-pi.star1-pi.star2)
  a.star = apply(pi.star, 1, function(x) sample(c(1:3), size = 1, prob = x))

  # outcome
  mu.mat = sapply(c(1,2,3), function(k) .75 - .5*(k==a.star))
  mu = runif(N,.5,1) - .5*(a == a.star)
  y = rbinom(N, 1, mu)
  return(list( data = data.frame(y,a,x),true.mumat = mu.mat, true.pimat=pi, a.star = a.star))
}
simFunc2 <- function(N = 5000){
  # constraint should be binding
  # psi = 0.25 is the true minimal E(Y^a)
  meanx = c(0,0,0)
  x = matrix(unlist(lapply(meanx, function(x) rnorm(N,x,1))), nrow = N, byrow =T)

  # probability assigned to prison a
  pi1 = .5*as.numeric(x[,1]>1) + .2
  pi2 = .5*as.numeric(x[,1]< -1) + .2
  pi = cbind(pi1, pi2, 1-pi1-pi2)
  a = apply(pi, 1, function(x) sample(c(1:3), size = 1, prob = x))

  # probability prison a is your best prison
  # in this case, prison 1 best for many more individuals than can be assigned there
  pi.star1 = .5*as.numeric(x[,2]>0) + .2
  pi.star2 = .5*as.numeric(x[,2]< -1) + .2
  pi.star = cbind(pi.star1, pi.star2, 1-pi.star1-pi.star2)
  a.star = apply(pi.star, 1, function(x) sample(c(1:3), size = 1, prob = x))

  # outcome
  mu.mat = sapply(c(1,2,3), function(k) .75 - .5*(k==a.star))
  mu = runif(N,.5,1) - .5*(a == a.star)
  y = rbinom(N, 1, mu)
  return(list( data = data.frame(y,a,x),true.mumat = mu.mat, true.pimat=pi, a.star = a.star))
}
simFunc3 <- function(N = 5000){
  # constraint should be binding
  # have less drastic difference in being at ideal vs not
  meanx = c(0,0,0)
  x = matrix(unlist(lapply(meanx, function(x) rnorm(N,x,1))), nrow = N, byrow =T)

  # probability assigned to prison a
  pi1 = .5*as.numeric(x[,1]>1) + .2
  pi2 = .5*as.numeric(x[,1]< -1) + .2
  pi = cbind(pi1, pi2, 1-pi1-pi2)
  a = apply(pi, 1, function(x) sample(c(1:3), size = 1, prob = x))

  # probability prison a is your best prison
  # in this case, prison 1 best for many more individuals than can be assigned there
  pi.star1 = .5*as.numeric(x[,2]>0) + .2
  pi.star2 = .5*as.numeric(x[,2]< -.5) + .2
  pi.star = cbind(pi.star1, pi.star2, 1-pi.star1-pi.star2)
  a.star = apply(pi.star, 1, function(x) sample(c(1:3), size = 1, prob = x))

  # outcome
  mu.mat = sapply(c(1,2,3), function(k) .5 - .2*(k==a.star))
  mu = runif(N,.25,.75) - .2*(a == a.star)
  y = rbinom(N, 1, mu)
  return(list( data = data.frame(y,a,x),true.mumat = mu.mat, true.pimat=pi, a.star = a.star))
}
simFunc4 <- function(N = 5000){
  # constraint should be binding
  # have less drastic difference in being at ideal vs not
  meanx = c(0,0,0)
  x = matrix(unlist(lapply(meanx, function(x) rnorm(N,x,1))), nrow = N, byrow =T)

  # probability assigned to prison a
  pi1 = .5*as.numeric(x[,1]>1) + .2
  pi2 = .5*as.numeric(x[,1]< -1) + .2
  pi = cbind(pi1, pi2, 1-pi1-pi2)
  a = apply(pi, 1, function(x) sample(c(1:3), size = 1, prob = x))

  # probability prison a is your best prison
  # in this case, prison 1 best for many more individuals than can be assigned there
  pi.star1 = .5*as.numeric(x[,2]>1) + .2
  pi.star2 = .5*as.numeric(x[,2]< -1) + .2
  pi.star = cbind(pi.star1, pi.star2, 1-pi.star1-pi.star2)
  a.star = apply(pi.star, 1, function(x) sample(c(1:3), size = 1, prob = x))

  # outcome
  mu.mat = sapply(c(1,2,3), function(k) .5 - .2*(k==a.star))
  mu = runif(N,.25,.75) - .2*(a == a.star)
  y = rbinom(N, 1, mu)
  return(list( data = data.frame(y,a,x),true.mumat = mu.mat, true.pimat=pi, a.star = a.star))
}
simFunc5 <- function(N = 5000){
  # constraint should be binding
  # have less drastic difference in being at ideal vs not
  meanx = c(0,0)
  x = matrix(unlist(lapply(meanx, function(x) rnorm(N,x,1))), nrow = N, byrow =T)

  # probability assigned to prison a
  pi1 = .5*as.numeric(x[,1]>1) + .2
  pi2 = .5*as.numeric(x[,1]< -1) + .2
  pi = cbind(pi1, pi2, 1-pi1-pi2)
  a = apply(pi, 1, function(x) sample(c(1:3), size = 1, prob = x))

  # probability prison a is your best prison
  # in this case, prison 1 best for many more individuals than can be assigned there
  pi.star1 = .5*as.numeric(x[,2]>0) + .2
  pi.star2 = .5*as.numeric(x[,2]< -0.5) + .2
  pi.star = cbind(pi.star1, pi.star2, 1-pi.star1-pi.star2)
  a.star = apply(pi.star, 1, function(x) sample(c(1:3), size = 1, prob = x))

  # outcome
  mu.mat = sapply(c(1,2,3), function(k) .75 - .1*(k==a.star))
  mu = runif(N,.6,.9) - .1*(a == a.star)
  y = rbinom(N, 1, mu)
  return(list( data = data.frame(y,a,x),true.mumat = mu.mat, true.pimat=pi, a.star = a.star))
}
if.func <- function(D){
  df = D$df; mu.pred = D$mu.pred; fhat = D$fhat; Avals = D$Avals; phat = D$phat
  mu.hat = diag(mu.pred %*% t(model.matrix(~Avals[fhat]-1))) + (rnorm(N,e.mean,1)/B)
  ratM = (as.numeric(df$A == fhat)/phat) + (rnorm(N,e.mean,1)/B)
  ratM * (df$y - mu.hat) + mu.hat
}
get.f <- function(D){
  constr <- as.numeric(round(table(sort(D$data$a))*1.05))
  Avals <- sort(unique(D$data$a))
  ### send muhat matrix to matlab, constraint for constrained optimization
  write.csv(constr, "~jacquelinemauro/Dropbox/sorter/approxSim/optConstr.csv")
  write.csv(D$true.mumat, "~jacquelinemauro/Dropbox/sorter/approxSim/optMuhat.csv")
  run_matlab_script("~jacquelinemauro/Dropbox/sorter/approxSim/approxConstr.m")

  ### read assignment vector from matlab
  f.mat <- read.csv("~jacquelinemauro/Dropbox/sorter/approxSim/optfhat.csv", header = F)
  f.con <- Avals[apply(f.mat,1,which.max)]
  f.con
}
get.psi <- function(D){
  dat = D$data; mumat = D$true.mumat; pimat = D$true.pimat; f.vec = D$f.vec
  Avals = sort(unique(dat$a))
  temp = sapply(Avals, function(x) as.numeric(Avals[f.vec] == x))
  mu.hat = diag(mumat %*% t(temp))
  #pi.hat = diag(pimat %*% t(temp))
  #ifs = (as.numeric(dat$a == f.vec)/pi.hat) * (dat$y - mu.hat) + mu.hat
  #return(list(effect = mean(ifs), sd = sd(ifs)/sqrt(length(f.vec))))
  return(list(effect = mean(mu.hat), sd = sd(mu.hat)/sqrt(length(f.vec))))
}
add.error <- function(D, e.mean){
  N = dim(D$true.mumat)[1]; B = N^(1/D$K); num.a = length(unique(D$data$a))
  errs <- matrix(rnorm(N*2*num.a,e.mean,1)/B, ncol = 2*num.a)
  muhat <- D$true.mumat + errs[,1:num.a]
  inv.pihat <- (1/D$true.pimat) + errs[,(num.a+1):(2*num.a)]
  return(list(muhat = muhat, inv.pihat = inv.pihat))
}
add.error2 <- function(D, e.mean){
  N = dim(D$true.mumat)[1]; B = N^(1/D$K); num.a = length(unique(D$data$a))
  errs <- matrix(rnorm(N*3*num.a,e.mean,1)/B, ncol = 3*num.a)
  muhat <- D$true.mumat + errs[,1:num.a]
  inv.pihat <- (1/D$true.pimat) + errs[,(num.a+1):(2*num.a)]
  muhat2 <- D$true.mumat + errs[,(2*num.a +1):dim(errs)[2]]
  return(list(muhat = muhat, inv.pihat = inv.pihat, muhat2 = muhat2))
}
get.fhat <- function(D){
  constr <- as.numeric(round(table(sort(D$data$a))*1.05))
  Avals <- sort(unique(D$data$a))

  ### send muhat matrix to matlab, constraint for constrained optimization
  write.csv(constr, "~jacquelinemauro/Dropbox/sorter/approxSim/optConstr.csv")
  write.csv(D$muhat, "~jacquelinemauro/Dropbox/sorter/approxSim/optMuhat.csv")
  run_matlab_script("~jacquelinemauro/Dropbox/sorter/approxSim/approxConstr.m")

  ### read assignment vector from matlab
  f.mat <- read.csv("~jacquelinemauro/Dropbox/sorter/approxSim/optfhat.csv", header = F)
  f.con <- Avals[apply(f.mat,1,which.max)]
  f.con
}
get.psihat <- function(D){
  dat = D$data; mumat = D$muhat; pimat = D$inv.pihat; f.vec = D$fhat
  Avals = sort(unique(dat$a))
  temp = sapply(Avals, function(x) as.numeric(Avals[f.vec] == x))
  mu.hat = diag(mumat %*% t(temp))
  pi.hat = diag(pimat %*% t(temp))
  ifs = (as.numeric(dat$a == f.vec)*pi.hat) * (dat$y - mu.hat) + mu.hat
  return(list(ifs = ifs, est = mean(ifs), sd = sd(ifs)/sqrt(length(f.vec))))
}
get.psihatU <- function(D, type = 'double'){
  dat = D$data; mumat = D$muhat; pimat = D$inv.pihat
  if(type == 'double'){f.vec = D$muhat.min}
  else if(type == 'triple'){f.vec = D$muhat.min2}
  Avals = sort(unique(dat$a))
  temp = sapply(Avals, function(x) as.numeric(Avals[f.vec] == x))
  mu.hat = diag(mumat %*% t(temp))
  pi.hat = diag(pimat %*% t(temp))
  ifs = (as.numeric(dat$a == f.vec)*pi.hat) * (dat$y - mu.hat) + mu.hat
  return(list(ifs = ifs, est = mean(ifs), sd = sd(ifs)/sqrt(length(f.vec))))
}
get.psihatU2 <- function(D){
  dat = D$data; mumat = D$true.mumat; pimat = D$true.pimat; f.vec = D$muhat.min
  Avals = sort(unique(dat$a))
  temp = sapply(Avals, function(x) as.numeric(Avals[f.vec] == x))
  mu.hat = diag(mumat %*% t(temp))
  #pi.hat = diag(pimat %*% t(temp))
  #ifs = (as.numeric(dat$a == f.vec)/pi.hat) * (dat$y - mu.hat) + mu.hat
  #return(list(est = mean(ifs), sd = sd(ifs)/sqrt(length(f.vec))))
  return(list(est = mean(mu.hat), sd = sd(mu.hat)/sqrt(length(f.vec))))
}
get.psihat2 <- function(D){
  dat = D$data; mumat = D$true.mumat; pimat = D$true.pimat; f.vec = D$fhat
  Avals = sort(unique(dat$a))
  temp = sapply(Avals, function(x) as.numeric(Avals[f.vec] == x))
  mu.hat = diag(mumat %*% t(temp))
  #pi.hat = diag(pimat %*% t(temp))
  #ifs = (as.numeric(dat$a == f.vec)/pi.hat) * (dat$y - mu.hat) + mu.hat
  #return(list(est = mean(ifs), sd = sd(ifs)/sqrt(length(f.vec))))
  return(list(est = mean(mu.hat), sd = sd(mu.hat)/sqrt(length(f.vec))))
}

get.psihat.mu <- function(D){
  #only fluctuate mu
  dat = D$data; mumat = D$muhat; pimat = D$true.pimat; f.vec = D$fhat
  Avals = sort(unique(dat$a))
  temp = sapply(Avals, function(x) as.numeric(Avals[f.vec] == x))
  mu.hat = diag(mumat %*% t(temp))
  pi.hat = diag(pimat %*% t(temp))
  ifs = (as.numeric(dat$a == f.vec)/pi.hat) * (dat$y - mu.hat) + mu.hat
  return(list(ifs = ifs, est = mean(ifs), sd = sd(ifs)/sqrt(length(f.vec))))
}
get.psihatU.mu <- function(D, type = 'double'){
  #only fluctuate mu
  # used to have this using true f but don't know why
  dat = D$data; mumat = D$muhat; pimat = D$true.pimat
  if(type == 'double'){f.vec = D$muhat.min}
  else if(type == 'triple'){f.vec = D$muhat.min2}
  Avals = sort(unique(dat$a))
  temp = sapply(Avals, function(x) as.numeric(Avals[f.vec] == x))
  mu.hat = diag(mumat %*% t(temp))
  pi.hat = diag(pimat %*% t(temp))
  ifs = (as.numeric(dat$a == f.vec)/pi.hat) * (dat$y - mu.hat) + mu.hat
  return(list(ifs = ifs, est = mean(ifs), sd = sd(ifs)/sqrt(length(f.vec))))
}

get.psihat.pi <- function(D){
  #only fluctuate pi
  dat = D$data; mumat = D$true.mumat; pimat = D$inv.pihat; f.vec = D$fhat
  Avals = sort(unique(dat$a))
  temp = sapply(Avals, function(x) as.numeric(Avals[f.vec] == x))
  mu.hat = diag(mumat %*% t(temp))
  pi.hat = diag(pimat %*% t(temp))
  ifs = (as.numeric(dat$a == f.vec)*pi.hat) * (dat$y - mu.hat) + mu.hat
  return(list(ifs = ifs, est = mean(ifs), sd = sd(ifs)/sqrt(length(f.vec))))
}
get.psihatU.pi <- function(D, type = 'double'){
  #only fluctuate pi
  # used to have this using true f but don't know why
  dat = D$data; mumat = D$true.mumat; pimat = D$inv.pihat
  if(type == 'double'){f.vec = D$muhat.min}
  else if(type == 'triple'){f.vec = D$muhat.min2}
  Avals = sort(unique(dat$a))
  temp = sapply(Avals, function(x) as.numeric(Avals[f.vec] == x))
  mu.hat = diag(mumat %*% t(temp))
  pi.hat = diag(pimat %*% t(temp))
  ifs = (as.numeric(dat$a == f.vec)*pi.hat) * (dat$y - mu.hat) + mu.hat
  return(list(ifs = ifs, est = mean(ifs), sd = sd(ifs)/sqrt(length(f.vec))))
}

get.psiU.if <- function(D){
  # don't fluctuate either pi or mu
  dat = D$data; mumat = D$true.mumat; pimat = D$true.pimat; f.vec = D$a.star
  Avals = sort(unique(dat$a))
  temp = sapply(Avals, function(x) as.numeric(Avals[f.vec] == x))
  mu.hat = diag(mumat %*% t(temp))
  pi.hat = diag(pimat %*% t(temp))
  ifs = (as.numeric(dat$a == f.vec)/pi.hat) * (dat$y - mu.hat) + mu.hat
  return(list(ifs = ifs, est = mean(ifs), sd = sd(ifs)/sqrt(length(f.vec))))
}

# parameter values
nsim = 100
K = c(1.99,2.99,3.99,5.99)
N = 5000 # size of dataset
e.mean <- 1 # mean of error term

### run non-binding constraint version ----
# produce all datasets
datlist <- lapply(c(1:length(K)), function(x) lapply(1:nsim, function(y) simFunc(N=5000)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$K <- K[l]}}

# get true constrained f vector (f_c)
f.vecs <- lapply(datlist, function(y) lapply(y, function(m) get.f(m)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$f.vec <- f.vecs[[l]][[j]]}}

# get true unconstrained f vector (f_u)
mu.min = f.vecs
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$mu.min = apply(datlist[[l]][[j]]$true.mumat,1,which.min)
    mu.min[[l]][[j]] = datlist[[l]][[j]]$mu.min
  }
}

# get true constrained causal parameter (psi_c)
psis <- lapply(datlist, function(y) lapply(y, function(m) get.psi(D=m)))
psis.mat <- lapply(psis, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psi <- psis[[l]][[j]]}}

# get true unconstrained causal parameter (psi_u)
psiUs.mat <- psis.mat
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$psiU = mean(apply(datlist[[l]][[j]]$true.mumat,1,min))
    psiUs.mat[[l]] = c(datlist[[l]][[j]]$psiU,sd(apply(datlist[[l]][[j]]$true.mumat,1,min)))
  }
}

# add error terms to mumat and pihat
etahat <- lapply(datlist, function(y) lapply(y, function(m) add.error(D=m, e.mean = 2)))
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$muhat <- etahat[[l]][[j]]$muhat
    datlist[[l]][[j]]$pihat <- etahat[[l]][[j]]$pihat
  }
}

# get unconstrained fhat from noisy mumat and pihat (fhat_u)
muhat.min = f.vecs
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){
  datlist[[l]][[j]]$muhat.min = apply(datlist[[l]][[j]]$muhat,1,which.min)
  muhat.min[[l]][[j]] = datlist[[l]][[j]]$muhat.min
  }
}

# get fhat from noisy mumat and pihat (fhat_c)
fhats = f.vecs
for(i in 1:length(datlist)){
  for(j in 1:length(datlist[[i]])){
    datlist[[i]][[j]]$fhat = get.fhat(datlist[[i]][[j]])
    fhats[[i]][[j]] = datlist[[i]][[j]]$fhat
  }
}

# get estimated unconstrained psihat (psihat_u)
psihatUs <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU(D=m)))
psihatU.mat <- lapply(psihatUs, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU <- psihatUs[[l]][[j]]}}

# get estimated psihat -- f, mu and pi all noisy (psihat_c)
psihats <- lapply(datlist, function(y) lapply(y, function(m) get.psihat(D=m)))
psihat.mat <- lapply(psihats, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat <- psihats[[l]][[j]]}}

# get estimated unconstrained psihat tilde -- f noisy, mu and pi true
psihatU2s <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU2(D=m)))
psihatU2.mat <- lapply(psihatU2s, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU2 <- psihatU2s[[l]][[j]]}}

# get estimated constrained psihat tilde -- f noisy, mu and pi true
psihat2s <- lapply(datlist, function(y) lapply(y, function(m) get.psihat2(D=m)))
psihat2.mat <- lapply(psihat2s, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat2 <- psihat2s[[l]][[j]]}}

# output summary statistics
rmse_uhat_u <- unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psiU - j$psihatU$est)^2 ))))))
rmse_uhat_utilde <- unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihatU$est - j$psihatU2$est)^2 ))))))
rmse_chat_c <- unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psi$effect - j$psihat$est)^2 ))))))
rmse_chat_ctilde <- unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihat$est - j$psihatU2$est)^2 ))))))

bias_uhat_u <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psiU - j$psihatU$est) ))))))
bias_uhat_utilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihatU$est - j$psihatU2$est) ))))))
bias_chat_c <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psi$effect - j$psihat$est) ))))))
bias_chat_ctilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihat$est - j$psihatU2$est) ))))))

coverage_uhat_u <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psiU) & (j$psihatU$est + 1.96*j$psihatU$sd >= j$psiU) ))))))
coverage_uhat_utilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psihatU2$est) & (j$psihatU$est + 1.96*j$psihatU$sd >= j$psihatU2$est) ))))))
coverage_chat_c <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat$est - 1.96*j$psihat$sd <= j$psi$effect) & (j$psihat$est + 1.96*j$psihat$sd >= j$psi$effect) ))))))
coverage_chat_ctilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat$est - 1.96*j$psihat$sd <= j$psihat2$est) & (j$psihat$est + 1.96*j$psihat$sd >= j$psihat2$est) ))))))

tabs <- tabsU <- list()
for(i in 1:length(datlist)){tabs[[i]] = round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), fhats[[i]], f.vecs[[i]]),1,mean),ncol=3),5)}
for(i in 1:length(datlist)){tabsU[[i]] = round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), muhat.min[[i]], mu.min[[i]]),1,mean),ncol=3),5)}

write.csv(do.call("rbind", tabs), "~jacquelinemauro/Dropbox/sorter/approxSim/ftables_notbinding.csv")
write.csv(do.call("rbind", tabsU), "~jacquelinemauro/Dropbox/sorter/approxSim/fUtables_notbinding.csv")

write.csv(data.frame(K,rmse_uhat_u,bias_uhat_u,coverage_uhat_u), "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_notbinding_uhat_u.csv")
write.csv(data.frame(K,rmse_uhat_utilde,bias_uhat_utilde,coverage_uhat_utilde), "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_notbinding_uhat_utilde.csv")
write.csv(data.frame(K,rmse_chat_c,bias_chat_c,coverage_chat_c), "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_notbinding_chat_c.csv")
write.csv(data.frame(K,rmse_chat_ctilde,bias_chat_ctilde,coverage_chat_ctilde), "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_notbinding_chat_ctilde.csv")

write.csv(cbind(K,matrix(unlist(psis.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/true_psi_notbinding.csv")
write.csv(cbind(K,matrix(unlist(psihat.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/allnoise_psihat_notbinding.csv")
write.csv(cbind(K,matrix(unlist(psihat2.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/fnoise_psihat_notbinding.csv")
write.csv(cbind(K,matrix(unlist(psiUs.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/true_psiU_notbinding.csv")
write.csv(cbind(K,matrix(unlist(psihatU.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/allnoise_psihatU_notbinding.csv")
write.csv(cbind(K,matrix(unlist(psihatU2.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/fnoise_psihatU_notbinding.csv")


### run binding constraint version ----
# only change is simulation function and output naming
# produce all datasets
datlist <- lapply(c(1:length(K)), function(x) lapply(1:nsim, function(y) simFunc2(N=5000)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$K <- K[l]}}

# get true constrained f vector (f_c)
f.vecs <- lapply(datlist, function(y) lapply(y, function(m) get.f(m)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$f.vec <- f.vecs[[l]][[j]]}}

# get true unconstrained f vector (f_u)
mu.min = f.vecs
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$mu.min = apply(datlist[[l]][[j]]$true.mumat,1,which.min)
    mu.min[[l]][[j]] = datlist[[l]][[j]]$mu.min
  }
}

# get true constrained causal parameter (psi_c)
psis <- lapply(datlist, function(y) lapply(y, function(m) get.psi(D=m)))
psis.mat <- lapply(psis, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psi <- psis[[l]][[j]]}}

# get true unconstrained causal parameter (psi_u)
psiUs.mat <- psis.mat
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$psiU = mean(apply(datlist[[l]][[j]]$true.mumat,1,min))
    psiUs.mat[[l]] = c(datlist[[l]][[j]]$psiU,sd(apply(datlist[[l]][[j]]$true.mumat,1,min)))
  }
}

# add error terms to mumat and pihat
etahat <- lapply(datlist, function(y) lapply(y, function(m) add.error(D=m, e.mean = 2)))
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$muhat <- etahat[[l]][[j]]$muhat
    datlist[[l]][[j]]$pihat <- etahat[[l]][[j]]$pihat
  }
}

# get unconstrained fhat from noisy mumat and pihat (fhat_u)
muhat.min = f.vecs
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){
  datlist[[l]][[j]]$muhat.min = apply(datlist[[l]][[j]]$muhat,1,which.min)
  muhat.min[[l]][[j]] = datlist[[l]][[j]]$muhat.min
}
}

# get fhat from noisy mumat and pihat (fhat_c)
fhats = f.vecs
for(i in 1:length(datlist)){
  for(j in 1:length(datlist[[i]])){
    datlist[[i]][[j]]$fhat = get.fhat(datlist[[i]][[j]])
    fhats[[i]][[j]] = datlist[[i]][[j]]$fhat
  }
}

# get estimated unconstrained psihat (psihat_u)
psihatUs <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU(D=m)))
psihatU.mat <- lapply(psihatUs, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU <- psihatUs[[l]][[j]]}}

# get estimated psihat -- f, mu and pi all noisy (psihat_c)
psihats <- lapply(datlist, function(y) lapply(y, function(m) get.psihat(D=m)))
psihat.mat <- lapply(psihats, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat <- psihats[[l]][[j]]}}

# get estimated unconstrained psihat tilde -- f noisy, mu and pi true
psihatU2s <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU2(D=m)))
psihatU2.mat <- lapply(psihatU2s, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU2 <- psihatU2s[[l]][[j]]}}

# get estimated constrained psihat tilde -- f noisy, mu and pi true
psihat2s <- lapply(datlist, function(y) lapply(y, function(m) get.psihat2(D=m)))
psihat2.mat <- lapply(psihat2s, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat2 <- psihat2s[[l]][[j]]}}

# output summary statistics
rmse_uhat_u <- unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psiU - j$psihatU$est)^2 ))))))
rmse_uhat_utilde <- unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihatU$est - j$psihatU2$est)^2 ))))))
rmse_chat_c <- unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psi$effect - j$psihat$est)^2 ))))))
rmse_chat_ctilde <- unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihat$est - j$psihatU2$est)^2 ))))))

bias_uhat_u <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psiU - j$psihatU$est) ))))))
bias_uhat_utilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihatU$est - j$psihatU2$est) ))))))
bias_chat_c <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psi$effect - j$psihat$est) ))))))
bias_chat_ctilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihat$est - j$psihatU2$est) ))))))

coverage_uhat_u <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psiU) & (j$psihatU$est + 1.96*j$psihatU$sd >= j$psiU) ))))))
coverage_uhat_utilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psihatU2$est) & (j$psihatU$est + 1.96*j$psihatU$sd >= j$psihatU2$est) ))))))
coverage_chat_c <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat$est - 1.96*j$psihat$sd <= j$psi$effect) & (j$psihat$est + 1.96*j$psihat$sd >= j$psi$effect) ))))))
coverage_chat_ctilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat$est - 1.96*j$psihat$sd <= j$psihat2$est) & (j$psihat$est + 1.96*j$psihat$sd >= j$psihat2$est) ))))))

tabs <- tabsU <- list()
for(i in 1:length(datlist)){tabs[[i]] = round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), fhats[[i]], f.vecs[[i]]),1,mean),ncol=3),5)}
for(i in 1:length(datlist)){tabsU[[i]] = round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), muhat.min[[i]], mu.min[[i]]),1,mean),ncol=3),5)}

write.csv(do.call("rbind", tabs), "~jacquelinemauro/Dropbox/sorter/approxSim/ftables_notbinding.csv")
write.csv(do.call("rbind", tabsU), "~jacquelinemauro/Dropbox/sorter/approxSim/fUtables_notbinding.csv")

write.csv(data.frame(K,rmse_uhat_u,bias_uhat_u,coverage_uhat_u), "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_notbinding_uhat_u.csv")
write.csv(data.frame(K,rmse_uhat_utilde,bias_uhat_utilde,coverage_uhat_utilde), "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_notbinding_uhat_utilde.csv")
write.csv(data.frame(K,rmse_chat_c,bias_chat_c,coverage_chat_c), "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_notbinding_chat_c.csv")
write.csv(data.frame(K,rmse_chat_ctilde,bias_chat_ctilde,coverage_chat_ctilde), "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_notbinding_chat_ctilde.csv")

write.csv(cbind(K,matrix(unlist(psis.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/true_psi_notbinding.csv")
write.csv(cbind(K,matrix(unlist(psihat.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/allnoise_notpsihat_binding.csv")
write.csv(cbind(K,matrix(unlist(psihat2.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/fnoise_psihat_notbinding.csv")
write.csv(cbind(K,matrix(unlist(psiUs.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/true_psiU_notbinding.csv")
write.csv(cbind(K,matrix(unlist(psihatU.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/allnoise_notpsihatU_binding.csv")
write.csv(cbind(K,matrix(unlist(psihatU2.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/fnoise_psihatU_notbinding.csv")


### run binding constraint version, negative emean ----
# produce all datasets
datlist <- lapply(c(1:length(K)), function(x) lapply(1:nsim, function(y) simFunc3(N=5000)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$K <- K[l]}}

# get true constrained f vector (f_c)
f.vecs <- lapply(datlist, function(y) lapply(y, function(m) get.f(m)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$f.vec <- f.vecs[[l]][[j]]}}

# get true unconstrained f vector (f_u)
mu.min = f.vecs
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$mu.min = apply(datlist[[l]][[j]]$true.mumat,1,which.min)
    mu.min[[l]][[j]] = datlist[[l]][[j]]$mu.min
  }
}

# get true constrained causal parameter (psi_c)
psis <- lapply(datlist, function(y) lapply(y, function(m) get.psi(D=m)))
psis.mat <- lapply(psis, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psi <- psis[[l]][[j]]}}

# get true unconstrained causal parameter (psi_u)
psiUs.mat <- psis.mat
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$psiU = mean(apply(datlist[[l]][[j]]$true.mumat,1,min))
    psiUs.mat[[l]] = c(datlist[[l]][[j]]$psiU,sd(apply(datlist[[l]][[j]]$true.mumat,1,min)))
  }
}

# add error terms to mumat and pihat
etahat <- lapply(datlist, function(y) lapply(y, function(m) add.error(D=m, e.mean = -1)))
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$muhat <- etahat[[l]][[j]]$muhat
    datlist[[l]][[j]]$inv.pihat <- etahat[[l]][[j]]$inv.pihat
  }
}

# get unconstrained fhat from noisy mumat and pihat (fhat_u)
muhat.min = f.vecs
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){
  datlist[[l]][[j]]$muhat.min = apply(datlist[[l]][[j]]$muhat,1,which.min)
  muhat.min[[l]][[j]] = datlist[[l]][[j]]$muhat.min
}
}

# get fhat from noisy mumat and pihat (fhat_c)
fhats = f.vecs
for(i in 1:length(datlist)){
  for(j in 1:length(datlist[[i]])){
    datlist[[i]][[j]]$fhat = get.fhat(datlist[[i]][[j]])
    fhats[[i]][[j]] = datlist[[i]][[j]]$fhat
  }
}

# get estimated unconstrained psihat (psihat_u)
psihatUs <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU(D=m)))
psihatU.mat <- lapply(psihatUs, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU <- psihatUs[[l]][[j]]}}

# get estimated psihat -- f, mu and pi all noisy (psihat_c)
psihats <- lapply(datlist, function(y) lapply(y, function(m) get.psihat(D=m)))
psihat.mat <- lapply(psihats, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat <- psihats[[l]][[j]]}}

# get estimated unconstrained psihat tilde -- f noisy, mu and pi true
psihatU2s <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU2(D=m)))
psihatU2.mat <- lapply(psihatU2s, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU2 <- psihatU2s[[l]][[j]]}}

# get estimated constrained psihat tilde -- f noisy, mu and pi true
psihat2s <- lapply(datlist, function(y) lapply(y, function(m) get.psihat2(D=m)))
psihat2.mat <- lapply(psihat2s, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat2 <- psihat2s[[l]][[j]]}}

# output summary statistics

rmse_uhat_u <- unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psiU - j$psihatU$est)^2 ))))))
rmse_uhat_utilde <- unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihatU$est - j$psihatU2$est)^2 ))))))
rmse_chat_c <- unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psi$effect - j$psihat$est)^2 ))))))
rmse_chat_ctilde <- unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihat$est - j$psihatU2$est)^2 ))))))

bias_uhat_u <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psiU - j$psihatU$est) ))))))
bias_uhat_utilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihatU$est - j$psihatU2$est) ))))))
bias_chat_c <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psi$effect - j$psihat$est) ))))))
bias_chat_ctilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihat$est - j$psihatU2$est) ))))))

coverage_uhat_u <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psiU) & (j$psihatU$est + 1.96*j$psihatU$sd >= j$psiU) ))))))
coverage_uhat_utilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psihatU2$est) & (j$psihatU$est + 1.96*j$psihatU$sd >= j$psihatU2$est) ))))))
coverage_chat_c <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat$est - 1.96*j$psihat$sd <= j$psi$effect) & (j$psihat$est + 1.96*j$psihat$sd >= j$psi$effect) ))))))
coverage_chat_ctilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat$est - 1.96*j$psihat$sd <= j$psihat2$est) & (j$psihat$est + 1.96*j$psihat$sd >= j$psihat2$est) ))))))


power_uhat_u <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihatU$est + 1.96*j$psihatU$sd < mean(j$data$y) ))))))
power_chat_c <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihat$est + 1.96*j$psihat$sd < mean(j$data$y)))))))

power_uhat_u_mu <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihatU.mu$est + 1.96*j$psihatU.mu$sd < mean(j$data$y)  ))))))
power_uhat_u_pi <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihatU.pi$est + 1.96*j$psihatU.pi$sd < mean(j$data$y)  ))))))
power_chat_c_mu <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihat.mu$est + 1.96*j$psihat.mu$sd < mean(j$data$y) ))))))
power_chat_c_pi <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihat.pi$est + 1.96*j$psihat.pi$sd < mean(j$data$y) ))))))



f.correct = round(unlist(lapply(datlist, function(l) mean(unlist(lapply(l, function(k) mean(k$f.vec == k$fhat)))))),4)
fU.correct = round(unlist(lapply(datlist, function(l) mean(unlist(lapply(l, function(k) mean(k$muhat.min == k$a.star)))))),4)

tabs <- tabsU <- list()
for(i in 1:length(datlist)){tabs[[i]] = round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), fhats[[i]], f.vecs[[i]]),1,mean),ncol=3),5)}
#for(i in 1:length(datlist)){tabsU[[i]] = round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), dat.list[[i]], mu.min[[i]]),1,mean),ncol=3),5)}



dU1 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psiUs.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihatU.mat, function(a) a[1])),4),
                 rmse_uhat_u,bias_uhat_u,coverage_uhat_u,fU.correct,
                 power_uhat_u)
dU2 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psihatU2.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihatU.mat, function(a) a[1])),4),
                 rmse_uhat_utilde,bias_uhat_utilde,coverage_uhat_utilde,fU.correct,
                 power_uhat_u)
dC1 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psis.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihat.mat, function(a) a[1])),4),
                 rmse_chat_c,bias_chat_c,coverage_chat_c,f.correct,
                 power_chat_c)
dC2 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psihat2.mat, function(a) a[1])),2),
                 round(unlist(lapply(psihat.mat, function(a) a[1])),2),
                 rmse_chat_ctilde,bias_chat_ctilde,coverage_chat_ctilde,f.correct,
                 power_chat_c)
names(dU1) <- names(dU2) <- names(dC1) <- names(dC2) <- c('Rate','$\\psi$', '\\hat{\\psi}', 'RMSE', 'Bias', '95% Coverage', '$f = \\hat{f}$','Excludes \bar{Y}')
write.csv(dU1, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_uhat_uNeg.csv",row.names = F)
write.csv(dU2, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_uhat_utildeNeg.csv",row.names = F)
write.csv(dC1, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_chat_cNeg.csv",row.names = F)
write.csv(dC2, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_chat_ctildeNeg.csv",row.names = F)

print(xtable(x = dU1, caption = 'Unconstrained Optimization, True Parameter',label = 'tab:dU1a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dU2, caption = 'Unconstrained Optimization, Noisy f',label = 'tab:dU2a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dC1, caption = 'Constrained Optimization, True Parameter',label = 'tab:dC1a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dC2, caption = 'Constrained Optimization, Noisy f',label = 'tab:dC2a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)


### run binding constraint, small effect version ----
# only change is simulation function and output naming
# produce all datasets
datlist <- lapply(c(1:length(K)), function(x) lapply(1:nsim, function(y) simFunc3(N=5000)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$K <- K[l]}}

# get true constrained f vector (f_c)
f.vecs <- lapply(datlist, function(y) lapply(y, function(m) get.f(m)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$f.vec <- f.vecs[[l]][[j]]}}

# get true unconstrained f vector (f_u)
mu.min = f.vecs # this is useless, this is a.star
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$mu.min = apply(datlist[[l]][[j]]$true.mumat,1,which.min)
    mu.min[[l]][[j]] = datlist[[l]][[j]]$mu.min
  }
}

# get true constrained causal parameter (psi_c)
psis <- lapply(datlist, function(y) lapply(y, function(m) get.psi(D=m)))
psis.mat <- lapply(psis, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psi <- psis[[l]][[j]]}}

# get true unconstrained causal parameter (psi_u)
psiUs.mat <- psis.mat
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$psiU = mean(apply(datlist[[l]][[j]]$true.mumat,1,min))
    psiUs.mat[[l]] = c(datlist[[l]][[j]]$psiU,sd(apply(datlist[[l]][[j]]$true.mumat,1,min)))
  }
}

# add error terms to mumat and pihat
etahat <- lapply(datlist, function(y) lapply(y, function(m) add.error(D=m, e.mean = e.mean)))
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$muhat <- etahat[[l]][[j]]$muhat
    datlist[[l]][[j]]$inv.pihat <- etahat[[l]][[j]]$inv.pihat
  }
}

# get unconstrained fhat from noisy mumat and pihat (fhat_u)
muhat.min = f.vecs
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){
  datlist[[l]][[j]]$muhat.min = apply(datlist[[l]][[j]]$muhat,1,which.min)
  muhat.min[[l]][[j]] = datlist[[l]][[j]]$muhat.min
}
}

# get fhat from noisy mumat and pihat (fhat_c)
fhats = f.vecs
for(i in 1:length(datlist)){
  for(j in 1:length(datlist[[i]])){
    datlist[[i]][[j]]$fhat = get.fhat(datlist[[i]][[j]])
    fhats[[i]][[j]] = datlist[[i]][[j]]$fhat
  }
}

# get estimated unconstrained psihat (psihat_u)
psihatUs <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU(D=m)))
psihatU.mat <- lapply(psihatUs, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU <- psihatUs[[l]][[j]]}}

# get estimated psihat -- f, mu and pi all noisy (psihat_c)
psihats <- lapply(datlist, function(y) lapply(y, function(m) get.psihat(D=m)))
psihat.mat <- lapply(psihats, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat <- psihats[[l]][[j]]}}

# get estimated unconstrained psihat tilde -- f noisy, mu and pi true
psihatU2s <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU2(D=m)))
psihatU2.mat <- lapply(psihatU2s, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU2 <- psihatU2s[[l]][[j]]}}

# get estimated constrained psihat tilde -- f noisy, mu and pi true
psihat2s <- lapply(datlist, function(y) lapply(y, function(m) get.psihat2(D=m)))
psihat2.mat <- lapply(psihat2s, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat2 <- psihat2s[[l]][[j]]}}

# output summary statistics
rmse_uhat_u <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psiU - j$psihatU$est)^2 )))))),4)
rmse_uhat_utilde <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihatU$est - j$psihatU2$est)^2 )))))),4)
rmse_chat_c <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psi$effect - j$psihat$est)^2 )))))),4)
rmse_chat_ctilde <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihat$est - j$psihatU2$est)^2 )))))),4)

bias_uhat_u <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psiU - j$psihatU$est) )))))),4)
bias_uhat_utilde <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihatU$est - j$psihatU2$est) )))))),4)
bias_chat_c <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psi$effect - j$psihat$est) )))))),4)
bias_chat_ctilde <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihat$est - j$psihatU2$est) )))))),4)

coverage_uhat_u <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psiU) & (j$psihatU$est + 1.96*j$psihatU$sd >= j$psiU) ))))))
coverage_uhat_utilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psihatU2$est) & (j$psihatU$est + 1.96*j$psihatU$sd >= j$psihatU2$est) ))))))
coverage_chat_c <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat$est - 1.96*j$psihat$sd <= j$psi$effect) & (j$psihat$est + 1.96*j$psihat$sd >= j$psi$effect) ))))))
coverage_chat_ctilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat$est - 1.96*j$psihat$sd <= j$psihat2$est) & (j$psihat$est + 1.96*j$psihat$sd >= j$psihat2$est) ))))))
f.correct = round(unlist(lapply(datlist, function(l) mean(unlist(lapply(l, function(k) mean(k$f.vec == k$fhat)))))),4)
fU.correct = round(unlist(lapply(datlist, function(l) mean(unlist(lapply(l, function(k) mean(k$muhat.min == k$a.star)))))),4)

tabs <- tabsU <- list()
for(i in 1:length(datlist)){tabs[[i]] = round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), fhats[[i]], f.vecs[[i]]),1,mean),ncol=3),5)}
for(i in 1:length(datlist)){tabsU[[i]] = round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), muhat.min[[i]], mu.min[[i]]),1,mean),ncol=3),5)}

write.csv(do.call("rbind", tabs), "~jacquelinemauro/Dropbox/sorter/approxSim/ftables_bindingSm.csv")
write.csv(do.call("rbind", tabsU), "~jacquelinemauro/Dropbox/sorter/approxSim/fUtables_bindingSm.csv")

dU1 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psiUs.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihatU.mat, function(a) a[1])),4),
                 rmse_uhat_u,bias_uhat_u,coverage_uhat_u,fU.correct)
dU2 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psihatU2.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihatU.mat, function(a) a[1])),4),
                 rmse_uhat_utilde,bias_uhat_utilde,coverage_uhat_utilde,fU.correct)
dC1 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psis.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihat.mat, function(a) a[1])),4),
                 rmse_chat_c,bias_chat_c,coverage_chat_c,f.correct)
dC2 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psihat2.mat, function(a) a[1])),2),
                 round(unlist(lapply(psihat.mat, function(a) a[1])),2),
                 rmse_chat_ctilde,bias_chat_ctilde,coverage_chat_ctilde,f.correct)
names(dU1) <- names(dU2) <- names(dC1) <- names(dC2) <- c('Rate','$\\psi$', 'Estimate', 'RMSE', 'Bias', '95% Coverage', 'Correct f %')
write.csv(dU1, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_uhat_uSm.csv",row.names = F)
write.csv(dU2, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_uhat_utildeSm.csv",row.names = F)
write.csv(dC1, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_chat_cSm.csv",row.names = F)
write.csv(dC2, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_chat_ctildeSm.csv",row.names = F)

print(xtable(x = dU1, caption = 'Unconstrained Optimization, True Parameter',label = 'tab:dU1'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dU2, caption = 'Unconstrained Optimization, Noisy f',label = 'tab:dU2'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dC1, caption = 'Unconstrained Optimization, True Parameter',label = 'tab:dC1'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dC2, caption = 'Unconstrained Optimization, Noisy f',label = 'tab:dC2'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)


write.csv(cbind(K,matrix(unlist(psis.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/true_psi_bindingSm.csv")
write.csv(cbind(K,matrix(unlist(psihat.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/allnoise_psihat_bindingSm.csv")
write.csv(cbind(K,matrix(unlist(psihat2.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/fnoise_psihat_bindingSm.csv")
write.csv(cbind(K,matrix(unlist(psiUs.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/true_psiU_bindingSm.csv")
write.csv(cbind(K,matrix(unlist(psihatU.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/allnoise_notpsihatU_bindingSm.csv")
write.csv(cbind(K,matrix(unlist(psihatU2.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/fnoise_psihatU_bindingSm.csv")

out.constrained = data.frame(Rate = paste('N^(1/',round(K),')',sep = ""),
                             TrueConstrained = unlist(lapply(psis.mat, function(a) a[1])),
                             EstFConstrained = unlist(lapply(psihat2.mat, function(a) a[1])),
                             EstAllConstrained = unlist(lapply(psihat.mat, function(a) a[1])))
out.constrained$Lower95 = out.constrained$EstAllConstrained - 1.96*unlist(lapply(psihat.mat, function(a) a[2]))
out.constrained$Upper95 = out.constrained$EstAllConstrained + 1.96*unlist(lapply(psihat.mat, function(a) a[2]))
out.constrained[,-1] = round(100*out.constrained[,-1],2)
write.csv(out.constrained, "~jacquelinemauro/Dropbox/sorter/approxSim/outConstrained_bindingSm.csv")

out.unconstrained = data.frame(Rate = paste('N^(1/',round(K),')',sep = ""),
                             TrueUnconstrained = unlist(lapply(psiUs.mat, function(a) a[1])),
                             EstFUnconstrained = unlist(lapply(psihatU2.mat, function(a) a[1])),
                             EstAllUnconstrained = unlist(lapply(psihatU.mat, function(a) a[1])))
out.unconstrained$Lower95 = out.unconstrained$EstAllUnconstrained - 1.96*unlist(lapply(psihatU.mat, function(a) a[2]))
out.unconstrained$Upper95 = out.unconstrained$EstAllUnconstrained + 1.96*unlist(lapply(psihatU.mat, function(a) a[2]))
out.unconstrained[,-1] = round(100*out.unconstrained[,-1],2)
write.csv(out.constrained, "~jacquelinemauro/Dropbox/sorter/approxSim/outUnconstrained_bindingSm.csv")



### run non binding constraint, small effect version ----
# only change is simulation function and output naming
# produce all datasets
datlist <- lapply(c(1:length(K)), function(x) lapply(1:nsim, function(y) simFunc4(N=5000)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$K <- K[l]}}

# get true constrained f vector (f_c)
f.vecs <- lapply(datlist, function(y) lapply(y, function(m) get.f(m)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$f.vec <- f.vecs[[l]][[j]]}}

# get true unconstrained f vector (f_u)
mu.min = f.vecs # this is useless, this is a.star
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$mu.min = apply(datlist[[l]][[j]]$true.mumat,1,which.min)
    mu.min[[l]][[j]] = datlist[[l]][[j]]$mu.min
  }
}

# get true constrained causal parameter (psi_c)
psis <- lapply(datlist, function(y) lapply(y, function(m) get.psi(D=m)))
psis.mat <- lapply(psis, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psi <- psis[[l]][[j]]}}

# get true unconstrained causal parameter (psi_u)
psiUs.mat <- psis.mat
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$psiU = mean(apply(datlist[[l]][[j]]$true.mumat,1,min))
    psiUs.mat[[l]] = c(datlist[[l]][[j]]$psiU,sd(apply(datlist[[l]][[j]]$true.mumat,1,min)))
  }
}

# add error terms to mumat and pihat
etahat <- lapply(datlist, function(y) lapply(y, function(m) add.error(D=m, e.mean = e.mean)))
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$muhat <- etahat[[l]][[j]]$muhat
    datlist[[l]][[j]]$inv.pihat <- etahat[[l]][[j]]$inv.pihat
  }
}

# get unconstrained fhat from noisy mumat and pihat (fhat_u)
muhat.min = f.vecs
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){
  datlist[[l]][[j]]$muhat.min = apply(datlist[[l]][[j]]$muhat,1,which.min)
  muhat.min[[l]][[j]] = datlist[[l]][[j]]$muhat.min
}
}

# get fhat from noisy mumat and pihat (fhat_c)
fhats = f.vecs
for(i in 1:length(datlist)){
  for(j in 1:length(datlist[[i]])){
    datlist[[i]][[j]]$fhat = get.fhat(datlist[[i]][[j]])
    fhats[[i]][[j]] = datlist[[i]][[j]]$fhat
  }
}

# get estimated unconstrained psihat (psihat_u)
psihatUs <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU(D=m)))
psihatU.mat <- lapply(psihatUs, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU <- psihatUs[[l]][[j]]}}

# get estimated psihat -- f, mu and pi all noisy (psihat_c)
psihats <- lapply(datlist, function(y) lapply(y, function(m) get.psihat(D=m)))
psihat.mat <- lapply(psihats, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat <- psihats[[l]][[j]]}}

# get estimated unconstrained psihat tilde -- f noisy, mu and pi true
psihatU2s <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU2(D=m)))
psihatU2.mat <- lapply(psihatU2s, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU2 <- psihatU2s[[l]][[j]]}}

# get estimated constrained psihat tilde -- f noisy, mu and pi true
psihat2s <- lapply(datlist, function(y) lapply(y, function(m) get.psihat2(D=m)))
psihat2.mat <- lapply(psihat2s, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat2 <- psihat2s[[l]][[j]]}}

# output summary statistics
rmse_uhat_u <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psiU - j$psihatU$est)^2 )))))),4)
rmse_uhat_utilde <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihatU$est - j$psihatU2$est)^2 )))))),4)
rmse_chat_c <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psi$effect - j$psihat$est)^2 )))))),4)
rmse_chat_ctilde <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihat$est - j$psihatU2$est)^2 )))))),4)

bias_uhat_u <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psiU - j$psihatU$est) )))))),4)
bias_uhat_utilde <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihatU$est - j$psihatU2$est) )))))),4)
bias_chat_c <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psi$effect - j$psihat$est) )))))),4)
bias_chat_ctilde <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihat$est - j$psihatU2$est) )))))),4)

coverage_uhat_u <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psiU) & (j$psihatU$est + 1.96*j$psihatU$sd >= j$psiU) ))))))
coverage_uhat_utilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psihatU2$est) & (j$psihatU$est + 1.96*j$psihatU$sd >= j$psihatU2$est) ))))))
coverage_chat_c <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat$est - 1.96*j$psihat$sd <= j$psi$effect) & (j$psihat$est + 1.96*j$psihat$sd >= j$psi$effect) ))))))
coverage_chat_ctilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat$est - 1.96*j$psihat$sd <= j$psihat2$est) & (j$psihat$est + 1.96*j$psihat$sd >= j$psihat2$est) ))))))
f.correct = round(unlist(lapply(datlist, function(l) mean(unlist(lapply(l, function(k) mean(k$f.vec == k$fhat)))))),4)
fU.correct = round(unlist(lapply(datlist, function(l) mean(unlist(lapply(l, function(k) mean(k$muhat.min == k$a.star)))))),4)

tabs <- tabsU <- list()
for(i in 1:length(datlist)){tabs[[i]] = round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), fhats[[i]], f.vecs[[i]]),1,mean),ncol=3),5)}
for(i in 1:length(datlist)){tabsU[[i]] = round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), muhat.min[[i]], mu.min[[i]]),1,mean),ncol=3),5)}

write.csv(do.call("rbind", tabs), "~jacquelinemauro/Dropbox/sorter/approxSim/ftables_nonbindingSm.csv")
write.csv(do.call("rbind", tabsU), "~jacquelinemauro/Dropbox/sorter/approxSim/fUtables_nonbindingSm.csv")

dU1 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psiUs.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihatU.mat, function(a) a[1])),4),
                 rmse_uhat_u,bias_uhat_u,coverage_uhat_u,fU.correct)
dU2 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psihatU2.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihatU.mat, function(a) a[1])),4),
                 rmse_uhat_utilde,bias_uhat_utilde,coverage_uhat_utilde,fU.correct)
dC1 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psis.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihat.mat, function(a) a[1])),4),
                 rmse_chat_c,bias_chat_c,coverage_chat_c,f.correct)
dC2 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psihat2.mat, function(a) a[1])),2),
                 round(unlist(lapply(psihat.mat, function(a) a[1])),2),
                 rmse_chat_ctilde,bias_chat_ctilde,coverage_chat_ctilde,f.correct)
names(dU1) <- names(dU2) <- names(dC1) <- names(dC2) <- c('Rate','$\\psi$', '\\hat{\\psi}', 'RMSE', 'Bias', '95% Coverage', '$f = \\hat{f}$')
write.csv(dU1, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_nonbinding_uhat_uSm.csv",row.names = F)
write.csv(dU2, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_nonbinding_uhat_utildeSm.csv",row.names = F)
write.csv(dC1, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_nonbinding_chat_cSm.csv",row.names = F)
write.csv(dC2, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_nonbinding_chat_ctildeSm.csv",row.names = F)

print(xtable(x = dU1, caption = 'Unconstrained Optimization, True Parameter',label = 'tab:dU1a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dU2, caption = 'Unconstrained Optimization, Noisy f',label = 'tab:dU2a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dC1, caption = 'Constrained Optimization, True Parameter',label = 'tab:dC1a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dC2, caption = 'Constrained Optimization, Noisy f',label = 'tab:dC2a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)


write.csv(cbind(K,matrix(unlist(psis.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/true_psi_nonbindingSm.csv")
write.csv(cbind(K,matrix(unlist(psihat.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/allnoise_psihat_nonbindingSm.csv")
write.csv(cbind(K,matrix(unlist(psihat2.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/fnoise_psihat_nonbindingSm.csv")
write.csv(cbind(K,matrix(unlist(psiUs.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/true_psiU_nonbindingSm.csv")
write.csv(cbind(K,matrix(unlist(psihatU.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/allnoise_notpsihatU_nonbindingSm.csv")
write.csv(cbind(K,matrix(unlist(psihatU2.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/fnoise_psihatU_nonbindingSm.csv")

out.constrained = data.frame(Rate = paste('N^(1/',round(K),')',sep = ""),
                             TrueConstrained = unlist(lapply(psis.mat, function(a) a[1])),
                             EstFConstrained = unlist(lapply(psihat2.mat, function(a) a[1])),
                             EstAllConstrained = unlist(lapply(psihat.mat, function(a) a[1])))
out.constrained$Lower95 = out.constrained$EstAllConstrained - 1.96*unlist(lapply(psihat.mat, function(a) a[2]))
out.constrained$Upper95 = out.constrained$EstAllConstrained + 1.96*unlist(lapply(psihat.mat, function(a) a[2]))
out.constrained[,-1] = round(100*out.constrained[,-1],2)
write.csv(out.constrained, "~jacquelinemauro/Dropbox/sorter/approxSim/outConstrained_nonbindingSm.csv")

out.unconstrained = data.frame(Rate = paste('N^(1/',round(K),')',sep = ""),
                               TrueUnconstrained = unlist(lapply(psiUs.mat, function(a) a[1])),
                               EstFUnconstrained = unlist(lapply(psihatU2.mat, function(a) a[1])),
                               EstAllUnconstrained = unlist(lapply(psihatU.mat, function(a) a[1])))
out.unconstrained$Lower95 = out.unconstrained$EstAllUnconstrained - 1.96*unlist(lapply(psihatU.mat, function(a) a[2]))
out.unconstrained$Upper95 = out.unconstrained$EstAllUnconstrained + 1.96*unlist(lapply(psihatU.mat, function(a) a[2]))
out.unconstrained[,-1] = round(100*out.unconstrained[,-1],2)
write.csv(out.constrained, "~jacquelinemauro/Dropbox/sorter/approxSim/outUnconstrained_nonbindingSm.csv")

### run non binding constraint, small effect version, negative emean ----
# just changing e.mean to -1
# produce all datasets
datlist <- lapply(c(1:length(K)), function(x) lapply(1:nsim, function(y) simFunc4(N=5000)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$K <- K[l]}}

# get true constrained f vector (f_c)
f.vecs <- lapply(datlist, function(y) lapply(y, function(m) get.f(m)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$f.vec <- f.vecs[[l]][[j]]}}

# get true unconstrained f vector (f_u)
mu.min = f.vecs # this is useless, this is a.star
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$mu.min = apply(datlist[[l]][[j]]$true.mumat,1,which.min)
    mu.min[[l]][[j]] = datlist[[l]][[j]]$mu.min
  }
}

# get true constrained causal parameter (psi_c)
psis <- lapply(datlist, function(y) lapply(y, function(m) get.psi(D=m)))
psis.mat <- lapply(psis, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psi <- psis[[l]][[j]]}}

# get true unconstrained causal parameter (psi_u)
psiUs.mat <- psis.mat
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$psiU = mean(apply(datlist[[l]][[j]]$true.mumat,1,min))
    psiUs.mat[[l]] = c(datlist[[l]][[j]]$psiU,sd(apply(datlist[[l]][[j]]$true.mumat,1,min)))
  }
}

# add error terms to mumat and pihat
etahat <- lapply(datlist, function(y) lapply(y, function(m) add.error(D=m, e.mean = -1)))
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$muhat <- etahat[[l]][[j]]$muhat
    datlist[[l]][[j]]$inv.pihat <- etahat[[l]][[j]]$inv.pihat
  }
}

# get unconstrained fhat from noisy mumat and pihat (fhat_u)
muhat.min = f.vecs
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){
  datlist[[l]][[j]]$muhat.min = apply(datlist[[l]][[j]]$muhat,1,which.min)
  muhat.min[[l]][[j]] = datlist[[l]][[j]]$muhat.min
}
}

# get fhat from noisy mumat and pihat (fhat_c)
fhats = f.vecs
for(i in 1:length(datlist)){
  for(j in 1:length(datlist[[i]])){
    datlist[[i]][[j]]$fhat = get.fhat(datlist[[i]][[j]])
    fhats[[i]][[j]] = datlist[[i]][[j]]$fhat
  }
}

# get estimated unconstrained psihat (psihat_u)
psihatUs <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU(D=m)))
psihatU.mat <- lapply(psihatUs, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU <- psihatUs[[l]][[j]]}}

# get estimated psihat -- f, mu and pi all noisy (psihat_c)
psihats <- lapply(datlist, function(y) lapply(y, function(m) get.psihat(D=m)))
psihat.mat <- lapply(psihats, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat <- psihats[[l]][[j]]}}

# get estimated unconstrained psihat tilde -- f noisy, mu and pi true
psihatU2s <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU2(D=m)))
psihatU2.mat <- lapply(psihatU2s, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU2 <- psihatU2s[[l]][[j]]}}

# get estimated constrained psihat tilde -- f noisy, mu and pi true
psihat2s <- lapply(datlist, function(y) lapply(y, function(m) get.psihat2(D=m)))
psihat2.mat <- lapply(psihat2s, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat2 <- psihat2s[[l]][[j]]}}

# output summary statistics
rmse_uhat_u <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psiU - j$psihatU$est)^2 )))))),4)
rmse_uhat_utilde <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihatU$est - j$psihatU2$est)^2 )))))),4)
rmse_chat_c <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psi$effect - j$psihat$est)^2 )))))),4)
rmse_chat_ctilde <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihat$est - j$psihatU2$est)^2 )))))),4)

bias_uhat_u <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psiU - j$psihatU$est) )))))),4)
bias_uhat_utilde <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihatU$est - j$psihatU2$est) )))))),4)
bias_chat_c <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psi$effect - j$psihat$est) )))))),4)
bias_chat_ctilde <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihat$est - j$psihatU2$est) )))))),4)

coverage_uhat_u <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psiU) & (j$psihatU$est + 1.96*j$psihatU$sd >= j$psiU) ))))))
coverage_uhat_utilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psihatU2$est) & (j$psihatU$est + 1.96*j$psihatU$sd >= j$psihatU2$est) ))))))
coverage_chat_c <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat$est - 1.96*j$psihat$sd <= j$psi$effect) & (j$psihat$est + 1.96*j$psihat$sd >= j$psi$effect) ))))))
coverage_chat_ctilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat$est - 1.96*j$psihat$sd <= j$psihat2$est) & (j$psihat$est + 1.96*j$psihat$sd >= j$psihat2$est) ))))))
f.correct = round(unlist(lapply(datlist, function(l) mean(unlist(lapply(l, function(k) mean(k$f.vec == k$fhat)))))),4)
fU.correct = round(unlist(lapply(datlist, function(l) mean(unlist(lapply(l, function(k) mean(k$muhat.min == k$a.star)))))),4)

tabs <- tabsU <- list()
for(i in 1:length(datlist)){tabs[[i]] = round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), fhats[[i]], f.vecs[[i]]),1,mean),ncol=3),5)}
for(i in 1:length(datlist)){tabsU[[i]] = round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), muhat.min[[i]], mu.min[[i]]),1,mean),ncol=3),5)}

write.csv(do.call("rbind", tabs), "~jacquelinemauro/Dropbox/sorter/approxSim/ftables_nonbindingNeg.csv")
write.csv(do.call("rbind", tabsU), "~jacquelinemauro/Dropbox/sorter/approxSim/fUtables_nonbindingNeg.csv")

dU1 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psiUs.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihatU.mat, function(a) a[1])),4),
                 rmse_uhat_u,bias_uhat_u,coverage_uhat_u,fU.correct)
dU2 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psihatU2.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihatU.mat, function(a) a[1])),4),
                 rmse_uhat_utilde,bias_uhat_utilde,coverage_uhat_utilde,fU.correct)
dC1 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psis.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihat.mat, function(a) a[1])),4),
                 rmse_chat_c,bias_chat_c,coverage_chat_c,f.correct)
dC2 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psihat2.mat, function(a) a[1])),2),
                 round(unlist(lapply(psihat.mat, function(a) a[1])),2),
                 rmse_chat_ctilde,bias_chat_ctilde,coverage_chat_ctilde,f.correct)
names(dU1) <- names(dU2) <- names(dC1) <- names(dC2) <- c('Rate','$\\psi$', '\\hat{\\psi}', 'RMSE', 'Bias', '95% Coverage', '$f = \\hat{f}$')
write.csv(dU1, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_nonbinding_uhat_uNeg.csv",row.names = F)
write.csv(dU2, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_nonbinding_uhat_utildeNeg.csv",row.names = F)
write.csv(dC1, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_nonbinding_chat_cNeg.csv",row.names = F)
write.csv(dC2, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_nonbinding_chat_ctildeNeg.csv",row.names = F)

print(xtable(x = dU1, caption = 'Unconstrained Optimization, True Parameter',label = 'tab:dU1a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dU2, caption = 'Unconstrained Optimization, Noisy f',label = 'tab:dU2a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dC1, caption = 'Constrained Optimization, True Parameter',label = 'tab:dC1a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dC2, caption = 'Constrained Optimization, Noisy f',label = 'tab:dC2a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)


write.csv(cbind(K,matrix(unlist(psis.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/true_psi_nonbindingNeg.csv")
write.csv(cbind(K,matrix(unlist(psihat.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/allnoise_psihat_nonbindingNeg.csv")
write.csv(cbind(K,matrix(unlist(psihat2.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/fnoise_psihat_nonbindingNeg.csv")
write.csv(cbind(K,matrix(unlist(psiUs.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/true_psiU_nonbindingNeg.csv")
write.csv(cbind(K,matrix(unlist(psihatU.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/allnoise_notpsihatU_nonbindingNeg.csv")
write.csv(cbind(K,matrix(unlist(psihatU2.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/fnoise_psihatU_nonbindingNeg.csv")

out.constrained = data.frame(Rate = paste('N^(1/',round(K),')',sep = ""),
                             TrueConstrained = unlist(lapply(psis.mat, function(a) a[1])),
                             EstFConstrained = unlist(lapply(psihat2.mat, function(a) a[1])),
                             EstAllConstrained = unlist(lapply(psihat.mat, function(a) a[1])))
out.constrained$Lower95 = out.constrained$EstAllConstrained - 1.96*unlist(lapply(psihat.mat, function(a) a[2]))
out.constrained$Upper95 = out.constrained$EstAllConstrained + 1.96*unlist(lapply(psihat.mat, function(a) a[2]))
out.constrained[,-1] = round(100*out.constrained[,-1],2)
write.csv(out.constrained, "~jacquelinemauro/Dropbox/sorter/approxSim/outConstrained_nonbindingNeg.csv")

out.unconstrained = data.frame(Rate = paste('N^(1/',round(K),')',sep = ""),
                               TrueUnconstrained = unlist(lapply(psiUs.mat, function(a) a[1])),
                               EstFUnconstrained = unlist(lapply(psihatU2.mat, function(a) a[1])),
                               EstAllUnconstrained = unlist(lapply(psihatU.mat, function(a) a[1])))
out.unconstrained$Lower95 = out.unconstrained$EstAllUnconstrained - 1.96*unlist(lapply(psihatU.mat, function(a) a[2]))
out.unconstrained$Upper95 = out.unconstrained$EstAllUnconstrained + 1.96*unlist(lapply(psihatU.mat, function(a) a[2]))
out.unconstrained[,-1] = round(100*out.unconstrained[,-1],2)
write.csv(out.constrained, "~jacquelinemauro/Dropbox/sorter/approxSim/outUnconstrained_nonbindingNeg.csv")




### run binding constraint, even smaller effect version fluctuate mu/pi separately ----
nsim= 200
K = c(1.99,2.99,3.99,5.99)
e.mean = 1
# produce all datasets
datlist <- lapply(c(1:length(K)), function(x) lapply(1:nsim, function(y) simFunc5(N=5000)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$K <- K[l]}}

# get true constrained f vector (f_c)
f.vecs <- lapply(datlist, function(y) lapply(y, function(m) get.f(m)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$f.vec <- f.vecs[[l]][[j]]}}

# get true constrained causal parameter (psi_c)
psis <- lapply(datlist, function(y) lapply(y, function(m) get.psi(D=m)))
psis.mat <- lapply(psis, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psi <- psis[[l]][[j]]}}

# get true unconstrained causal parameter (psi_u)
psiUs.mat <- psis.mat
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$psiU = mean(apply(datlist[[l]][[j]]$true.mumat,1,min))
    psiUs.mat[[l]] = c(datlist[[l]][[j]]$psiU,sd(apply(datlist[[l]][[j]]$true.mumat,1,min)))
  }
}

# get true unconstrained causal parameter using if
psiUIFs <- lapply(datlist, function(y) lapply(y, function(m) get.psiU.if(D=m)))
psiUIF.mat <- lapply(psiUIFs, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psiUIF <- psiUIFs[[l]][[j]]}}


# add error terms to mumat and pihat
etahat <- lapply(datlist, function(y) lapply(y, function(m) add.error(D=m, e.mean = e.mean)))
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$muhat <- etahat[[l]][[j]]$muhat
    datlist[[l]][[j]]$inv.pihat <- etahat[[l]][[j]]$inv.pihat
  }
}

# get fhat from noisy mumat and pihat (fhat_c)
fhats = f.vecs
for(i in 1:length(datlist)){
  for(j in 1:length(datlist[[i]])){
    datlist[[i]][[j]]$fhat = get.fhat(datlist[[i]][[j]])
    fhats[[i]][[j]] = datlist[[i]][[j]]$fhat
  }
}

# get unconstrained fhat from noisy mumat and pihat (fhat_u)
muhat.min = f.vecs
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){
  datlist[[l]][[j]]$muhat.min = apply(datlist[[l]][[j]]$muhat,1,which.min)
  muhat.min[[l]][[j]] = datlist[[l]][[j]]$muhat.min
}
}

# get estimated unconstrained psihat (psihat_u)
psihatUs <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU(D=m)))
psihatU.mat <- lapply(psihatUs, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU <- psihatUs[[l]][[j]]}}

# get estimated unconstrained psihat, only mu noisy
psihatU.mus <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU.mu(D=m)))
psihatU.mu.mat <- lapply(psihatU.mus, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU.mu <- psihatU.mus[[l]][[j]]}}

# get estimated unconstrained psihat, only pi noisy
psihatU.pis <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU.pi(D=m)))
psihatU.pi.mat <- lapply(psihatU.pis, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU.pi <- psihatU.pis[[l]][[j]]}}

# get estimated psihat -- f, mu and pi all noisy (psihat_c)
psihats <- lapply(datlist, function(y) lapply(y, function(m) get.psihat(D=m)))
psihat.mat <- lapply(psihats, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat <- psihats[[l]][[j]]}}

# get estimated psihat -- f, mu noisy, pi true
psihat.pis <- lapply(datlist, function(y) lapply(y, function(m) get.psihat.pi(D=m)))
psihat.pi.mat <- lapply(psihat.pis, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat.pi <- psihat.pis[[l]][[j]]}}

# get estimated psihat -- f, pi noisy, mu true
psihat.mus <- lapply(datlist, function(y) lapply(y, function(m) get.psihat.mu(D=m)))
psihat.mu.mat <- lapply(psihat.mus, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat.mu <- psihat.mus[[l]][[j]]}}


# get estimated unconstrained psihat tilde -- f noisy, mu and pi true
psihatU2s <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU2(D=m)))
psihatU2.mat <- lapply(psihatU2s, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU2 <- psihatU2s[[l]][[j]]}}

# get estimated constrained psihat tilde -- f noisy, mu and pi true
psihat2s <- lapply(datlist, function(y) lapply(y, function(m) get.psihat2(D=m)))
psihat2.mat <- lapply(psihat2s, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat2 <- psihat2s[[l]][[j]]}}

# output summary statistics
rmse_uhat_u <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psiU - j$psihatU$est)^2 )))))),4)
rmse_uhat_utilde <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihatU$est - j$psihatU2$est)^2 )))))),4)
rmse_chat_c <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psi$effect - j$psihat$est)^2 )))))),4)
rmse_chat_ctilde <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihat$est - j$psihatU2$est)^2 )))))),4)

rmse_uhat_u_mu <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psiU - j$psihatU.mu$est)^2 )))))),4)
rmse_uhat_u_pi <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psiU - j$psihatU.pi$est)^2 )))))),4)
rmse_chat_c_mu <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psi$effect - j$psihat.mu$est)^2 )))))),4)
rmse_chat_c_pi <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psi$effect - j$psihat.pi$est)^2 )))))),4)


bias_uhat_u <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psiU - j$psihatU$est) )))))),4)
bias_uhat_utilde <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihatU$est - j$psihatU2$est) )))))),4)
bias_chat_c <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psi$effect - j$psihat$est) )))))),4)
bias_chat_ctilde <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihat$est - j$psihatU2$est) )))))),4)

bias_uhat_u_mu <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psiU - j$psihatU.mu$est) )))))),4)
bias_uhat_u_pi <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psiU - j$psihatU.pi$est) )))))),4)
bias_chat_c_mu <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psi$effect - j$psihat.mu$est) )))))),4)
bias_chat_c_pi <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psi$effect - j$psihat.pi$est) )))))),4)

coverage_uhat_u <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psiU) & (j$psihatU$est + 1.96*j$psihatU$sd >= j$psiU) ))))))
coverage_uhat_utilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psihatU2$est) & (j$psihatU$est + 1.96*j$psihatU$sd >= j$psihatU2$est) ))))))
coverage_chat_c <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat$est - 1.96*j$psihat$sd <= j$psi$effect) & (j$psihat$est + 1.96*j$psihat$sd >= j$psi$effect) ))))))
coverage_chat_ctilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat$est - 1.96*j$psihat$sd <= j$psihat2$est) & (j$psihat$est + 1.96*j$psihat$sd >= j$psihat2$est) ))))))

coverage_uhat_u_mu <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU.mu$est - 1.96*j$psihatU.mu$sd <= j$psiU) & (j$psihatU.mu$est + 1.96*j$psihatU.mu$sd >= j$psiU) ))))))
coverage_uhat_u_pi <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU.pi$est - 1.96*j$psihatU.pi$sd <= j$psiU) & (j$psihatU.pi$est + 1.96*j$psihatU.pi$sd >= j$psiU) ))))))
coverage_chat_c_mu <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat.mu$est - 1.96*j$psihat.mu$sd <= j$psi$effect) & (j$psihat.mu$est + 1.96*j$psihat.mu$sd >= j$psi$effect) ))))))
coverage_chat_c_pi <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat.pi$est - 1.96*j$psihat.pi$sd <= j$psi$effect) & (j$psihat.pi$est + 1.96*j$psihat.pi$sd >= j$psi$effect) ))))))

power_uhat_u <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihatU$est + 1.96*j$psihatU$sd < mean(j$data$y) ))))))
power_chat_c <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihat$est + 1.96*j$psihat$sd < mean(j$data$y)))))))

power_uhat_u_mu <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihatU.mu$est + 1.96*j$psihatU.mu$sd < mean(j$data$y)  ))))))
power_uhat_u_pi <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihatU.pi$est + 1.96*j$psihatU.pi$sd < mean(j$data$y)  ))))))
power_chat_c_mu <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihat.mu$est + 1.96*j$psihat.mu$sd < mean(j$data$y) ))))))
power_chat_c_pi <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihat.pi$est + 1.96*j$psihat.pi$sd < mean(j$data$y) ))))))



f.correct = round(unlist(lapply(datlist, function(l) mean(unlist(lapply(l, function(k) mean(k$f.vec == k$fhat)))))),4)
fU.correct = round(unlist(lapply(datlist, function(l) mean(unlist(lapply(l, function(k) mean(k$muhat.min == k$a.star)))))),4)

tabs <- tabsU <- list()
for(i in 1:length(datlist)){tabs[[i]] = round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), fhats[[i]], f.vecs[[i]]),1,mean),ncol=3),5)}
#for(i in 1:length(datlist)){tabsU[[i]] = round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), dat.list[[i]], mu.min[[i]]),1,mean),ncol=3),5)}



dU1 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psiUs.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihatU.mat, function(a) a[1])),4),
                 rmse_uhat_u,bias_uhat_u,coverage_uhat_u,fU.correct,
                 power_uhat_u)
dUmu = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psiUs.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihatU.mu.mat, function(a) a[1])),4),
                 rmse_uhat_u_mu,bias_uhat_u_mu,coverage_uhat_u_mu,fU.correct,
                 power_uhat_u_mu)
dUpi = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psiUs.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihatU.pi.mat, function(a) a[1])),4),
                 rmse_uhat_u_pi,bias_uhat_u_pi,coverage_uhat_u_pi,fU.correct,
                 power_uhat_u_pi)
dU2 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psihatU2.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihatU.mat, function(a) a[1])),4),
                 rmse_uhat_utilde,bias_uhat_utilde,coverage_uhat_utilde,fU.correct,
                 power_uhat_u)
dC1 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psis.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihat.mat, function(a) a[1])),4),
                 rmse_chat_c,bias_chat_c,coverage_chat_c,f.correct,
                 power_chat_c)
dCmu = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psis.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihat.mu.mat, function(a) a[1])),4),
                 rmse_chat_c_mu,bias_chat_c_mu,coverage_chat_c_mu,f.correct,
                 power_chat_c_mu)
dCpi = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psis.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihat.pi.mat, function(a) a[1])),4),
                 rmse_chat_c_pi,bias_chat_c_pi,coverage_chat_c_pi,f.correct,
                 power_chat_c_pi)
dC2 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psihat2.mat, function(a) a[1])),2),
                 round(unlist(lapply(psihat.mat, function(a) a[1])),2),
                 rmse_chat_ctilde,bias_chat_ctilde,coverage_chat_ctilde,f.correct,
                 power_chat_c)
names(dU1) <- names(dU2) <- names(dC1) <- names(dC2) <- c('Rate','$\\psi$', '\\hat{\\psi}', 'RMSE', 'Bias', '95% Coverage', '$f = \\hat{f}$','Excludes \bar{Y}')
names(dUmu) <- names(dUpi) <- names(dCmu) <- names(dCpi) <- names(dU1)
write.csv(dU1, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_uhat_uVSm.csv",row.names = F)
write.csv(dU2, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_uhat_utildeVSm.csv",row.names = F)
write.csv(dC1, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_chat_cVSm.csv",row.names = F)
write.csv(dC2, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_chat_ctildeVSm.csv",row.names = F)

write.csv(dUmu, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_uhat_umuVSm.csv",row.names = F)
write.csv(dUpi, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_uhat_upiVSm.csv",row.names = F)
write.csv(dCmu, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_chat_cmuVSm.csv",row.names = F)
write.csv(dCpi, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_chat_cpiVSm.csv",row.names = F)

print(xtable(x = dU1, caption = 'Unconstrained Optimization, True Parameter',label = 'tab:dU1a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dU2, caption = 'Unconstrained Optimization, Noisy f',label = 'tab:dU2a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dC1, caption = 'Constrained Optimization, True Parameter',label = 'tab:dC1a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dC2, caption = 'Constrained Optimization, Noisy f',label = 'tab:dC2a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)

print(xtable(x = dUmu, caption = 'Unconstrained Optimization, Noisy mu and f',label = 'tab:dUmu'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dUpi, caption = 'Unconstrained Optimization, Noisy pi and f',label = 'tab:dUpi'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dCmu, caption = 'Constrained Optimization, Noisy mu and f',label = 'tab:dCmu'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dCpi, caption = 'Constrained Optimization, Noisy pi and f',label = 'tab:dCpi'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)

write.csv(cbind(K,matrix(unlist(psis.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/true_psi_bindingVSm.csv")
write.csv(cbind(K,matrix(unlist(psihat.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/allnoise_psihat_bindingVSm.csv")
write.csv(cbind(K,matrix(unlist(psihat2.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/fnoise_psihat_bindingVSm.csv")
write.csv(cbind(K,matrix(unlist(psiUs.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/true_psiU_bindingVSm.csv")
write.csv(cbind(K,matrix(unlist(psihatU.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/allnoise_notpsihatU_bindingVSm.csv")
write.csv(cbind(K,matrix(unlist(psihatU2.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/fnoise_psihatU_bindingVSm.csv")

write.csv(do.call("rbind", tabs), "~jacquelinemauro/Dropbox/sorter/approxSim/ftables_bindingVSm.csv")
write.csv(do.call("rbind", tabsU), "~jacquelinemauro/Dropbox/sorter/approxSim/fUtables_bindingVSm.csv")

out.constrained = data.frame(Rate = paste('N^(1/',round(K),')',sep = ""),
                             TrueConstrained = unlist(lapply(psis.mat, function(a) a[1])),
                             EstFConstrained = unlist(lapply(psihat2.mat, function(a) a[1])),
                             EstAllConstrained = unlist(lapply(psihat.mat, function(a) a[1])))
out.constrained$Lower95 = out.constrained$EstAllConstrained - 1.96*unlist(lapply(psihat.mat, function(a) a[2]))
out.constrained$Upper95 = out.constrained$EstAllConstrained + 1.96*unlist(lapply(psihat.mat, function(a) a[2]))
out.constrained[,-1] = round(100*out.constrained[,-1],2)
write.csv(out.constrained, "~jacquelinemauro/Dropbox/sorter/approxSim/outConstrained_bindingVSm.csv")

out.unconstrained = data.frame(Rate = paste('N^(1/',round(K),')',sep = ""),
                               TrueUnconstrained = unlist(lapply(psiUs.mat, function(a) a[1])),
                               EstFUnconstrained = unlist(lapply(psihatU2.mat, function(a) a[1])),
                               EstAllUnconstrained = unlist(lapply(psihatU.mat, function(a) a[1])))
out.unconstrained$Lower95 = out.unconstrained$EstAllUnconstrained - 1.96*unlist(lapply(psihatU.mat, function(a) a[2]))
out.unconstrained$Upper95 = out.unconstrained$EstAllUnconstrained + 1.96*unlist(lapply(psihatU.mat, function(a) a[2]))
out.unconstrained[,-1] = round(100*out.unconstrained[,-1],2)
write.csv(out.constrained, "~jacquelinemauro/Dropbox/sorter/approxSim/outUnconstrained_bindingVSm.csv")




### run binding constraint, even smaller effect version fluctuate mu/pi separately, negative emean ----
nsim= 100
K = c(1.99,2.99,3.99,5.99)
e.mean = -1
# produce all datasets
datlist <- lapply(c(1:length(K)), function(x) lapply(1:nsim, function(y) simFunc5(N=5000)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$K <- K[l]}}

# get true constrained f vector (f_c)
f.vecs <- lapply(datlist, function(y) lapply(y, function(m) get.f(m)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$f.vec <- f.vecs[[l]][[j]]}}

# get true constrained causal parameter (psi_c)
psis <- lapply(datlist, function(y) lapply(y, function(m) get.psi(D=m)))
psis.mat <- lapply(psis, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psi <- psis[[l]][[j]]}}

# get true unconstrained causal parameter (psi_u)
psiUs.mat <- psis.mat
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$psiU = mean(apply(datlist[[l]][[j]]$true.mumat,1,min))
    psiUs.mat[[l]] = c(datlist[[l]][[j]]$psiU,sd(apply(datlist[[l]][[j]]$true.mumat,1,min)))
  }
}

# get true unconstrained causal parameter using if
psiUIFs <- lapply(datlist, function(y) lapply(y, function(m) get.psiU.if(D=m)))
psiUIF.mat <- lapply(psiUIFs, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psiUIF <- psiUIFs[[l]][[j]]}}


# add error terms to mumat and pihat
etahat <- lapply(datlist, function(y) lapply(y, function(m) add.error(D=m, e.mean = e.mean)))
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$muhat <- etahat[[l]][[j]]$muhat
    datlist[[l]][[j]]$inv.pihat <- etahat[[l]][[j]]$inv.pihat
  }
}

# get fhat from noisy mumat and pihat (fhat_c)
fhats = f.vecs
for(i in 1:length(datlist)){
  for(j in 1:length(datlist[[i]])){
    datlist[[i]][[j]]$fhat = get.fhat(datlist[[i]][[j]])
    fhats[[i]][[j]] = datlist[[i]][[j]]$fhat
  }
}

# get unconstrained fhat from noisy mumat and pihat (fhat_u)
muhat.min = f.vecs
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){
  datlist[[l]][[j]]$muhat.min = apply(datlist[[l]][[j]]$muhat,1,which.min)
  muhat.min[[l]][[j]] = datlist[[l]][[j]]$muhat.min
}
}

# get estimated unconstrained psihat (psihat_u)
psihatUs <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU(D=m)))
psihatU.mat <- lapply(psihatUs, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU <- psihatUs[[l]][[j]]}}

# get estimated unconstrained psihat, only mu noisy
psihatU.mus <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU.mu(D=m)))
psihatU.mu.mat <- lapply(psihatU.mus, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU.mu <- psihatU.mus[[l]][[j]]}}

# get estimated unconstrained psihat, only pi noisy
psihatU.pis <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU.pi(D=m)))
psihatU.pi.mat <- lapply(psihatU.pis, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU.pi <- psihatU.pis[[l]][[j]]}}

# get estimated psihat -- f, mu and pi all noisy (psihat_c)
psihats <- lapply(datlist, function(y) lapply(y, function(m) get.psihat(D=m)))
psihat.mat <- lapply(psihats, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat <- psihats[[l]][[j]]}}

# get estimated psihat -- f, mu noisy, pi true
psihat.pis <- lapply(datlist, function(y) lapply(y, function(m) get.psihat.pi(D=m)))
psihat.pi.mat <- lapply(psihat.pis, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat.pi <- psihat.pis[[l]][[j]]}}

# get estimated psihat -- f, pi noisy, mu true
psihat.mus <- lapply(datlist, function(y) lapply(y, function(m) get.psihat.mu(D=m)))
psihat.mu.mat <- lapply(psihat.mus, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat.mu <- psihat.mus[[l]][[j]]}}


# get estimated unconstrained psihat tilde -- f noisy, mu and pi true
psihatU2s <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU2(D=m)))
psihatU2.mat <- lapply(psihatU2s, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU2 <- psihatU2s[[l]][[j]]}}

# get estimated constrained psihat tilde -- f noisy, mu and pi true
psihat2s <- lapply(datlist, function(y) lapply(y, function(m) get.psihat2(D=m)))
psihat2.mat <- lapply(psihat2s, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat2 <- psihat2s[[l]][[j]]}}

# output summary statistics
rmse_uhat_u <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psiU - j$psihatU$est)^2 )))))),4)
rmse_uhat_utilde <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihatU$est - j$psihatU2$est)^2 )))))),4)
rmse_chat_c <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psi$effect - j$psihat$est)^2 )))))),4)
rmse_chat_ctilde <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihat$est - j$psihatU2$est)^2 )))))),4)

rmse_uhat_u_mu <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psiU - j$psihatU.mu$est)^2 )))))),4)
rmse_uhat_u_pi <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psiU - j$psihatU.pi$est)^2 )))))),4)
rmse_chat_c_mu <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psi$effect - j$psihat.mu$est)^2 )))))),4)
rmse_chat_c_pi <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psi$effect - j$psihat.pi$est)^2 )))))),4)


bias_uhat_u <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psiU - j$psihatU$est) )))))),4)
bias_uhat_utilde <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihatU$est - j$psihatU2$est) )))))),4)
bias_chat_c <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psi$effect - j$psihat$est) )))))),4)
bias_chat_ctilde <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihat$est - j$psihatU2$est) )))))),4)

bias_uhat_u_mu <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psiU - j$psihatU.mu$est) )))))),4)
bias_uhat_u_pi <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psiU - j$psihatU.pi$est) )))))),4)
bias_chat_c_mu <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psi$effect - j$psihat.mu$est) )))))),4)
bias_chat_c_pi <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psi$effect - j$psihat.pi$est) )))))),4)

coverage_uhat_u <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psiU) & (j$psihatU$est + 1.96*j$psihatU$sd >= j$psiU) ))))))
coverage_uhat_utilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psihatU2$est) & (j$psihatU$est + 1.96*j$psihatU$sd >= j$psihatU2$est) ))))))
coverage_chat_c <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat$est - 1.96*j$psihat$sd <= j$psi$effect) & (j$psihat$est + 1.96*j$psihat$sd >= j$psi$effect) ))))))
coverage_chat_ctilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat$est - 1.96*j$psihat$sd <= j$psihat2$est) & (j$psihat$est + 1.96*j$psihat$sd >= j$psihat2$est) ))))))

coverage_uhat_u_mu <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU.mu$est - 1.96*j$psihatU.mu$sd <= j$psiU) & (j$psihatU.mu$est + 1.96*j$psihatU.mu$sd >= j$psiU) ))))))
coverage_uhat_u_pi <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU.pi$est - 1.96*j$psihatU.pi$sd <= j$psiU) & (j$psihatU.pi$est + 1.96*j$psihatU.pi$sd >= j$psiU) ))))))
coverage_chat_c_mu <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat.mu$est - 1.96*j$psihat.mu$sd <= j$psi$effect) & (j$psihat.mu$est + 1.96*j$psihat.mu$sd >= j$psi$effect) ))))))
coverage_chat_c_pi <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat.pi$est - 1.96*j$psihat.pi$sd <= j$psi$effect) & (j$psihat.pi$est + 1.96*j$psihat.pi$sd >= j$psi$effect) ))))))

power_uhat_u <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihatU$est + 1.96*j$psihatU$sd < mean(j$data$y) ))))))
power_chat_c <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihat$est + 1.96*j$psihat$sd < mean(j$data$y)))))))

power_uhat_u_mu <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihatU.mu$est + 1.96*j$psihatU.mu$sd < mean(j$data$y)  ))))))
power_uhat_u_pi <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihatU.pi$est + 1.96*j$psihatU.pi$sd < mean(j$data$y)  ))))))
power_chat_c_mu <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihat.mu$est + 1.96*j$psihat.mu$sd < mean(j$data$y) ))))))
power_chat_c_pi <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihat.pi$est + 1.96*j$psihat.pi$sd < mean(j$data$y) ))))))



f.correct = round(unlist(lapply(datlist, function(l) mean(unlist(lapply(l, function(k) mean(k$f.vec == k$fhat)))))),4)
fU.correct = round(unlist(lapply(datlist, function(l) mean(unlist(lapply(l, function(k) mean(k$muhat.min == k$a.star)))))),4)

tabs <- tabsU <- list()
for(i in 1:length(datlist)){tabs[[i]] = round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), fhats[[i]], f.vecs[[i]]),1,mean),ncol=3),5)}
#for(i in 1:length(datlist)){tabsU[[i]] = round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), dat.list[[i]], mu.min[[i]]),1,mean),ncol=3),5)}



dU1 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psiUs.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihatU.mat, function(a) a[1])),4),
                 rmse_uhat_u,bias_uhat_u,coverage_uhat_u,fU.correct,
                 power_uhat_u)
dUmu = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                  round(unlist(lapply(psiUs.mat, function(a) a[1])),4),
                  round(unlist(lapply(psihatU.mu.mat, function(a) a[1])),4),
                  rmse_uhat_u_mu,bias_uhat_u_mu,coverage_uhat_u_mu,fU.correct,
                  power_uhat_u_mu)
dUpi = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                  round(unlist(lapply(psiUs.mat, function(a) a[1])),4),
                  round(unlist(lapply(psihatU.pi.mat, function(a) a[1])),4),
                  rmse_uhat_u_pi,bias_uhat_u_pi,coverage_uhat_u_pi,fU.correct,
                  power_uhat_u_pi)
dU2 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psihatU2.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihatU.mat, function(a) a[1])),4),
                 rmse_uhat_utilde,bias_uhat_utilde,coverage_uhat_utilde,fU.correct,
                 power_uhat_u)
dC1 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psis.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihat.mat, function(a) a[1])),4),
                 rmse_chat_c,bias_chat_c,coverage_chat_c,f.correct,
                 power_chat_c)
dCmu = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                  round(unlist(lapply(psis.mat, function(a) a[1])),4),
                  round(unlist(lapply(psihat.mu.mat, function(a) a[1])),4),
                  rmse_chat_c_mu,bias_chat_c_mu,coverage_chat_c_mu,f.correct,
                  power_chat_c_mu)
dCpi = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                  round(unlist(lapply(psis.mat, function(a) a[1])),4),
                  round(unlist(lapply(psihat.pi.mat, function(a) a[1])),4),
                  rmse_chat_c_pi,bias_chat_c_pi,coverage_chat_c_pi,f.correct,
                  power_chat_c_pi)
dC2 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psihat2.mat, function(a) a[1])),2),
                 round(unlist(lapply(psihat.mat, function(a) a[1])),2),
                 rmse_chat_ctilde,bias_chat_ctilde,coverage_chat_ctilde,f.correct,
                 power_chat_c)
names(dU1) <- names(dU2) <- names(dC1) <- names(dC2) <- c('Rate','$\\psi$', '\\hat{\\psi}', 'RMSE', 'Bias', '95% Coverage', '$f = \\hat{f}$','Excludes \bar{Y}')
names(dUmu) <- names(dUpi) <- names(dCmu) <- names(dCpi) <- names(dU1)
write.csv(dU1, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_uhat_uVSmNeg.csv",row.names = F)
write.csv(dU2, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_uhat_utildeVsmNeg.csv",row.names = F)
write.csv(dC1, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_chat_cVsmNeg.csv",row.names = F)
write.csv(dC2, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_chat_ctildeVsmNeg.csv",row.names = F)

write.csv(dUmu, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_uhat_umuVsmNeg.csv",row.names = F)
write.csv(dUpi, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_uhat_upiVsmNeg.csv",row.names = F)
write.csv(dCmu, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_chat_cmuVsmNeg.csv",row.names = F)
write.csv(dCpi, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_chat_cpiVsmNeg.csv",row.names = F)

print(xtable(x = dU1, caption = 'Unconstrained Optimization, True Parameter',label = 'tab:dU1a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dU2, caption = 'Unconstrained Optimization, Noisy f',label = 'tab:dU2a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dC1, caption = 'Constrained Optimization, True Parameter',label = 'tab:dC1a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dC2, caption = 'Constrained Optimization, Noisy f',label = 'tab:dC2a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)

print(xtable(x = dUmu, caption = 'Unconstrained Optimization, Noisy mu and f',label = 'tab:dUmu'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dUpi, caption = 'Unconstrained Optimization, Noisy pi and f',label = 'tab:dUpi'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dCmu, caption = 'Constrained Optimization, Noisy mu and f',label = 'tab:dCmu'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dCpi, caption = 'Constrained Optimization, Noisy pi and f',label = 'tab:dCpi'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)

write.csv(cbind(K,matrix(unlist(psis.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/true_psi_bindingVsmNeg.csv")
write.csv(cbind(K,matrix(unlist(psihat.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/allnoise_psihat_bindingVsmNeg.csv")
write.csv(cbind(K,matrix(unlist(psihat2.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/fnoise_psihat_bindingVsmNeg.csv")
write.csv(cbind(K,matrix(unlist(psiUs.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/true_psiU_bindingVsmNeg.csv")
write.csv(cbind(K,matrix(unlist(psihatU.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/allnoise_notpsihatU_bindingVsmNeg.csv")
write.csv(cbind(K,matrix(unlist(psihatU2.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/fnoise_psihatU_bindingVsmNeg.csv")

write.csv(do.call("rbind", tabs), "~jacquelinemauro/Dropbox/sorter/approxSim/ftables_bindingVsmNeg.csv")
write.csv(do.call("rbind", tabsU), "~jacquelinemauro/Dropbox/sorter/approxSim/fUtables_bindingVsmNeg.csv")

out.constrained = data.frame(Rate = paste('N^(1/',round(K),')',sep = ""),
                             TrueConstrained = unlist(lapply(psis.mat, function(a) a[1])),
                             EstFConstrained = unlist(lapply(psihat2.mat, function(a) a[1])),
                             EstAllConstrained = unlist(lapply(psihat.mat, function(a) a[1])))
out.constrained$Lower95 = out.constrained$EstAllConstrained - 1.96*unlist(lapply(psihat.mat, function(a) a[2]))
out.constrained$Upper95 = out.constrained$EstAllConstrained + 1.96*unlist(lapply(psihat.mat, function(a) a[2]))
out.constrained[,-1] = round(100*out.constrained[,-1],2)
write.csv(out.constrained, "~jacquelinemauro/Dropbox/sorter/approxSim/outConstrained_bindingVsmNeg.csv")

out.unconstrained = data.frame(Rate = paste('N^(1/',round(K),')',sep = ""),
                               TrueUnconstrained = unlist(lapply(psiUs.mat, function(a) a[1])),
                               EstFUnconstrained = unlist(lapply(psihatU2.mat, function(a) a[1])),
                               EstAllUnconstrained = unlist(lapply(psihatU.mat, function(a) a[1])))
out.unconstrained$Lower95 = out.unconstrained$EstAllUnconstrained - 1.96*unlist(lapply(psihatU.mat, function(a) a[2]))
out.unconstrained$Upper95 = out.unconstrained$EstAllUnconstrained + 1.96*unlist(lapply(psihatU.mat, function(a) a[2]))
out.unconstrained[,-1] = round(100*out.unconstrained[,-1],2)
write.csv(out.constrained, "~jacquelinemauro/Dropbox/sorter/approxSim/outUnconstrained_bindingVsmNeg.csv")



### run binding constraint, large effect version fluctuate mu/pi separately, negative emean, 3-piece sample splitting ----
nsim= 100
K = c(1.99,2.99,3.99,5.99)
e.mean = -1
# produce all datasets
datlist <- lapply(c(1:length(K)), function(x) lapply(1:nsim, function(y) simFunc5(N=5000)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$K <- K[l]}}

# get true constrained f vector (f_c)
f.vecs <- lapply(datlist, function(y) lapply(y, function(m) get.f(m)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$f.vec <- f.vecs[[l]][[j]]}}

# get true constrained causal parameter (psi_c)
psis <- lapply(datlist, function(y) lapply(y, function(m) get.psi(D=m)))
psis.mat <- lapply(psis, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psi <- psis[[l]][[j]]}}

# get true unconstrained causal parameter (psi_u)
psiUs.mat <- psis.mat
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$psiU = mean(apply(datlist[[l]][[j]]$true.mumat,1,min))
    psiUs.mat[[l]] = c(datlist[[l]][[j]]$psiU,sd(apply(datlist[[l]][[j]]$true.mumat,1,min)))
  }
}

# add error terms to mumat and pihat
etahat <- lapply(datlist, function(y) lapply(y, function(m) add.error2(D=m, e.mean = -1)))
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$muhat <- etahat[[l]][[j]]$muhat
    datlist[[l]][[j]]$inv.pihat <- etahat[[l]][[j]]$inv.pihat
    datlist[[l]][[j]]$muhat2 <- etahat[[l]][[j]]$muhat2
  }
}

# get fhat from noisy mumat and pihat (fhat_c)
fhats = f.vecs
for(i in 1:length(datlist)){
  for(j in 1:length(datlist[[i]])){
    datlist[[i]][[j]]$fhat = get.fhat(datlist[[i]][[j]])
    fhats[[i]][[j]] = datlist[[i]][[j]]$fhat
  }
}

# get unconstrained fhat from noisy mumat and pihat (fhat_u)
muhat.min = f.vecs
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){
  datlist[[l]][[j]]$muhat.min = apply(datlist[[l]][[j]]$muhat,1,which.min)
  muhat.min[[l]][[j]] = datlist[[l]][[j]]$muhat.min
}
}
# triple split version
muhat.min2 = f.vecs
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){
  datlist[[l]][[j]]$muhat.min2 = apply(datlist[[l]][[j]]$muhat2,1,which.min)
  muhat.min2[[l]][[j]] = datlist[[l]][[j]]$muhat.min2
}
}

# get estimated unconstrained psihat (psihat_u)
psihatUs <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU(D=m)))
psihatU.mat <- lapply(psihatUs, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU <- psihatUs[[l]][[j]]}}

# get estimated unconstrained psihat, only mu noisy
psihatU.mus <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU.mu(D=m)))
psihatU.mu.mat <- lapply(psihatU.mus, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU.mu <- psihatU.mus[[l]][[j]]}}

# get estimated unconstrained psihat, only pi noisy
psihatU.pis <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU.pi(D=m)))
psihatU.pi.mat <- lapply(psihatU.pis, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU.pi <- psihatU.pis[[l]][[j]]}}

# get estimated unconstrained psihat in triple split case (psihat_u_split)
psihatUTs <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU(D=m, type = 'triple')))
psihatUT.mat <- lapply(psihatUTs, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatUT <- psihatUTs[[l]][[j]]}}

# get estimated unconstrained psihat in triple split case, only mu noisy
psihatUT.mus <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU.mu(D=m, type = 'triple')))
psihatUT.mu.mat <- lapply(psihatUT.mus, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatUT.mu <- psihatUT.mus[[l]][[j]]}}

# get estimated unconstrained psihat in triple split case, only pi noisy
psihatUT.pis <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU.pi(D=m,type = 'triple')))
psihatUT.pi.mat <- lapply(psihatUT.pis, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatUT.pi <- psihatUT.pis[[l]][[j]]}}


# get estimated psihat -- f, mu and pi all noisy (psihat_c)
psihats <- lapply(datlist, function(y) lapply(y, function(m) get.psihat(D=m)))
psihat.mat <- lapply(psihats, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat <- psihats[[l]][[j]]}}

# get estimated psihat -- f, mu noisy, pi true
psihat.pis <- lapply(datlist, function(y) lapply(y, function(m) get.psihat.pi(D=m)))
psihat.pi.mat <- lapply(psihat.pis, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat.pi <- psihat.pis[[l]][[j]]}}

# get estimated psihat -- f, pi noisy, mu true
psihat.mus <- lapply(datlist, function(y) lapply(y, function(m) get.psihat.mu(D=m)))
psihat.mu.mat <- lapply(psihat.mus, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat.mu <- psihat.mus[[l]][[j]]}}


# get estimated unconstrained psihat tilde -- f noisy, mu and pi true
psihatU2s <- lapply(datlist, function(y) lapply(y, function(m) get.psihatU2(D=m)))
psihatU2.mat <- lapply(psihatU2s, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihatU2 <- psihatU2s[[l]][[j]]}}

# get estimated constrained psihat tilde -- f noisy, mu and pi true
psihat2s <- lapply(datlist, function(y) lapply(y, function(m) get.psihat2(D=m)))
psihat2.mat <- lapply(psihat2s, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat2 <- psihat2s[[l]][[j]]}}

# output summary statistics
rmse_uhat_u <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psiU - j$psihatU$est)^2 )))))),4)
rmse_uhat_utilde <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihatU$est - j$psihatU2$est)^2 )))))),4)
rmse_uThat_u <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psiU - j$psihatUT$est)^2 )))))),4)
rmse_uThat_utilde <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihatUT$est - j$psihatU2$est)^2 )))))),4)
rmse_chat_c <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psi$effect - j$psihat$est)^2 )))))),4)
rmse_chat_ctilde <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psihat$est - j$psihatU2$est)^2 )))))),4)

rmse_uhat_u_mu <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psiU - j$psihatU.mu$est)^2 )))))),4)
rmse_uhat_u_pi <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psiU - j$psihatU.pi$est)^2 )))))),4)
rmse_uThat_u_mu <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psiU - j$psihatUT.mu$est)^2 )))))),4)
rmse_uThat_u_pi <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psiU - j$psihatUT.pi$est)^2 )))))),4)
rmse_chat_c_mu <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psi$effect - j$psihat.mu$est)^2 )))))),4)
rmse_chat_c_pi <- round(unlist(lapply(datlist, function(k) sqrt(sum(unlist(lapply(k, function(j) (j$psi$effect - j$psihat.pi$est)^2 )))))),4)


bias_uhat_u <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psiU - j$psihatU$est) )))))),4)
bias_uhat_utilde <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihatU$est - j$psihatU2$est) )))))),4)
bias_uThat_u <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psiU - j$psihatUT$est) )))))),4)
bias_uThat_utilde <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihatUT$est - j$psihatU2$est) )))))),4)
bias_chat_c <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psi$effect - j$psihat$est) )))))),4)
bias_chat_ctilde <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psihat$est - j$psihatU2$est) )))))),4)

bias_uhat_u_mu <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psiU - j$psihatU.mu$est) )))))),4)
bias_uhat_u_pi <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psiU - j$psihatU.pi$est) )))))),4)
bias_uThat_u_mu <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psiU - j$psihatUT.mu$est) )))))),4)
bias_uThat_u_pi <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psiU - j$psihatUT.pi$est) )))))),4)
bias_chat_c_mu <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psi$effect - j$psihat.mu$est) )))))),4)
bias_chat_c_pi <- round(unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) abs(j$psi$effect - j$psihat.pi$est) )))))),4)

coverage_uhat_u <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psiU) & (j$psihatU$est + 1.96*j$psihatU$sd >= j$psiU) ))))))
coverage_uhat_utilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psihatU2$est) & (j$psihatU$est + 1.96*j$psihatU$sd >= j$psihatU2$est) ))))))
coverage_uThat_u <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psiU) & (j$psihatUT$est + 1.96*j$psihatUT$sd >= j$psiU) ))))))
coverage_uThat_utilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU$est - 1.96*j$psihatU$sd <= j$psihatU2$est) & (j$psihatUT$est + 1.96*j$psihatUT$sd >= j$psihatU2$est) ))))))
coverage_chat_c <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat$est - 1.96*j$psihat$sd <= j$psi$effect) & (j$psihat$est + 1.96*j$psihat$sd >= j$psi$effect) ))))))
coverage_chat_ctilde <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat$est - 1.96*j$psihat$sd <= j$psihat2$est) & (j$psihat$est + 1.96*j$psihat$sd >= j$psihat2$est) ))))))

coverage_uhat_u_mu <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU.mu$est - 1.96*j$psihatU.mu$sd <= j$psiU) & (j$psihatU.mu$est + 1.96*j$psihatU.mu$sd >= j$psiU) ))))))
coverage_uhat_u_pi <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU.pi$est - 1.96*j$psihatU.pi$sd <= j$psiU) & (j$psihatU.pi$est + 1.96*j$psihatU.pi$sd >= j$psiU) ))))))
coverage_uThat_u_mu <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU.mu$est - 1.96*j$psihatU.mu$sd <= j$psiU) & (j$psihatUT.mu$est + 1.96*j$psihatUT.mu$sd >= j$psiU) ))))))
coverage_uThat_u_pi <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihatU.pi$est - 1.96*j$psihatU.pi$sd <= j$psiU) & (j$psihatUT.pi$est + 1.96*j$psihatUT.pi$sd >= j$psiU) ))))))
coverage_chat_c_mu <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat.mu$est - 1.96*j$psihat.mu$sd <= j$psi$effect) & (j$psihat.mu$est + 1.96*j$psihat.mu$sd >= j$psi$effect) ))))))
coverage_chat_c_pi <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) (j$psihat.pi$est - 1.96*j$psihat.pi$sd <= j$psi$effect) & (j$psihat.pi$est + 1.96*j$psihat.pi$sd >= j$psi$effect) ))))))

power_uhat_u <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihatU$est + 1.96*j$psihatU$sd < mean(j$data$y) ))))))
power_uThat_u <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihatUT$est + 1.96*j$psihatUT$sd < mean(j$data$y) ))))))
power_chat_c <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihat$est + 1.96*j$psihat$sd < mean(j$data$y)))))))

power_uhat_u_mu <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihatU.mu$est + 1.96*j$psihatU.mu$sd < mean(j$data$y)  ))))))
power_uhat_u_pi <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihatU.pi$est + 1.96*j$psihatU.pi$sd < mean(j$data$y)  ))))))
power_uThat_u_mu <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihatUT.mu$est + 1.96*j$psihatUT.mu$sd < mean(j$data$y)  ))))))
power_uThat_u_pi <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihatUT.pi$est + 1.96*j$psihatUT.pi$sd < mean(j$data$y)  ))))))
power_chat_c_mu <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihat.mu$est + 1.96*j$psihat.mu$sd < mean(j$data$y) ))))))
power_chat_c_pi <- unlist(lapply(datlist, function(k) (mean(unlist(lapply(k, function(j) j$psihat.pi$est + 1.96*j$psihat.pi$sd < mean(j$data$y) ))))))

f.correct = round(unlist(lapply(datlist, function(l) mean(unlist(lapply(l, function(k) mean(k$f.vec == k$fhat)))))),4)
fU.correct = round(unlist(lapply(datlist, function(l) mean(unlist(lapply(l, function(k) mean(k$muhat.min == k$a.star)))))),4)
fUT.correct = round(unlist(lapply(datlist, function(l) mean(unlist(lapply(l, function(k) mean(k$muhat.min2 == k$a.star)))))),4)

tabs <- list()
for(i in 1:length(datlist)){tabs[[i]] = round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), fhats[[i]], f.vecs[[i]]),1,mean),ncol=3),5)}
#for(i in 1:length(datlist)){tabsU[[i]] = round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), dat.list[[i]], mu.min[[i]]),1,mean),ncol=3),5)}



dU1 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psiUs.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihatU.mat, function(a) a[1])),4),
                 rmse_uhat_u,bias_uhat_u,coverage_uhat_u,fU.correct,
                 power_uhat_u)
dUmu = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                  round(unlist(lapply(psiUs.mat, function(a) a[1])),4),
                  round(unlist(lapply(psihatU.mu.mat, function(a) a[1])),4),
                  rmse_uhat_u_mu,bias_uhat_u_mu,coverage_uhat_u_mu,fU.correct,
                  power_uhat_u_mu)
dUpi = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                  round(unlist(lapply(psiUs.mat, function(a) a[1])),4),
                  round(unlist(lapply(psihatU.pi.mat, function(a) a[1])),4),
                  rmse_uhat_u_pi,bias_uhat_u_pi,coverage_uhat_u_pi,fU.correct,
                  power_uhat_u_pi)
dU2 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psihatU2.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihatU.mat, function(a) a[1])),4),
                 rmse_uhat_utilde,bias_uhat_utilde,coverage_uhat_utilde,fU.correct,
                 power_uhat_u)
dC1 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psis.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihat.mat, function(a) a[1])),4),
                 rmse_chat_c,bias_chat_c,coverage_chat_c,f.correct,
                 power_chat_c)
dCmu = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                  round(unlist(lapply(psis.mat, function(a) a[1])),4),
                  round(unlist(lapply(psihat.mu.mat, function(a) a[1])),4),
                  rmse_chat_c_mu,bias_chat_c_mu,coverage_chat_c_mu,f.correct,
                  power_chat_c_mu)
dCpi = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                  round(unlist(lapply(psis.mat, function(a) a[1])),4),
                  round(unlist(lapply(psihat.pi.mat, function(a) a[1])),4),
                  rmse_chat_c_pi,bias_chat_c_pi,coverage_chat_c_pi,f.correct,
                  power_chat_c_pi)
dC2 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psihat2.mat, function(a) a[1])),2),
                 round(unlist(lapply(psihat.mat, function(a) a[1])),2),
                 rmse_chat_ctilde,bias_chat_ctilde,coverage_chat_ctilde,f.correct,
                 power_chat_c)
dUT1 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psiUs.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihatUT.mat, function(a) a[1])),4),
                 rmse_uThat_u,bias_uThat_u,coverage_uThat_u,fUT.correct,
                 power_uThat_u)
dUTmu = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                  round(unlist(lapply(psiUs.mat, function(a) a[1])),4),
                  round(unlist(lapply(psihatUT.mu.mat, function(a) a[1])),4),
                  rmse_uThat_u_mu,bias_uThat_u_mu,coverage_uThat_u_mu,fUT.correct,
                  power_uThat_u_mu)
dUTpi = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                  round(unlist(lapply(psiUs.mat, function(a) a[1])),4),
                  round(unlist(lapply(psihatUT.pi.mat, function(a) a[1])),4),
                  rmse_uThat_u_pi,bias_uThat_u_pi,coverage_uThat_u_pi,fUT.correct,
                  power_uThat_u_pi)
dUT2 = data.frame(paste('$N^(1/',round(K),')$',sep = ""),
                 round(unlist(lapply(psihatU2.mat, function(a) a[1])),4),
                 round(unlist(lapply(psihatUT.mat, function(a) a[1])),4),
                 rmse_uThat_utilde,bias_uThat_utilde,coverage_uThat_utilde,fUT.correct,
                 power_uThat_u)

names(dU1) <- names(dU2) <- names(dC1) <- names(dC2) <- c('Rate','$\\psi$', '\\hat{\\psi}', 'RMSE', 'Bias', '95% Coverage', '$f = \\hat{f}$','Excludes \bar{Y}')
names(dUmu) <- names(dUpi) <- names(dCmu) <- names(dCpi) <- names(dU1)
names(dUT1) <- names(dUT2) <- names(dUTpi) <- names(dUTmu) <- names(dU1)
write.csv(dU1, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_uhat_uNeg.csv",row.names = F)
write.csv(dU2, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_uhat_utildeNeg.csv",row.names = F)
write.csv(dC1, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_chat_cNeg.csv",row.names = F)
write.csv(dC2, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_chat_ctildeNeg.csv",row.names = F)

write.csv(dUmu, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_uhat_umuNeg.csv",row.names = F)
write.csv(dUpi, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_uhat_upiNeg.csv",row.names = F)
write.csv(dCmu, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_chat_cmuNeg.csv",row.names = F)
write.csv(dCpi, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_chat_cpiNeg.csv",row.names = F)

write.csv(dUT1, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_uhat_uNeg.csv",row.names = F)
write.csv(dUT2, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_uhat_utildeNeg.csv",row.names = F)
write.csv(dUTmu, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_chat_cNeg.csv",row.names = F)
write.csv(dUTpi, "~jacquelinemauro/Dropbox/sorter/approxSim/sumstats_binding_chat_ctildeNeg.csv",row.names = F)

print(xtable(x = dU1, caption = 'Unconstrained Optimization, True Parameter',label = 'tab:dU1a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dU2, caption = 'Unconstrained Optimization, Noisy f',label = 'tab:dU2a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dC1, caption = 'Constrained Optimization, True Parameter',label = 'tab:dC1a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dC2, caption = 'Constrained Optimization, Noisy f',label = 'tab:dC2a'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)

print(xtable(x = dUmu, caption = 'Unconstrained Optimization, Noisy mu and f',label = 'tab:dUmu'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dUpi, caption = 'Unconstrained Optimization, Noisy pi and f',label = 'tab:dUpi'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dCmu, caption = 'Constrained Optimization, Noisy mu and f',label = 'tab:dCmu'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)
print(xtable(x = dCpi, caption = 'Constrained Optimization, Noisy pi and f',label = 'tab:dCpi'), type = 'latex', sanitize.text.function = function(x){x}, include.rownames = F)

write.csv(cbind(K,matrix(unlist(psis.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/true_psi_bindingNeg.csv")
write.csv(cbind(K,matrix(unlist(psihat.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/allnoise_psihat_bindingNeg.csv")
write.csv(cbind(K,matrix(unlist(psihat2.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/fnoise_psihat_bindingNeg.csv")
write.csv(cbind(K,matrix(unlist(psiUs.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/true_psiU_bindingNeg.csv")
write.csv(cbind(K,matrix(unlist(psihatU.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/allnoise_notpsihatU_bindingNeg.csv")
write.csv(cbind(K,matrix(unlist(psihatU2.mat),ncol=2, byrow = T)), "~jacquelinemauro/Dropbox/sorter/approxSim/fnoise_psihatU_bindingNeg.csv")

write.csv(do.call("rbind", tabs), "~jacquelinemauro/Dropbox/sorter/approxSim/ftables_bindingNeg.csv")


