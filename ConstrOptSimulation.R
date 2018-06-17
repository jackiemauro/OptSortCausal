# attempt to set up a simulation for the approximate constrained procedure
# not going to attempt to set up something where i know the exact answer
# will instead output true mu,pi terms, pass through optimizer and add noise

rm(list = ls())
library(ggplot2)

simFunc <- function(N=5000, psi = c(1,2,3)){
  meanx = c(0,0,0)
  alpha = matrix(c(1,1,-1),nrow = 3)
  x = matrix(unlist(lapply(meanx, function(x) rnorm(N,x,1))), nrow = N, byrow =T)
  Pr = matrix(rep(1/3,N*3),ncol=3)
  a = sapply(c(1:N), function(y) sample(c(1:3), size = 1, replace = T, prob = Pr[y,]))
  y = diag((x+psi) %*% t(model.matrix(~as.factor(a)-1)))

  ####### calculate true mu mat ######
  # If pr = 1/3 all the time
  #E(y) = E(y0 + (psi1+x1)*I(a==1) + (psi2+x2)*I(a==2) + (psi3+x3)*I(a==3))
  #     = E((psi1+x1)*I(a==1) + (psi2+x2)*I(a==2) + (psi3+x3)*I(a==3))
  #E((psi1+x1)*I(a==1)) = E(psi1*I(a==1) + x1*I(a==1))
  #              = psi1*P(a==1) + E[E(x1*I(a==1)|x1)]
  #              = psi1/3 + E[x1*P(a==1|x1)]
  #              = psi1/3 + E[x1]/3
  #              = psi1/3
  true.mu = psi*Pr

  # If pr depends on x -- need to figure out a good one for this
  #E(y) = E(y0 + (psi1+x1)*I(a==1) + (psi2+x2)*I(a==2) + (psi3+x3)*I(a==3))
  #     = E((psi1+x1)*I(a==1) + (psi2+x2)*I(a==2) + (psi3+x3)*I(a==3))
  #E((psi1+x1)*I(a==1)) = E(psi1*I(a==1) + x1*I(a==1))
  #              = psi1*E[P(a==1|x1)] + E[E(x1*I(a==1)|x1)]
  #              = psi1*E[P(a==1|x1)] + E[x1*P(a==1|x1)]
  # (eg)         = psi1*E[0.2*I(x1>.5) + .6*I(x1<=.5)] + E[x1*0.2*I(x1>.5) + x1*.6*I(x1<=.5)]
  #              = psi1*(0.2*P(x1>.5) + .6*P(x1<=.5)) + E[x1*0.2*I(x1>.5) + x1*.6*I(x1<=.5)]

  return(list( data = data.frame(y,a,x),true.mumat = true.mu, true.pimat=Pr))
}

if.func <- function(D){
  df = D$df; mu.pred = D$mu.pred; fhat = D$fhat; Avals = D$Avals; phat = D$phat
  mu.hat = diag(mu.pred %*% t(model.matrix(~Avals[fhat]-1))) + (rnorm(N,e.mean,1)/B)
  ratM = (as.numeric(df$A == fhat)/phat) + (rnorm(N,e.mean,1)/B)
  ratM * (df$y - mu.hat) + mu.hat
}

### run some simulations adding various noise (not run) ----
nsim = 10
K = c(1.99,2.99,3.99,5.99)
psi <- true.eff <- c(1,2,3)
N = 5000 # size of dataset
e.mean <- 2 # mean of error term

# produce all datasets
datlist <- lapply(c(1:length(K)), function(x) lapply(1:nsim, function(y) simFunc(N=5000)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$K <- K[l]}}

# get true f vector
# it's probably better to write a matlab script that will do all the constraining at once
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
f.vecs <- lapply(datlist, function(y) lapply(y, function(m) get.f(m)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$f.vec <- f.vecs[[l]][[j]]}}

# get true causal parameter
get.psi <- function(D){
  dat = D$data; mumat = D$true.mumat; pimat = D$true.pimat; f.vec = D$f.vec
  Avals = sort(unique(dat$a))
  temp = sapply(Avals, function(x) as.numeric(Avals[f.vec] == x))
  mu.hat = diag(mumat %*% t(temp))
  pi.hat = diag(pimat %*% t(temp))
  ifs = (as.numeric(dat$a == f.vec)/pi.hat) * (dat$y - mu.hat) + mu.hat
  return(list(effect = mean(ifs), sd = sd(ifs)/sqrt(length(f.vec))))
}
psis <- lapply(datlist, function(y) lapply(y, function(m) get.psi(D=m)))
psis.mat <- lapply(psis, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psi <- psis[[l]][[j]]}}

# add error terms to mumat and pihat
add.error <- function(D, e.mean){
  N = dim(D$true.mumat)[1]; B = N^(1/D$K); num.a = length(unique(D$data$a))
  errs <- matrix(rnorm(N*2*num.a,e.mean,1)/B, ncol = 2*num.a)
  muhat <- D$true.mumat + errs[,1:num.a]
  pihat <- D$true.pimat + errs[,(num.a+1):(2*num.a)]
  return(list(muhat = muhat, pihat = pihat))
}
etahat <- lapply(datlist, function(y) lapply(y, function(m) add.error(D=m, e.mean = 2)))
for(l in 1:length(datlist)){
  for(j in 1:length(datlist[[l]])){
    datlist[[l]][[j]]$muhat <- etahat[[l]][[j]]$muhat
    datlist[[l]][[j]]$pihat <- etahat[[l]][[j]]$pihat
  }
}

# get fhat from noisy mumat and pihat
# again, it's probably better to write a matlab script that will do all the constraining at once
get.fhat <- function(D){
  constr <- as.numeric(round(table(sort(D$data$a))*1.05))
  Avals <- sort(unique(D$data$a))

  ### send muhat matrix to matlab, constraint for constrained optimization
  write.csv(constr, "~jacquelinemauro/Dropbox/sorter/approxSim/optConstr.csv")
  write.csv(D$muhat, "~jacquelinemauro/Dropbox/sorter/approxSim/optMuhat.csv")
  run_matlab_script("~jacquelinemauro/Dropbox/sorter/approxSim/approxConstr.m")

  ### read assignment vector from matlab
  f.mat <- read.csv("~jacquelinemauro/Dropbox/sorter/optfhat.csv", header = F)
  f.con <- Avals[apply(f.mat,1,which.max)]
  f.con
}
fhats <- lapply(datlist, function(y) lapply(y, function(m) get.f(D)))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$fhat <- fhats[[l]][[j]]}}

# get estimated psihat
get.psihat <- function(D){
  dat = D$data; mumat = D$muhat; pimat = D$pihat; f.vec = D$fhat
  Avals = sort(unique(dat$a))
  temp = sapply(Avals, function(x) as.numeric(Avals[f.vec] == x))
  mu.hat = diag(mumat %*% t(temp))
  pi.hat = diag(pimat %*% t(temp))
  ifs = (as.numeric(dat$a == f.vec)/pi.hat) * (dat$y - mu.hat) + mu.hat
  return(list(est = mean(ifs), sd = sd(ifs)/sqrt(length(f.vec))))
}
psihats <- lapply(datlist, function(y) lapply(y, function(m) get.psihat(D=m)))
psihat.mat <- lapply(psihats, function(k) apply(matrix(unlist(k), nrow = 2),1,mean))
for(l in 1:length(datlist)){for(j in 1:length(datlist[[l]])){datlist[[l]][[j]]$psihat <- psihats[[l]][[j]]}}
