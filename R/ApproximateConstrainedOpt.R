#' Function for approximate constrained minimization
#' using named assignment vector -- not done
#'
#' @param df the dataframe. This should be organized as follows:
#' the outcome should be named "y", the treatment should be named "A",
#' the 3rd through final columns will be considered covariates.
#'
#' @param aLevel a named dataframe of treatment level covariates
#'
#' @param obsD a named vector containing the treatment level covariate
#' actually observed.
#'
#' @param nsplits number of sample splits, defaults to 2
#'
#' @param mu.algo algorithm to estimate mu nuisance parameters, either
#' superlearner or ranger
#'
#' @param pi.algo algorithm to estimate pi nuisance parameters, either
#' superlearner or ranger
#'
#' @param sl.lib library of superlearner algorithms defaults to:
#' c("SL.gam","SL.glm","SL.glmnet","SL.glm.interaction", "SL.mean","SL.ranger","SL.rpart")
#'
#' @return returns the estimate, sd, assignment vector and influence-function
#' values for the unconstrained optimization. Also returns the matrix of
#' influence function values that can be used in the constrained optimization
#' procedure.

approx.constr.opt.causal.nm <- function(df,aLevel,obsD,nsplits = 2,
                                 mu.algo = 'superlearner',
                                 pi.algo = 'superlearner',
                                 sl.lib = c("SL.gam","SL.glm","SL.glmnet","SL.glm.interaction", "SL.mean","SL.ranger","SL.rpart")){
  library('ranger')
  library('SuperLearner')
  library('randomForest')
  library('plyr')
  library("matlabr")
  options(matlab.path = "/Applications/MATLAB_R2018a.app/bin")
  n = dim(df)[1]
  Avals <- names(aLevel); n.avals <- length(Avals)

  s <- sample(rep(1:nsplits,ceiling(n/nsplits))[1:n])
  psihat <- sdhat <- rep(NA,2)
  assig.vec <- ifs <- est.assig.vec <- rep(NA,n)
  phihat <- out.mu <- out.muF <- out.pi <- matrix(rep(NA, n*n.avals), ncol = n.avals)
  for (vfold in 1:nsplits){
    train <- s!=vfold; test <- s==vfold
    if (nsplits==1){ train <- test }

    ### training and testing sets
    train.df <- df[train,]; train.df$obsD <- obsD[train]
    test.df <- df[test,]; test.df$obsD <- obsD[test]
    Xtrain <- train.df[,-c(1:2)]
    Xtest <- test.df[,-c(1:2)]
    aMat.train <- aLevel[train,]
    aMat.test <- aLevel[test,]

    ### train E(Y|A,X) & predict E(Y|A = a, X) for each a
    if(mu.algo == 'ranger'){
      list.out = lapply(Avals, function(a) mu.pred.rg.nm.ap(train.df = train.df, Xtrain = Xtrain, Xtest = Xtest, a.val = a,aMat.train=aMat.train, aMat.test=aMat.test))
      preds <- matrix(unlist(lapply(list.out, function(l) l[[1]])),ncol = length(Avals), byrow = F)
      muF <- matrix(unlist(lapply(list.out, function(l) l[[2]])),ncol = length(Avals), byrow = F)
    }
    if(mu.algo == 'superlearner'){
      list.out = lapply(Avals, function(a) mu.pred.sl.nm.ap(train.df = train.df, Xtrain = Xtrain, Xtest = Xtest, a.val = a,aMat.train=aMat.train, aMat.test=aMat.test))
      preds <- matrix(unlist(lapply(list.out, function(l) l[[1]])),ncol = length(Avals), byrow = F)
      muF <- matrix(unlist(lapply(list.out, function(l) l[[2]])),ncol = length(Avals), byrow = F)
    }

    ### train P(A|X) & predict P(A=a|X) for each a
    if(pi.algo == 'logistic'){
      list.out = lapply(Avals, function(a) pi.pred.lg.nm(a.val=a,train.df=train.df,Xtrain=Xtrain,Xtest=Xtest,aMat.train=aMat.train, aMat.test=aMat.test))
      phat.pre = matrix(unlist(list.out),ncol = length(Avals), byrow = F)
    }
    if(pi.algo == 'ranger'){
      list.out = lapply(Avals, function(a) pi.pred.rg.nm(a.val=a,train.df=train.df,Xtrain=Xtrain,Xtest=Xtest,aMat.train=aMat.train, aMat.test=aMat.test))
      phat.pre = matrix(unlist(list.out),ncol = length(Avals), byrow = F)
    }
    if(pi.algo == 'superlearner'){
      list.out = lapply(Avals, function(a) pi.pred.sl.nm(a.val=a,train.df=train.df,Xtrain=Xtrain,Xtest=Xtest,aMat.train=aMat.train, aMat.test=aMat.test))
      phat.pre = matrix(unlist(list.out),ncol = length(Avals), byrow = F)
    }
    if(pi.algo == 'ranger2'){
      phat.pre = pi.pred.rg2.nm(train.df=train.df,Xtrain=Xtrain,Xtest=Xtest)
    }

    ### nuisance function outputs
    out.pi[test,] <- phat.pre
    out.mu[test,] <- preds

    ### outputs for matlab
    out.muF[train,] <- muF
    constr <- round(apply(aLevel[train,],2,sum) * 1.05)

    ### send muhat matrix to matlab, constraint for constrained optimization
    write.csv(constr, "~jacquelinemauro/Dropbox/sorter/optConstr.csv")
    write.csv(muF, "~jacquelinemauro/Dropbox/sorter/optMuhat.csv")
    run_matlab_script("~jacquelinemauro/Dropbox/sorter/approxConstr.m")

    ### read assignment vector from matlab
    f.mat <- read.csv("~jacquelinemauro/Dropbox/sorter/optfhat.csv", header = F)
    f.con <- Avals[apply(f.mat,1,which.max)]

    ### train E(f|X) on first sample -- how do i deal with distance in the test set?
    Xtrain$obsD <- sapply(1:dim(f.mat)[1], function(y) aMat.train[y,which.max(f.mat[y,])])
    #Xtest$obsD <- aMat.test[,names(aMat.test)==f.con]
    Xtest$obsD <- obsD[test]
    rng.dat <- data.frame(A = as.factor(f.con), Xtrain)
    fhat = predict(ranger::ranger(A~., data = rng.dat, write.forest = T), Xtest, type = 'response')$pre

    ### train E(Y|A,X) & predict E(Y|A = a, X)
    phat.pre[phat.pre < 1e-3] = 1e-3
    phat.pre[phat.pre > 1-1e-3] = 1-1e-3
    temp = t(sapply(Avals, function(x) as.numeric(test.df$A == x)))
    phat = diag(phat.pre %*% temp)

    ### get estimate of effect at assigned A
    mu.hat = diag(preds %*% t(model.matrix(~Avals[fhat]-1))) # check this
    ifvals = (as.numeric(test.df$A == fhat)/phat) * (test.df$y - mu.hat) + mu.hat
    psihat[vfold] = mean(ifvals[phat!=0])
    sdhat[vfold] = sd(ifvals[phat!=0])/sqrt(dim(df)[1])

    ### store ifvals and assignment vector
    assig.vec[train] <- f.con
    est.assig.vec[test] <- fhat
    ifs[test] <- ifvals
  }

  psi = mean(psihat)
  sd = mean(sdhat)

  return(list(psi = psi,sd = sd, ifvals = ifs, assig.vec = assig.vec,
              muhat = out.mu, pihat = out.pi, fhat = est.assig.vec))
}

mu.pred.sl.nm.ap <- function(a.val, train.df, Xtrain, Xtest, sl.lib= c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger"), aMat.train, aMat.test){
  Xtrain$obsD <- aMat.train[,names(aMat.train)==a.val]
  Xtest$obsD <- aMat.test[,names(aMat.train)==a.val]
  out = SuperLearner(Y = train.df$y, X = Xtrain, family = binomial(), SL.library = sl.lib)
  preds1 = c(predict.SuperLearner(out, newdata = Xtest, onlySL = TRUE)[[1]])
  preds2 = c(predict.SuperLearner(out, newdata = Xtrain, onlySL = TRUE)[[1]])
  return(list(preds1,preds2))
}
mu.pred.rg.nm.ap <- function(a.val, train.df, Xtrain, Xtest, sl.lib= c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger"), aMat.train, aMat.test){
  d <- cbind(train.df$y,Xtrain); names(d)<-c('y',names(Xtrain))
  d$obsD <- aMat.train[,names(aMat.train)==a.val]
  Xtest$obsD <- aMat.test[,names(aMat.train)==a.val]
  preds1 <- predict(ranger::ranger(y~., data = d, write.forest = T), Xtest)$pre
  preds2 <- predict(ranger::ranger(y~., data = d, write.forest = T), Xtrain)$pre
  return(list(preds1,preds2))
}