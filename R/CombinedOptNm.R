#' Function combining constrained and unconstrained minimization
#' using named assignment vector
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
#'@param nsplits number of sample splits, defaults to 2
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


# TO DO: do you actually need to predict pi at each a?
constr.opt.causal.nm <- function(df,aLevel,obsD, nsplits = 2,
                              mu.algo = 'superlearner',
                              pi.algo = 'superlearner',
                              sl.lib = c("SL.gam","SL.glm","SL.glmnet","SL.glm.interaction", "SL.mean","SL.ranger","SL.rpart")){
  library('ranger')
  library('SuperLearner')
  library('randomForest')
  library('plyr')
  n = dim(df)[1]
  Avals <- names(aLevel); n.avals <- length(Avals)

  s <- sample(rep(1:nsplits,ceiling(n/nsplits))[1:n])
  psihat <- sdhat <- rep(NA,2)
  assig.vec <- ifs <- rep(NA,n)
  phihat <- out.mu <- out.pi <- matrix(rep(NA, n*n.avals), ncol = n.avals)
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
      list.out = lapply(Avals, function(a) mu.pred.rg.nm(train.df = train.df, Xtrain = Xtrain, Xtest = Xtest, a.val = a,aMat.train=aMat.train, aMat.test=aMat.test))
      preds <- matrix(unlist(list.out),ncol = length(Avals), byrow = F)
    }
    if(mu.algo == 'superlearner'){
      list.out = lapply(Avals, function(a) mu.pred.sl.nm(train.df = train.df, Xtrain = Xtrain, Xtest = Xtest, a.val = a,aMat.train=aMat.train, aMat.test=aMat.test))
      preds <- matrix(unlist(list.out),ncol = length(Avals), byrow = F)
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


    # truncate pi values
    phat.pre[phat.pre < 1e-3] = 1e-3
    phat.pre[phat.pre > 1-1e-3] = 1-1e-3

    temp = t(sapply(Avals, function(x) as.numeric(test.df$A == x)))
    phat = diag(phat.pre %*% temp)

    ### output phihat for constrained version
    phihat[test,] <- sapply(1:n.avals, function(a) (as.numeric(test.df$A == Avals[a])/phat.pre[,a])*(test.df$y - preds[,a]) + preds[,a])
    out.mu[test,] <- preds
    out.pi[test,] <- phat.pre

    ### get assignment vector f and E(Y| A = f, X)
    f.hat <- Avals[apply(preds, 1, which.min)]
    min.mu <- apply(preds,1,min)

    ### get estimate of effect at minimum
    ifvals = (as.numeric(test.df$A == f.hat)/phat) * (test.df$y - min.mu) + min.mu
    psihat[vfold] = mean(ifvals[phat!=0])
    sdhat[vfold] = sd(ifvals[phat!=0])/sqrt(dim(df)[1])

    ### store ifvals and assignment vector
    assig.vec[test] <- f.hat
    ifs[test] <- ifvals
  }

  psi = mean(psihat)
  sd = mean(sdhat)

  return(list(psi = psi,sd = sd, ifvals = ifs, assig.vec = assig.vec, phihat = phihat,
              muhat = out.mu, pihat = out.pi))
}


# nuisance parameter estimation functions
mu.pred.sl.nm <- function(a.val, train.df, Xtrain, Xtest, sl.lib= c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger"), aMat.train, aMat.test){
  Xtrain$obsD <- aMat.train[,names(aMat.train)==a.val]
  Xtest$obsD <- aMat.test[,names(aMat.train)==a.val]
  out = SuperLearner(Y = train.df$y, X = Xtrain, family = binomial(), SL.library = sl.lib)
  preds = c(predict.SuperLearner(out, newdata = Xtest, onlySL = TRUE)[[1]])
  return(preds)
}
mu.pred.rg.nm <- function(a.val, train.df, Xtrain, Xtest, sl.lib= c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger"), aMat.train, aMat.test){
  d <- cbind(train.df$y,Xtrain); names(d)<-c('y',names(Xtrain))
  d$obsD <- aMat.train[,names(aMat.train)==a.val]
  Xtest$obsD <- aMat.test[,names(aMat.train)==a.val]
  preds <- predict(ranger::ranger(y~., data = d, write.forest = T), Xtest)$pre
  return(preds)
}
pi.pred.sl.nm <- function(a.val, train.df, Xtrain, Xtest, sl.lib= c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger"), aMat.train, aMat.test){
  Xtrain$obsD <- aMat.train[,names(aMat.train)==a.val]
  Xtest$obsD <- aMat.test[,names(aMat.train)==a.val]
  out = SuperLearner(Y = as.numeric(train.df$A==a.val), X = Xtrain, family = binomial(), SL.library = sl.lib)
  preds = c(predict.SuperLearner(out, newdata = Xtest, onlySL = TRUE)[[1]])
  return(preds)
}
pi.pred.lg.nm <- function(a.val, train.df, Xtrain, Xtest, aMat.train, aMat.test){
  #need to cheange to nm
  d <- cbind(as.numeric(a.val==train.df$A),Xtrain); names(d)<-c('A',names(Xtrain))
  d$obsD <- aMat.train[,names(aMat.train)==a.val]
  Xtest$obsD <- aMat.test[,names(aMat.train)==a.val]
  preds <- predict(glm(A~., data = d, family = binomial), newdata = Xtest, type = 'response')
  return(preds)
}
pi.pred.rg.nm <- function(a.val, train.df, Xtrain, Xtest, aMat.train, aMat.test){
  library('ranger')
  d <- cbind(as.numeric(a.val==train.df$A),Xtrain); names(d)<-c('A',names(Xtrain))
  d$obsD <- aMat.train[,names(aMat.train)==a.val]
  Xtest$obsD <- aMat.test[,names(aMat.train)==a.val]
  preds <- predict(ranger::ranger(as.factor(A)~., data = d, write.forest = T,probability = T), Xtest)$pre
  return(preds[,2])
}
pi.pred.rg2.nm <- function(train.df, Xtrain, Xtest){
  library('ranger')
  #need to cheange to nm
  # does not use the distance value
  d <- cbind(train.df$A,Xtrain); names(d)<-c('A',names(Xtrain))
  preds <- predict(ranger::ranger(as.factor(A)~., data = d, write.forest = T,probability = T), Xtest)$pre
  return(preds)
}
