#' Function combining constrained and unconstrained minimization
#'
#' @param df the dataframe. This should be organized as follows:
#' the outcome should be named "y", the treatment should be named "A",
#' the 3rd through final columns will be considered covariates.
#'
#' @param aLevel a matrix of treatment level covariates
#'
#' @param obsD a vector containing the treatment level covariate
#' actually observed.
#'
#' @return returns the estimate, sd, assignment vector and influence-function
#' values for the unconstrained optimization. Also returns the matrix of
#' influence function values that can be used in the constrained optimization
#' procedure.

constr.opt.causal <- function(df,aLevel,obsD,
                              mu.algo = 'superlearner',
                              pi.algo = 'superlearner',
                              sl.lib = c("SL.gam","SL.glm","SL.glmnet","SL.glm.interaction", "SL.mean","SL.ranger","SL.rpart")){
  library('ranger')
  library('SuperLearner')
  library('randomForest')
  library('plyr')
  n = dim(df)[1]
  df$A <- as.numeric(df$A)
  Avals <- sort(unique(df$A)); n.avals <- length(Avals)
  Amap <- cbind(Avals, colnames(aLevel))

  s <- as.numeric(runif(n) < .5)+1
  psihat <- sdhat <- rep(NA,2)
  assig.vec <- ifs <- rep(NA,n)
  phihat <- out.mu <- out.pi <- matrix(rep(NA, n*n.avals), ncol = n.avals)
  for(rnd in 1:2){
    ### training and testing sets
    train.df <- df[s==rnd,]; train.df$obsD <- obsD[s==rnd]
    test.df <- df[s!=rnd,]; test.df$obsD <- obsD[s!=rnd]
    Xtrain <- train.df[,-c(1:2)]
    Xtest <- test.df[,-c(1:2)]
    aMat.train <- aLevel[s==rnd,]
    aMat.test <- aLevel[s!=rnd,]

    ### train E(Y|A,X) & predict E(Y|A = a, X) for each a
    if(mu.algo == 'ranger'){
      list.out = lapply(Avals, function(a) mu.pred.rg(train.df = train.df, Xtrain = Xtrain, Xtest = Xtest, a.val = a,aMat.train=aMat.train, aMat.test=aMat.test))
      preds <- matrix(unlist(list.out),ncol = length(Avals), byrow = F)
      }
    if(mu.algo == 'superlearner'){
      list.out = lapply(Avals, function(a) mu.pred.sl(train.df = train.df, Xtrain = Xtrain, Xtest = Xtest, a.val = a,aMat.train=aMat.train, aMat.test=aMat.test))
      preds <- matrix(unlist(list.out),ncol = length(Avals), byrow = F)
    }


    ### train P(A|X) & predict P(A=a|X) for each a
    if(pi.algo == 'logistic'){
      list.out = lapply(Avals, function(a) pi.pred.lg(a.val=a,train.df=train.df,Xtrain=Xtrain,Xtest=Xtest,aMat.train=aMat.train, aMat.test=aMat.test))
      phat.pre = matrix(unlist(list.out),ncol = length(Avals), byrow = F)
    }
    if(pi.algo == 'ranger'){
      list.out = lapply(Avals, function(a) pi.pred.rg(a.val=a,train.df=train.df,Xtrain=Xtrain,Xtest=Xtest,aMat.train=aMat.train, aMat.test=aMat.test))
      phat.pre = matrix(unlist(list.out),ncol = length(Avals), byrow = F)
    }
    if(pi.algo == 'superlearner'){
      list.out = lapply(Avals, function(a) pi.pred.sl(a.val=a,train.df=train.df,Xtrain=Xtrain,Xtest=Xtest,aMat.train=aMat.train, aMat.test=aMat.test))
      phat.pre = matrix(unlist(list.out),ncol = length(Avals), byrow = F)
    }
    if(pi.algo == 'ranger2'){
      phat.pre = pi.pred.rg2(train.df=train.df,Xtrain=Xtrain,Xtest=Xtest)
    }


    # truncate pi values
    phat.pre[phat.pre < 1e-3] = 1e-3
    phat.pre[phat.pre > 1-1e-3] = 1-1e-3

    temp = t(sapply(Avals, function(x) as.numeric(test.df$A == x)))
    phat = diag(phat.pre %*% temp)

    ### output phihat for constrained version
    phihat[s!=rnd,] <- sapply(1:n.avals, function(a) (as.numeric(test.df$A == Avals[a])/phat.pre[,a])*(test.df$y - preds[,a]) + preds[,a])
    out.mu[s!=rnd,] <- preds
    out.pi[s!=rnd,] <- phat.pre

    ### get assignment vector f and E(Y| A = f, X)
    f.hat <- Avals[apply(preds, 1, which.min)]
    min.mu <- apply(preds,1,min)

    ### get estimate of effect at minimum
    ifvals = (as.numeric(test.df$A == f.hat)/phat) * (test.df$y - min.mu) + min.mu
    psihat[rnd] = mean(ifvals[phat!=0])
    sdhat[rnd] = sd(ifvals[phat!=0])/sqrt(dim(df)[1])

    ### store ifvals and assignment vector
    assig.vec[s!=rnd] <- f.hat
    ifs[s!=rnd] <- ifvals
  }

  psi = mean(psihat)
  sd = mean(sdhat)

  return(list(psi = psi,sd = sd, ifvals = ifs, assig.vec = assig.vec, phihat = phihat,
              muhat = out.mu, pihat = out.pi))
}


# nuisance parameter estimation functions
mu.pred.sl <- function(a.val, train.df, Xtrain, Xtest, sl.lib= c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger"), aMat.train, aMat.test){
  Xtrain$obsD <- aMat.train[,a.val]
  Xtest$obsD <- aMat.test[,a.val]
  out = SuperLearner(Y = train.df$y, X = Xtrain, family = binomial(), SL.library = sl.lib)
  preds = c(predict.SuperLearner(out, newdata = Xtest, onlySL = TRUE)[[1]])
  return(preds)
}
mu.pred.rg <- function(a.val, train.df, Xtrain, Xtest, sl.lib= c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger"), aMat.train, aMat.test){
  d <- cbind(train.df$y,Xtrain); names(d)<-c('y',names(Xtrain))
  d$obsD <- aMat.train[,a.val]
  Xtest$obsD <- aMat.test[,a.val]
  preds <- predict(ranger::ranger(y~., data = d, write.forest = T), Xtest)$pre
  return(preds)
}
pi.pred.sl <- function(a.val, train.df, Xtrain, Xtest, sl.lib= c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger"), aMat.train, aMat.test){
  Xtrain$obsD <- aMat.train[,a.val]
  Xtest$obsD <- aMat.test[,a.val]
  out = SuperLearner(Y = as.numeric(train.df$A==a.val), X = Xtrain, family = binomial(), SL.library = sl.lib)
  preds = c(predict.SuperLearner(out, newdata = Xtest, onlySL = TRUE)[[1]])
  return(preds)
}
pi.pred.lg <- function(a.val, train.df, Xtrain, Xtest, aMat.train, aMat.test){
  d <- cbind(as.numeric(a.val==train.df$A),Xtrain); names(d)<-c('A',names(Xtrain))
  d$obsD <- aMat.train[,a.val]
  Xtest$obsD <- aMat.test[,a.val]
  preds <- predict(glm(A~., data = d, family = binomial), newdata = Xtest, type = 'response')
  return(preds)
}
pi.pred.rg <- function(a.val, train.df, Xtrain, Xtest, aMat.train, aMat.test){
  library('ranger')
  d <- cbind(as.numeric(a.val==train.df$A),Xtrain); names(d)<-c('A',names(Xtrain))
  d$obsD <- aMat.train[,a.val]
  Xtest$obsD <- aMat.test[,a.val]
  preds <- predict(ranger::ranger(as.factor(A)~., data = d, write.forest = T,probability = T), Xtest)$pre
  return(preds[,2])
}
pi.pred.rg2 <- function(train.df, Xtrain, Xtest){
  library('ranger')
  # does not use the distance value
  d <- cbind(train.df$A,Xtrain); names(d)<-c('A',names(Xtrain))
  preds <- predict(ranger::ranger(as.factor(A)~., data = d, write.forest = T,probability = T), Xtest)$pre
  return(preds)
}
