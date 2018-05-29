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

constr.opt.causal <- function(df,aLevel,obsD){
  library('ranger')
  library('SuperLearner')
  library('randomForest')
  library('plyr')
  sl.lib = c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger")
  n = dim(df)[1]
  df$A <- as.numeric(df$A)
  Avals <- unique(df$A); n.avals <- length(Avals)

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

    ### train E(Y|A,X)
    #mu <- ranger::ranger(y~., data = train.df, write.forest = TRUE)
    mu <- SuperLearner(Y = train.df$y, X = Xtrain, family = binomial(), SL.library = sl.lib)

    ### predict E(Y|A = a, X) for each a
    preds <- matrix(unlist(lapply(Avals, function(a) mat.pred(df = test.df, A = a, mu = mu, aLevel = aMat.test))),
                    ncol = length(Avals), byrow = F)

    ### get P(A=a|X) for each a
    phat.pre = matrix(unlist(lapply(Avals, function(a) sl.pred(a.val=a,train.df=train.df,Xtrain=Xtrain,Xtest=Xtest,
                                                               aMat.train=aMat.train, aMat.test=aMat.test)))
                      ,ncol = length(Avals), byrow = F)

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

  #return(list(psi = psi,sd = sd, ifvals = ifs, assig.vec = assig.vec, phihat = phihat))

  # what if we do more similarly to the unconstrained version and minimize just based
  # on mu, then calculate the if afterwards?
  return(list(psi = psi,sd = sd, ifvals = ifs, assig.vec = assig.vec, phihat = phihat,
              muhat = out.mu, pihat = out.pi))
}



mat.pred <- function(df, A, mu, aLevel){
  t <- df
  t$obsD <- aLevel[,A]
  t$A <- A
  preds = c(predict.SuperLearner(mu, newdata = t, onlySL = TRUE)[[1]])
  return(preds)
  #predict(mu, t)$pre
}
sl.pred <- function(a.val, train.df, Xtrain, Xtest, sl.lib= c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger"), aMat.train, aMat.test){
  # need to add obsD
  Xtrain$obsD <- aMat.train[,a.val]
  Xtest$obsD <- aMat.test[,a.val]
  out = SuperLearner(Y = as.numeric(train.df$A==a.val), X = Xtrain, family = binomial(), SL.library = sl.lib)
  preds = c(predict.SuperLearner(out, newdata = Xtest, onlySL = TRUE)[[1]])
  return(preds)
}
lg.pred <- function(a.val, train.df, Xtrain, Xtest, aMat.train, aMat.test){
  d <- cbind(as.numeric(a.val==train.df$A),Xtrain); names(d)<-c('A',names(Xtrain))
  d$obsD <- aMat.train[,a.val]
  Xtest$obsD <- aMat.test[,a.val]
  preds <- predict(glm(A~., data = d, family = binomial), newdata = Xtest, type = 'response')
  return(preds)
}
rg.pred <- function(a.val, train.df, Xtrain, Xtest, aMat.train, aMat.test){
  library('ranger')
  d <- cbind(as.numeric(a.val==train.df$A),Xtrain); names(d)<-c('A',names(Xtrain))
  d$obsD <- aMat.train[,a.val]
  Xtest$obsD <- aMat.test[,a.val]
  preds <- predict(ranger::ranger(as.factor(A)~., data = d, write.forest = T, probability = T), Xtest)$pre
  return(preds[,2])
}
