#' Unconstrained Minimum
#'
#' @params df the dataframe. This should be organized as follows:
#' the outcome should be named "y", the treatment should be named "A",
#' the 3rd through final columns will be considered covariates.
#'
mat.pred1 <- function(df, A, mu){
  t <- df
  t$A <- A
  predict(mu, t)$pre
}
sl.pred1 <- function(a.val, train.df, Xtrain, Xtest, sl.lib){
  # need to add obsD
  out = SuperLearner(Y = as.numeric(train.df$A==a.val), X = Xtrain, family = binomial(), SL.library = sl.lib)
  preds = c(predict.SuperLearner(out, newdata = Xtest, onlySL = TRUE)[[1]])
  return(preds)
}
lg.pred1 <- function(a.val, train.df, Xtrain, Xtest){
  # need to add obsD
  d <- cbind(as.numeric(a.val==train.df$A),Xtrain); names(d)<-c('A',names(Xtrain))
  preds <- predict(glm(A~., data = d, family = binomial), newdata = Xtest, type = 'response')
  return(preds)
}
rg.pred1 <- function(a.val, train.df, Xtrain, Xtest){
  library('ranger')
  d <- cbind(as.numeric(a.val==train.df$A),Xtrain); names(d)<-c('A',names(Xtrain))
  preds <- predict(ranger::ranger(as.factor(A)~., data = d, write.forest = T, probability = T), Xtest)$pre
  return(preds[,2])
}



unconstrained.min <- function(df){
  library('ranger')
  library('SuperLearner')
  library('randomForest')
  library('plyr')
  sl.lib = c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger")
  n = dim(df)[1]
  df$A <- as.numeric(df$A)
  Avals <- unique(df$A)

  s <- as.numeric(runif(n) < .5)+1
  psihat <- sdhat <- rep(NA,2)
  assig.vec <- ifs <- rep(NA,n)
  for(rnd in 1:2){
    ### training and testing sets
    train.df <- df[s==rnd,]
    test.df <- df[s!=rnd,]
    Xtrain <- as.data.frame(train.df[,(3:dim(df)[2])])
    Xtest <- as.data.frame(test.df[,(3:dim(df)[2])])
    names(Xtrain) <- names(Xtest) <- names(df)[3:dim(df)[2]]

    ### train E(Y|A,X)
    mu <- ranger::ranger(y~., data = train.df, write.forest = TRUE)

    ### predict E(Y|A = a, X) for each a
    preds <- matrix(unlist(lapply(Avals, function(a) mat.pred(df = test.df, A = a, mu = mu))),
                    ncol = length(Avals), byrow = F)

    ### get assignment vector f and E(Y| A = f, X)
    f.hat <- Avals[apply(preds, 1, which.min)]
    min.mu <- apply(preds,1,min)
    temp = t(sapply(Avals, function(x) as.numeric(test.df$A == x)))
    obs.mu = diag(preds %*% temp)

    ### get P(A=a|X) for each a
    phat.pre = matrix(unlist(lapply(Avals, function(a) sl.pred1(a.val=a,train.df=train.df,Xtrain=Xtrain,Xtest=Xtest,sl.lib=sl.lib)))
                      ,ncol = length(Avals), byrow = F)
    phat = diag(phat.pre %*% temp)

    ### get estimate of effect
    ifvals = ((as.numeric(test.df$A == f.hat)/phat) * (test.df$y - obs.mu) + min.mu)
    psihat[rnd] = mean(ifvals[phat!=0])
    sdhat[rnd] = sd(ifvals[phat!=0])/sqrt(dim(df)[1])

    ### store ifvals and assignment vector
    assig.vec[s!=rnd] <- f.hat
    ifs[s!=rnd] <- ifvals
  }

  psi = mean(psihat)
  sd = mean(sdhat)

  return(list(psi = psi,sd = sd, ifvals = ifs, assig.vec = assig.vec))
}

unconstrained.min.lg <- function(df){
  library('ranger')
  library('SuperLearner')
  library('randomForest')
  library('plyr')
  sl.lib = c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger")
  n = dim(df)[1]
  df$A <- as.numeric(df$A)
  Avals <- unique(df$A)

  s <- as.numeric(runif(n) < .5)+1
  psihat <- sdhat <- rep(NA,2)
  assig.vec <- ifs <- rep(NA,n)
  for(rnd in 1:2){
    ### training and testing sets
    train.df <- df[s==rnd,]
    test.df <- df[s!=rnd,]
    Xtrain <- as.data.frame(train.df[,(3:dim(df)[2])])
    Xtest <- as.data.frame(test.df[,(3:dim(df)[2])])
    names(Xtrain) <- names(Xtest) <- names(df)[3:dim(df)[2]]

    ### train E(Y|A,X)
    mu <- ranger::ranger(y~., data = train.df, write.forest = TRUE)

    ### predict E(Y|A = a, X) for each a
    preds <- matrix(unlist(lapply(Avals, function(a) mat.pred(df = test.df, A = a, mu = mu))),
                    ncol = length(Avals), byrow = F)

    ### get assignment vector f and E(Y| A = f, X)
    f.hat <- Avals[apply(preds, 1, which.min)]
    min.mu <- apply(preds,1,min)
    temp = t(sapply(Avals, function(x) as.numeric(test.df$A == x)))
    obs.mu = diag(preds %*% temp)

    ### get P(A=a|X) for each a
    phat.pre = matrix(unlist(lapply(Avals, function(a) lg.pred1(a.val=a,train.df=train.df,Xtrain=Xtrain,Xtest=Xtest)))
                      ,ncol = length(Avals), byrow = F)
    phat = diag(phat.pre %*% temp)

    ### get estimate of effect
    ifvals = ((as.numeric(test.df$A == f.hat)/phat) * (test.df$y - obs.mu) + min.mu)
    psihat[rnd] = mean(ifvals[phat!=0])
    sdhat[rnd] = sd(ifvals[phat!=0])/sqrt(dim(df)[1])

    ### store ifvals and assignment vector
    assig.vec[s!=rnd] <- f.hat
    ifs[s!=rnd] <- ifvals
  }

  psi = mean(psihat)
  sd = mean(sdhat)

  return(list(psi = psi,sd = sd, ifvals = ifs, assig.vec = assig.vec))
}

unconstrained.min.rg <- function(df){
  library('ranger')
  library('SuperLearner')
  library('randomForest')
  library('plyr')
  sl.lib = c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger")
  n = dim(df)[1]
  df$A <- as.numeric(df$A)
  Avals <- unique(df$A)

  s <- as.numeric(runif(n) < .5)+1
  psihat <- sdhat <- rep(NA,2)
  assig.vec <- ifs <- rep(NA,n)
  for(rnd in 1:2){
    ### training and testing sets
    train.df <- df[s==rnd,]
    test.df <- df[s!=rnd,]
    Xtrain <- as.data.frame(train.df[,(3:dim(df)[2])])
    Xtest <- as.data.frame(test.df[,(3:dim(df)[2])])
    names(Xtrain) <- names(Xtest) <- names(df)[3:dim(df)[2]]

    ### train E(Y|A,X)
    # need to replace distance with observed distance (total_time)
    mu <- ranger::ranger(y~., data = train.df, write.forest = TRUE)

    ### predict E(Y|A = a, X) for each a
    preds <- matrix(unlist(lapply(Avals, function(a) mat.pred1(df = test.df, A = a, mu = mu))),
                    ncol = length(Avals), byrow = F)

    ### get assignment vector f and E(Y| A = f, X)
    f.hat <- Avals[apply(preds, 1, which.min)]
    min.mu <- apply(preds,1,min)
    temp = t(sapply(Avals, function(x) as.numeric(test.df$A == x)))
    obs.mu = diag(preds %*% temp)

    ### get P(A=a|X) for each a
    phat.pre = matrix(unlist(lapply(Avals, function(a) rg.pred1(a.val=a,train.df=train.df,Xtrain=Xtrain,Xtest=Xtest)))
                      ,ncol = length(Avals), byrow = F)
    phat = diag(phat.pre %*% temp)

    ### get estimate of effect
    ifvals = ((as.numeric(test.df$A == f.hat)/phat) * (test.df$y - min.mu) + min.mu)
    psihat[rnd] = mean(ifvals[phat!=0])
    sdhat[rnd] = sd(ifvals[phat!=0])/sqrt(dim(df)[1])

    ### store ifvals and assignment vector
    assig.vec[s!=rnd] <- f.hat
    ifs[s!=rnd] <- ifvals
  }

  psi = mean(psihat)
  sd = mean(sdhat)

  return(list(psi = psi,sd = sd, ifvals = ifs, assig.vec = assig.vec))
}

unconstrained.min.rg.aLevel <- function(df,aLevel,obsD){
  # includes a matrix of treatment level covariates
  # obsD is the vector of observed treatment level covariates
  library('ranger')
  library('SuperLearner')
  library('randomForest')
  library('plyr')
  sl.lib = c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger")
  n = dim(df)[1]
  df$A <- as.numeric(df$A)
  Avals <- unique(df$A)

  s <- as.numeric(runif(n) < .5)+1
  psihat <- sdhat <- rep(NA,2)
  assig.vec <- ifs <- rep(NA,n)
  for(rnd in 1:2){
    ### training and testing sets
    train.df <- df[s==rnd,]; train.df$obsD <- obsD[s==rnd]
    test.df <- df[s!=rnd,]; test.df$obsD <- obsD[s!=rnd]
    Xtrain <- train.df[,-c(1:2)]
    Xtest <- test.df[,-c(1:2)]
    aMat.train <- aLevel[s==rnd,]
    aMat.test <- aLevel[s!=rnd,]

    ### train E(Y|A,X)
    mu <- ranger::ranger(y~., data = train.df, write.forest = TRUE)

    ### predict E(Y|A = a, X) for each a
    preds <- matrix(unlist(lapply(Avals, function(a) mat.pred(df = test.df, A = a, mu = mu, aLevel = aMat.test))),
                    ncol = length(Avals), byrow = F)

    ### get assignment vector f and E(Y| A = f, X)
    f.hat <- Avals[apply(preds, 1, which.min)]
    min.mu <- apply(preds,1,min)

    ### get P(A=a|X) for each a
    phat.pre = matrix(unlist(lapply(Avals, function(a) rg.pred(a.val=a,train.df=train.df,Xtrain=Xtrain,Xtest=Xtest,
                                                               aMat.train=aMat.train, aMat.test=aMat.test)))
                      ,ncol = length(Avals), byrow = F)
    temp = t(sapply(Avals, function(x) as.numeric(test.df$A == x)))
    phat = diag(phat.pre %*% temp)

    ### get estimate of effect
    ifvals = (as.numeric(test.df$A == f.hat)/phat) * (test.df$y - min.mu) + min.mu
    psihat[rnd] = mean(ifvals[phat!=0])
    sdhat[rnd] = sd(ifvals[phat!=0])/sqrt(dim(df)[1])

    ### store ifvals and assignment vector
    assig.vec[s!=rnd] <- f.hat
    ifs[s!=rnd] <- ifvals
  }

  psi = mean(psihat)
  sd = mean(sdhat)

  return(list(psi = psi,sd = sd, ifvals = ifs, assig.vec = assig.vec))
}


unconstrained.min.par <- function(df){
  library('ranger')
  library('SuperLearner')
  library('randomForest')
  library('plyr')
  library('parallel')
  sl.lib = c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger")
  n = dim(df)[1]
  df$A <- as.numeric(df$A)
  Avals <- unique(df$A)
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)

  s <- as.numeric(runif(n) < .5)+1
  psihat <- sdhat <- rep(NA,2)
  assig.vec <- ifs <- rep(NA,n)
  for(rnd in 1:2){
    ### training and testing sets
    train.df <- df[s==rnd,]
    test.df <- df[s!=rnd,]
    Xtrain <- as.data.frame(train.df[,(3:dim(df)[2])])
    Xtest <- as.data.frame(test.df[,(3:dim(df)[2])])
    names(Xtrain) <- names(Xtest) <- names(df)[3:dim(df)[2]]


    ### train E(Y|A,X)
    mu <- ranger::ranger(y~., data = train.df, write.forest = TRUE)

    ### predict E(Y|A = a, X) for each a
    preds <- matrix(unlist(lapply(Avals, function(a) mat.pred(df = test.df, A = a, mu = mu))),
                    ncol = length(Avals), byrow = F)

    ### get assignment vector f and E(Y| A = f, X)
    f.hat <- Avals[apply(preds, 1, which.min)]
    min.mu <- apply(preds,1,min)
    temp = t(sapply(Avals, function(x) as.numeric(test.df$A == x)))
    obs.mu = diag(preds %*% temp)

    ### get P(A=a|X) for each a
    clusterExport(cl,list('Avals','sl.pred','train.df','Xtrain','Xtest','sl.lib','SuperLearner','All',
                          "SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger",'predict.SuperLearner'), envir=environment())
    lapply.out = parLapply(cl, Avals, function(a) sl.pred(a.val=a,train.df=train.df,Xtrain=Xtrain,Xtest=Xtest,sl.lib=sl.lib))
    phat.pre = matrix(unlist(lapply.out),ncol = length(Avals), byrow = F)
    phat = diag(phat.pre %*% temp)

    ### get estimate of effect
    ifvals = ((as.numeric(test.df$A == f.hat)/phat) * (test.df$y - obs.mu) + min.mu)
    psihat[rnd] = mean(ifvals[phat!=0])
    sdhat[rnd] = sd(ifvals[phat!=0])/sqrt(dim(df)[1])

    ### store ifvals and assignment vector
    assig.vec[s!=rnd] <- f.hat
    ifs[s!=rnd] <- ifvals
  }

  psi = mean(psihat)
  sd = mean(sdhat)
  stopCluster(cl)

  return(list(psi = psi,sd = sd, ifvals = ifs, assig.vec = assig.vec))
}
