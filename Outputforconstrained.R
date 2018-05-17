#### output phi values to put in linear optimization 

#### helper functions ----
sl.lib = c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger")
pi.pred.rg <- function(a.val, train.df, Xtrain, Xtest){
  library('ranger')
  d <- cbind(as.factor(a.val==train.df$A),Xtrain); names(d)<-c('A',names(Xtrain))
  preds <- predict(ranger::ranger(A~., data = d, write.forest = T, probability = T), Xtest)$pre
  return(preds)
}
pi.pred <- function(a.val, train.df, Xtrain, Xtest, sl.lib = c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger")){
  out = SuperLearner(Y = as.numeric(train.df$A==a.val), X = Xtrain, family = binomial(), SL.library = sl.lib,
                     newX = Xtest)$SL.predict
  return(out)
}
pi.pred.lg <- function(a.val, train.df, Xtrain, Xtest){
  d <- cbind(as.numeric(a.val==train.df$A),Xtrain); names(d)<-c('A',names(Xtrain))
  preds <- predict(glm(A~., data = d, family = binomial), newdata = Xtest, type = 'response')
  return(c(preds))
}
mu.pred <- function(a.val, train.df, test.df){
  mu <- ranger::ranger(y~., data = train.df, write.forest = TRUE)
  t <- test.df; t$A <- a.val
  preds <- predict(mu, t)$pre
  return(preds)
}

#### simulated data ----
df <- simsort(5000, k = .75)
s <- as.numeric(runif(5000) < .5) + 1
Avals = unique(df$A)


#### real data ----
df <- read.csv("~jacquelinemauro/CompleteCasesPrisoners.csv", header = T)[,-1]
names(df) <- c('y', 'A',sapply(c(1:dim(covs)[2]), function(k) paste('x',k,sep = "")))


make.phihat <- function(df){
  Avals <- unique(df$A); n.avals <- length(Avals)
  phihat <- matrix(rep(NA,n.avals*dim(df)[1]), ncol = n.avals)
  s <- as.numeric(runif(dim(df)[1]) < .5) + 1
  for (rnd in 1:2){
    train.df <- df[s==rnd,]
    test.df <- df[s!=rnd,]
    Xtrain <- as.data.frame(train.df[,(3:dim(df)[2])])
    Xtest <- as.data.frame(test.df[,(3:dim(df)[2])])
    names(Xtrain) <- names(Xtest) <- names(df)[3:dim(df)[2]]
    
    # train and predict mu and pi models
    muhat <- lapply(Avals, function(a) mu.pred(a.val = a, train.df = train.df, test.df = test.df))
    pihat <- lapply(Avals, function(a) pi.pred.lg(a.val = a, train.df = train.df, Xtrain = Xtrain, Xtest = Xtest))
   
    # output phihat matrix
    phihat[s!=rnd,] <- sapply(1:n.avals, function(a) (as.numeric(test.df$A == Avals[a])/pihat[[a]])*(test.df$y - muhat[[a]]) + muhat[[a]]) 
  }
  return(list(phihat = phihat, muhat = muhat, pihat = pihat))
}
out = make.phihat(df)
write.csv(out[[1]], '~jacquelinemauro/OptSortCausal/phihat_logistic.csv')

# don't truncate!
out.trunc <- out[[1]]
out.trunc[out.trunc<0] = 0
out.trunc[out.trunc>1] = 1
write.csv(out.trunc, '~jacquelinemauro/OptSortCausal/phihat_logistic_trunc.csv')

constraints <- table(df$A)
write.csv(out.trunc, '~jacquelinemauro/OptSortCausal/capacity.csv')