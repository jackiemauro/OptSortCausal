########## helpers for constrained and unconstrained #######
# mu.pred.sl.nm <- function(Xd,Xtest,Adummies.test,aMat.test,a.val,mu.model,sl.lib= c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger")){
#   Xtest$obsD <- aMat.test[,names(aMat.test)==a.val]
#   Ad <- Adummies.test; Ad[,names(aMat.test)==a.val] <- 1
#   d.test <- cbind(Xtest,Ad); names(d.test) <- c(names(Xd),names(Ad))
#   preds = c(predict.SuperLearner(mu.model, newdata = d.test, onlySL = TRUE)[[1]])
#   return(preds)
# }
mu.pred.sl.nm <- function(Xtest,aMat.test,a.val,mu.model,sl.lib= c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger")){
  Xtest$obsD <- aMat.test[,names(aMat.test)==a.val]
  Xtest$A <- a.val;Xtest$A <- factor(Xtest$A, levels = names(aMat.test))
  preds = c(predict.SuperLearner(mu.model, newdata = Xtest, onlySL = TRUE)[[1]])
  return(preds)
}
mu.pred.rg.nm <- function(Xtest,Adummies.test,aMat.test,a.val,mu.model,d){
  #not checked
  Xtest$obsD <- aMat.test[,names(aMat.test)==a.val]
  Xtest$A <- a.val;Xtest$A <- factor(Xtest$A, levels = names(aMat.test))
  preds <- predict(mu.model, Xtest)$pre
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

########## helpers for approximate constrained #######
mu.pred.sl.nm.ap <- function(Xtest,Xtrain,aMat.test,a.val,mu.model,sl.lib= c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger")){
  Xtest$obsD <- aMat.test[,names(aMat.test)==a.val]
  Xtest$A <- a.val; Xtest$A <- factor(Xtest$A, levels = names(aMat.test))
  Xtrain$A <- a.val; Xtrain$A <- factor(Xtrain$A, levels = names(aMat.test))
  preds1 = c(predict.SuperLearner(mu.model, newdata = Xtest, onlySL = TRUE)[[1]])
  preds2 = c(predict.SuperLearner(mu.model, newdata = Xtrain, onlySL = TRUE)[[1]])
  return(list(preds1,preds2))
}
mu.pred.rg.nm.ap <- function(a.val, Xtrain, Xtest, aMat.test, mu.model){
  #not checked
  Xtest$obsD <- aMat.test[,names(aMat.test)==a.val]
  Xtest$A <- a.val; Xtest$A <- factor(Xtest$A, levels = names(aMat.test))
  Xtrain$A <- a.val; Xtrain$A <- factor(Xtrain$A, levels = names(aMat.test))
  preds1 <- predict(mu.model, Xtest)$pre
  preds2 <- predict(mu.model, Xtrain)$pre
  return(list(preds1,preds2))
}
