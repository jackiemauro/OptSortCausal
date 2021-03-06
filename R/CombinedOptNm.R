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

# to do: add a trycatch on the matlab script so it doesn't go through if it's not connected

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
  psihat <- sdhat <- psihat_c <- sdhat_c <- rep(NA,2)
  assig.vec <- ifs <- assig.vec_c <- ifs_c <- rep(NA,n)
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
    Adummies <- model.matrix(~df$A-1)
    Adummies.train <- Adummies[train,]

    ### train E(Y|A,X) & predict E(Y|A = a, X) for each a
    if(mu.algo == 'ranger'){
      d <- cbind(train.df$y,Xtrain,Adummies.train)
      names(d)<-c('y',names(Xtrain), Avals)
      Adummies.test <- matrix(rep(0,dim(Xtest)[1]*n.avals),ncol = n.avals)

      mu.model <- ranger::ranger(y~., data = d, write.forest = T)
      list.out = lapply(Avals, function(a) mu.pred.rg.nm(Xtest,Adummies.test,aMat.test,a.val=a,mu.model,d))
      preds <- matrix(unlist(list.out),ncol = length(Avals), byrow = F)
    }
    if(mu.algo == 'superlearner'){
      Xd <- cbind(Xtrain,train.df$A)
      names(Xd) <- c(names(Xtrain), "A")

      mu.model = SuperLearner(Y = train.df$y, X = Xd, family = binomial(), SL.library = sl.lib)
      list.out = lapply(Avals, function(a) mu.pred.sl.nm(Xtest,aMat.test,a.val=a,mu.model))
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
    f.hat <- factor(f.hat, levels = Avals)
    min.mu <- apply(preds,1,min)

    ### get estimate of effect at minimum
    ifvals = (as.numeric(test.df$A == f.hat)/phat) * (test.df$y - min.mu) + min.mu
    psihat[vfold] = mean(ifvals[phat!=0])
    sdhat[vfold] = sd(ifvals[phat!=0])/sqrt(dim(df)[1])

    ### store ifvals and assignment vector
    assig.vec[test] <- Avals[f.hat]
    ifs[test] <- ifvals

    ### send muhat matrix to matlab, constraint for constrained optimization
    constr <- c(round(1.05*table(train.df$A)))
    write.csv(constr, "~jacquelinemauro/Dropbox/sorter/capacityFudgeNm.csv")
    write.csv(preds, "~jacquelinemauro/Dropbox/sorter/SLmuhatUnconstrNewdatNmA.csv")
    run_matlab_script("~jacquelinemauro/Dropbox/sorter/prison_assignment_sl_nm.m")

    ### read assignment vector from matlab
    f.mat <- read.csv("~jacquelinemauro/Dropbox/sorter/prison_assignment_sl_nmA.csv", header = F)
    f.con <- Avals[apply(f.mat,1,which.max)] #switches order here to alphabetical
    f.con <- factor(f.con, levels = Avals) # but here it should switch it back

    ### get estimate of effect at constrained minimum
    ifvals_c = (as.numeric(test.df$A == f.con)/phat) * (test.df$y - min.mu) + min.mu
    psihat_c[vfold] = mean(ifvals_c[phat!=0])
    sdhat_c[vfold] = sd(ifvals_c[phat!=0])/sqrt(dim(df)[1])

    ### store ifvals and assignment vector
    assig.vec_c[test] <- Avals[f.con]
    ifs_c[test] <- ifvals_c
  }
  assig.vec = factor(assig.vec, levels = Avals)
  assig.vec_c = factor(assig.vec_c, levels = Avals)
  psi = mean(psihat)
  sd = mean(sdhat)

  return(list(psi = psi,sd = sd, ifvals = ifs, assig.vec = assig.vec,
              assig.vec_c = assig.vec_c, psi_c = mean(psihat_c), sd_c = mean(sdhat_c),
              muhat = out.mu, pihat = out.pi))
}


