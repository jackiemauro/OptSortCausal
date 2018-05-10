rm(list = ls())
devtools::load_all()
df <- simsort(5000, k = .75)
f <- 1*(df$x<1) + 2*(df$x>=1 & df$x <2) + 3*(df$x>=2)

mat.pred <- function(df, A, mu){t <- df; t$A <- A; predict(mu, t)$pre}

unconstrained.min <- function(df){
  library('ranger')
  library('SuperLearner')
  library('randomForest')
  library('plyr')
  sl.lib = c("SL.earth","SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger")
  n = dim(df)[1]

  s <- as.numeric(runif(n) < .5)+1
  psihat <- sdhat <- rep(NA,2)
  assig.vec <- ifs <- rep(NA,n)
  for(rnd in 1:2){
    ### training and testing sets
    train.df <- df[s==rnd,]
    test.df <- df[s!=rnd,]

    ### train E(Y|A,X)
    mu <- ranger::ranger(y~., data = train.df, write.forest = TRUE)

    ### predict E(Y|A = a, X) for each a
    preds <- matrix(unlist(lapply(unique(df$A), function(a) mat.pred(df = test.df, A = a, mu = mu))),ncol = length(unique(df$A)), byrow = F)

    ### get assignment vector f and E(Y| A = f, X)
    f.hat <- unique(df$A)[apply(preds, 1, which.min)]
    min.mu <- apply(preds,1,min)
    temp = sapply(unique(df$A), function(x) test.df$A == x)
    obs.mu = sapply(1:dim(preds)[1], function(x) preds[x,which(temp[x,])])

    ### get P(A=a|X) for each a
    sl.pred <- function(a.val){
      out = SuperLearner(Y = as.numeric(train.df$A==a.val), X = data.frame(x = train.df$x), family = binomial(), SL.library = sl.lib)
      preds = c(predict.SuperLearner(out, newdata = data.frame(x = test.df$x), onlySL = TRUE)[[1]])
    }
    phat.pre = matrix(unlist(lapply(unique(df$A), function(x) sl.pred(x))),ncol = 3, byrow = F)
    phat = sapply(1:dim(phat.pre)[1], function(x) phat.pre[x,which(temp[x,])])

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

#### simplest case
out = unconstrained.min(df)
est.psi = out$psi
oracle.psi = .5# if I knew the right f and E(Y|A,X)
f.psi = mean(.5*as.numeric(out$assig == f) + .75*as.numeric(out$assig != f))# if I take this f as fixed, but I knew E(Y|A,X)
table(out$assig, f)

#### repeat with unequal A probs and p* = 0.25 vs p = 0.75
df <- simsort2(5000)
out2 = unconstrained.min(df)
est.psi = out2$psi
oracle.psi = .25# if I knew the right f and E(Y|A,X)
f.psi = mean(.25*as.numeric(out2$assig == f) + .75*as.numeric(out2$assig != f))# if I take this f as fixed, but I knew E(Y|A,X)

table(out$assig, f)
