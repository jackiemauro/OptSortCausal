#### output phi values to put in linear optimization

#### helper functions ----
sl.lib = c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger")
pi.pred.rg <- function(a.val, train.df, Xtrain, Xtest, aMat.train, aMat.test){
  library('ranger')
  d <- cbind(as.factor(a.val==train.df$A),Xtrain); names(d)<-c('A',names(Xtrain))
  d$obsD <- aMat.train[,a.val]
  Xtest$obsD <- aMat.test[,a.val]
  preds <- predict(ranger::ranger(A~., data = d, write.forest = T, probability = T), Xtest)$pre
  return(preds[,2])
}
pi.pred <- function(a.val, train.df, Xtrain, Xtest, aMat.train, aMat.test, sl.lib = c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger")){
  Xtrain$obsD <- aMat.train[,a.val]
  Xtest$obsD <- aMat.test[,a.val]
  out = SuperLearner(Y = as.numeric(train.df$A==a.val), X = Xtrain, family = binomial(), SL.library = sl.lib,
                     newX = Xtest)$SL.predict
  return(out)
}
pi.pred.lg <- function(a.val, train.df, Xtrain, Xtest, aMat.train, aMat.test){
  d <- cbind(as.numeric(a.val==train.df$A),Xtrain); names(d)<-c('A',names(Xtrain))
  d$obsD <- aMat.train[,a.val]
  Xtest$obsD <- aMat.test[,a.val]
  preds <- predict(glm(A~., data = d, family = binomial), newdata = Xtest, type = 'response')
  return(c(preds))
}
mu.pred <- function(a.val, train.df, test.df, aLevel){
  mu <- ranger::ranger(y~., data = train.df, write.forest = TRUE)
  t <- test.df; t$A <- a.val; t$obsD <- aLevel[,a.val]
  preds <- predict(mu, t)$pre
  return(preds)
}
make.phihat <- function(df,aLevel,obsD){
  Avals <- unique(df$A); n.avals <- length(Avals)
  phihat <- out.pi <- out.mu <- matrix(rep(NA,n.avals*dim(df)[1]), ncol = n.avals)
  s <- as.numeric(runif(dim(df)[1]) < .5) + 1
  for (rnd in 1:2){
    train.df <- df[s==rnd,]; train.df$obsD <- obsD[s==rnd]
    test.df <- df[s!=rnd,]; test.df$obsD <- obsD[s!=rnd]
    Xtrain <- train.df[,-c(1:2)]
    Xtest <- test.df[,-c(1:2)]
    aMat.train <- aLevel[s==rnd,]
    aMat.test <- aLevel[s!=rnd,]

    # train and predict mu and pi models
    muhat <- lapply(Avals, function(a) mu.pred(a.val = a, train.df = train.df, test.df = test.df, aLevel = aMat.test))
    pihat <- lapply(Avals, function(a) pi.pred.rg(a.val = a, train.df = train.df, Xtrain = Xtrain, Xtest = Xtest, aMat.train=aMat.train, aMat.test=aMat.test))

    # truncate pi values
    for(i in 1:25){pihat[[i]][which(pihat[[i]]<1e-3)] = 1e-3}
    for(i in 1:25){pihat[[i]][which(pihat[[i]]>1-1e-3)] = 1-1e-3}

    # output phihat matrix, pi's and mu's
    out.pi[s!=rnd,] <- matrix(unlist(pihat), ncol = n.avals)
    out.mu[s!=rnd,] <- matrix(unlist(muhat), ncol = n.avals)
    phihat[s!=rnd,] <- sapply(1:n.avals, function(a) (as.numeric(test.df$A == Avals[a])/pihat[[a]])*(test.df$y - muhat[[a]]) + muhat[[a]])
  }
  return(list(phihat = phihat, muhat = out.mu, pihat = out.pi))
}

#### load and set up prison data ----
dat <- read.csv('~jacquelinemauro/MergedData.csv')[,-1]
dat$no.visits.last.loc <- (1 - dat$visitslastlocyn1)
dat$no.childvisits <- (1 - dat$childyn)
dat$maxtime <- apply(dat[,2:26],1,max)
dat <- dat[-which(dat$NCRecid.Event == 'reincarceration'),] #drop if they go back for parole violation

options(na.action='na.pass')
county.f = factor(dat$CountyClass); county.dummies = model.matrix(~county.f)[,-c(1,2)]
minName.f = factor(dat$minTimeName); minName.dummies = model.matrix(~minName.f)[,-c(1,2)]
pris.dummies <- dat[,which(names(dat)=='alb'):which(names(dat)=='pit')]
dist.mat <- dat[,which(nm=='ALB_time'):which(nm=='WAM_time')]
names(dist.mat)<-names(pris.dummies)
dat$A <- apply(pris.dummies,1,which.max)
dat$nmA <- apply(pris.dummies,1,function(x) names(pris.dummies)[which.max(x)])

# using minimum distance prison as a proxy for home location
nm = names(dat)
covs = cbind(dat[,which(nm=="loslastloc"):which(nm=='ageyrs')],#dat[,which(nm=='total_time')],
             dat[,which(nm=='visitslastloc1'):which(nm=='highschoolgrad')],
             dat[,which(nm=='numofvisitsever')],
             dat[,which(nm=='child')], dat[,which(nm=='parent')], dat[,which(nm=='spouse')],
             dat[,which(nm=='friendsnonfamily')],
             dat[,which(nm=='numofpriormisconducts')]
)

df <- as.data.frame(cbind(dat$NCRecid3, dat$A, covs))
to.keep <- complete.cases(df)
df <- df[complete.cases(df),]  # highschool grad the most missing, 63 unobserved values
names(df) <- c('y', 'A',sapply(c(1:dim(covs)[2]), function(k) paste('x',k,sep = "")))
obsD <- dat$total_time[to.keep]
dist.mat <- dist.mat[to.keep,]

# allow 5% wiggle room
constr <- round(apply(pris.dummies[to.keep,],2,sum) * 1.05)
write.csv(constr, '~jacquelinemauro/Dropbox/sorter/capacityFudge.csv')

# no wiggle room
constr <- apply(pris.dummies[to.keep,],2,sum)
write.csv(constr, '~jacquelinemauro/Dropbox/sorter/capacity.csv')

#### output phihat matrix ----
out = make.phihat(df, aLevel = dist.mat, obsD = obsD)
write.csv(out[[1]], '~jacquelinemauro/Dropbox/sorter/phihat_logistic.csv')

# run prison_assignmentJM.m file in matlab
# really the fudge and no fudge should output different things but w/e
# 614 prisoners are assigned differently if we fudge

#assignedP <- read.csv('~jacquelinemauro/Dropbox/sorter/prison_assignment.csv', header = F)
assignedP.fudge <- read.csv('~jacquelinemauro/Dropbox/sorter/prison_assignment.csv', header = F)
mean(assignedP.fudge != pris.dummies[to.keep,])

assig.vec <- apply(assignedP,1,which.max)
mean(assig.vec == df$A)
assig.fudge <- apply(assignedP.fudge,1,which.max)
mean(assig.fudge == df$A)

est <- mean(apply(out[[1]]*assignedP.fudge,1,sum))
sd <- sd(apply(out[[1]]*assignedP.fudge,1,sum))/sqrt(dim(df)[1])

plot(c(table(assig.fudge)) ~ c(table(df$A)), pch = 19,
     xlab = "Observed Counts", ylab = "Counts after Constrained Assignment",
     main = "Prisoner Counts before and after Constrained Assignment")
abline(0,1)
