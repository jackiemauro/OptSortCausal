# this is doing a terrible job 
# i'm going back to the june 16/22 results


rm(list = ls())
dat <- read.csv('~jacquelinemauro/MergedData.csv')[,-1]
dat <- dat[-which(dat$NCRecid.Event == 'reincarceration'),] #drop if they go back for parole violation

nm = names(dat)
pris.dummies <- dat[,which(names(dat)=='alb'):which(names(dat)=='pit')]
avals <- sort(names(pris.dummies))
pris.dummies = pris.dummies[,avals]
dist.df <- dat[,which(nm=='ALB_time'):which(nm=='WAM_time')]
names(dist.df)<-avals
dat$A <- apply(pris.dummies,1,which.max)
dat$A <- factor(avals[dat$A], levels = avals)

covsA = cbind(dat[,which(nm=="loslastloc"):which(nm=='white')],dat[,which(nm=='urban'):which(nm=='ageyrs')],
             dat[,which(nm=='custody_level'):which(nm=='numofpriorinc')],
             dat[,which(nm=='visitslastloc1'):which(nm=='highschoolgrad')],
             dat[,which(nm=='child')], dat[,which(nm=='parent')], dat[,which(nm=='spouse')],
             dat[,which(nm=='friendsnonfamily')],
             dat[,which(nm=='numofpriormisconducts')],
             dat[,which(nm=='total_time')]
)
standardize <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}
covs = apply(covsA,2,standardize)
d.coord = dim(covsA)[2] #position of distance in covariate matrix
covs = cbind(covs, pris.dummies)

df <- data.frame(Y = dat$NCRecid3, covs)
to.keep <- complete.cases(df)
df <- df[complete.cases(df),]  # highschool grad the most missing, 63 unobserved values
names(df) <- c('y', sapply(c(1:dim(covs)[2]), function(k) paste('x',k,sep = "")))
dist.df <- dist.df[to.keep,]; dist.mat = matrix(dist.df)
dist.df <- apply(dist.df, 2, standardize)
pris.dummies <- pris.dummies[to.keep,]
at.A <- dat$A[to.keep]

nms <- c('Recidivism','Prison','Length of Stay', 'White',
         'Urban',"Prior Arrests" , "Married","Violent","lsir Score","Age",
         "Custody Level","Prior Incarcerations","Visits at Last Location",
         "Mental Health", "High School","Child Visits",
         "Parent Visits","Spouse Visits","Friends Visits","Misconducts","Distance")

Avals = levels(dat$A)
sum(Avals != names(dist.df)) #check dist.df in right order
sum(Avals != names(pris.dummies)) #check dummies in right order

sl.lib = c("SL.glmnet","SL.glm.interaction", "SL.mean","SL.ranger","SL.rpart","SL.earth","SL.xgboost")
# set up for train/test
n = dim(df)[1]; nsplits = 2
s <- sample(rep(1:nsplits,ceiling(n/nsplits))[1:n])

IF.U <- IF.C <- IF.Am <- IF.Ar <- SD.Am <- SD.Ar <- SD.U<-SD.C <- rep(NA, nsplits)
PI.U <- PI.C <- SD.PIU <- SD.PIC <- SD.PIAm <- SD.PIAr <- rep(NA, nsplits)
assig.vecU <- assig.vecC<- assig.vecAr <- assig.vecAm <- assig.vecT <- infl.funU <- infl.funC <- infl.funcAr <- infl.funcAm <- rep(NA, n)
pihat.mat <- muhat.mat <- muFhat.mat <- matrix(rep(NA, n*length(Avals)), ncol = length(Avals))
for(vfold in 1:nsplits){
  
  # make training/testing sets
  train.df = df[s==vfold,]; test.df = df[s!=vfold,]
  
  # output the constraint
  constr <- round(apply(pris.dummies[s!=vfold,],2,sum) * 1.05)
  write.csv(constr, '~jacquelinemauro/Dropbox/sorter/capacityFudgeNm.csv')

  # get nuisance models and predictions
  mu.model = SuperLearner(Y = train.df$y, X = train.df[,-1], family = binomial(), SL.library = sl.lib)
  newdat = test.df[,-1]
  muhat <- pihat <- matrix(rep(NA,length(Avals)*dim(test.df)[1]), ncol = length(Avals))
  for(i in 1:length(Avals)){
    newdat[,d.coord] = dist.df[s!=vfold,i] 
    newdum <- pris.dummies[s!=vfold,]; newdum[newdum==1] <- 0; newdum[,i] <- 1
    newdat[,(d.coord+1):dim(newdat)[2]] = newdum
    muhat[,i] = predict.SuperLearner(object = mu.model, newdata = newdat, onlySL = T)$pred
  }
  newdat = test.df[,c(2:(d.coord+1))]
  for(i in 1:length(Avals)){
    sl.lib.pi = c("SL.glmnet","SL.glm.interaction", "SL.mean","SL.ranger","SL.rpart","SL.lda","SL.xgboost","SL.polymars")
    pi.model = SuperLearner(Y = as.numeric(at.A[s==vfold] == Avals[i]), X = train.df[,c(2:(d.coord+1))], family = binomial(), SL.library = sl.lib.pi)
    newdat[,d.coord] = dist.df[s!=vfold,i] 
    pihat[,i] = predict.SuperLearner(object = pi.model, newdata = newdat, onlySL = T)$pred
  }
  
  # truncate extreme pi values
  print(paste(sum(pihat < 1e-3 | pihat > 1-1e-3), 'extreme pi values'))
  pihat[pihat < 1e-3] = 1e-3
  pihat[pihat > 1-1e-3] = 1-1e-3
  
  # output files
  pihat.mat[s!=vfold,] = pihat
  muhat.mat[s!=vfold,] = muhat
  
  # get unconstrained plug in and if estimates
  fU = apply(muhat,1,which.min)
  pluginU = mean(apply(muhat,1,min))
  fU.mat = sapply(c(1:length(Avals)), function(a) as.numeric(fU == a))
  pihatU = diag(pihat %*% t(fU.mat))
  muhatU = diag(muhat %*% t(fU.mat))
  ifU = (as.numeric(at.A[s!=vfold] == Avals[fU])/pihatU)*(test.df$y - muhatU) + muhatU
  psiU = mean(ifU)
  sdU = sd(ifU)/sqrt(length(muhatU))
  
  # get constrained estimates
  write.csv(constr, "~jacquelinemauro/Dropbox/sorter/capacityFudgeNm.csv")
  write.csv(muhat, "~jacquelinemauro/Dropbox/sorter/SLmuhatUnconstrNewdatNmA.csv")
  run_matlab_script("~jacquelinemauro/Dropbox/sorter/prison_assignment_sl_nm.m")
  
  fC.mat = read.csv("~jacquelinemauro/Dropbox/sorter/prison_assignment_sl_nmA.csv", header = F)
  fC = apply(fC.mat,1,which.max)
  pihatC = diag(pihat %*% t(fC.mat))
  muhatC = diag(muhat %*% t(fC.mat))
  pluginC = mean(muhatC)
  ifC = (as.numeric(at.A[s!=vfold] == Avals[fC])/pihatC)*(test.df$y - muhatC) + muhatC
  psiC = mean(ifC)
  sdC = sd(ifC)/sqrt(length(muhatC))
  
  # get appromximate constrained regression-based estimates
  muFhat <- matrix(rep(NA,length(Avals)*dim(test.df)[1]), ncol = length(Avals))
  newdat = train.df[,-1]
  for(i in 1:length(Avals)){
    newdat[,d.coord] = dist.df[s==vfold,i] 
    newdum <- pris.dummies[s!=vfold,]; newdum[newdum==1] <- 0; newdum[,i] <- 1
    newdat[,(d.coord+1):dim(newdat)[2]] = newdum
    muFhat[,i] = predict.SuperLearner(object = mu.model, newdata = newdat, onlySL = T)$pred
  }
  muFhat.mat[s!=vfold,] = muFhat
  
  constr.train <- round(apply(pris.dummies[s==vfold,],2,sum) * 1.05)
  write.csv(constr.train, "~jacquelinemauro/Dropbox/sorter/optConstr.csv")
  write.csv(muFhat, "~jacquelinemauro/Dropbox/sorter/optMuhat.csv")
  run_matlab_script("~jacquelinemauro/Dropbox/sorter/approxConstr.m")
  
  fAr.mat <- read.csv("~jacquelinemauro/Dropbox/sorter/optfhat.csv", header = F)
  fAr = apply(fAr.mat,1,which.max)
  fAr = factor(Avals[fAr], levels = Avals)
  
  class.df <- data.frame(A = fAr, muFhat)
  muF.model <- ranger::ranger(A~., data = class.df, write.forest = TRUE)
  fArhat <- predict(muF.model, data.frame(muhat), type='response')$pre
  fArhat.mat <- sapply(Avals, function(a) as.numeric(fArhat == a))
  
  pihatAr = diag(pihat %*% t(fArhat.mat))
  muhatAr = diag(muhat %*% t(fArhat.mat))
  pluginAr = mean(muhatAr)
  ifAr = (as.numeric(at.A[s!=vfold] == Avals[fArhat])/pihatAr)*(test.df$y - muhatAr) + muhatAr
  psiAr = mean(ifAr)
  sdAr = sd(ifAr)/sqrt(length(muhatAr))
  
  # get appromximate constrained regression-based estimates
  match.data <- data.frame(rbind(cbind(rep(0,dim(muhat)[1]),muhat),cbind(rep(1,dim(muFhat)[1]),muFhat)))
  names(match.data)[1]<-'Group'
  form = formula(paste('Group~', paste('X',c(2:(dim(muhat)[2]+1)), sep = "", collapse = "+"), sep = ""))
  match.it <- matchit(form, data = match.data, method="nearest", ratio=1)
  fAm = fAr[as.numeric(match.it$match.matrix)]
  fAmhat = factor(fAm, levels = Avals)
  
  fAmhat.mat <- sapply(Avals, function(a) as.numeric(fAmhat == a))
  
  pihatAm = diag(pihat %*% t(fAmhat.mat))
  muhatAm = diag(muhat %*% t(fAmhat.mat))
  pluginAm = mean(muhatAm)
  ifAm = (as.numeric(test.df$A == Avals[fAmhat])/pihatAm)*(test.df$y - muhatAm) + muhatAm
  psiAm = mean(ifAm)
  sdAm = sd(ifAm)/sqrt(length(muhatAm))
  
  # scalars
  IF.U[vfold] = psiU
  IF.C[vfold] = psiC
  IF.Ar[vfold] = psiAr
  IF.Am[vfold] = psiAm
  
  SD.U[vfold] = sdU
  SD.C[vfold] = sdC
  SD.Ar[vfold] = sdAr
  SD.Am[vfold] = sdAm
  
  PI.U[vfold] = pluginU
  PI.C[vfold] = pluginC
  PI.Ar[vfold] = pluginAr
  PI.Am[vfold] = pluginAm
  
  SD.PIU[vfold] = sd(muhatU)/sqrt(length(muhatU))
  SD.PIC[vfold] = sd(muhatC)/sqrt(length(muhatC))
  SD.PIAr[vfold] = sd(muhatAr)/sqrt(length(muhatAr))
  SD.PIAm[vfold] = sd(muhatAm)/sqrt(length(muhatAm))
  
  # vectors
  assig.vecU[s!=vfold] = fU
  assig.vecC[s!=vfold] = fC
  assig.vecAr[s!=vfold] = fArhat
  assig.vecAm[s!=vfold] = fAmhat
  assig.vecT[s!=vfold] = fAr
  infl.funU[s!=vfold] = ifU
  infl.funC[s!=vfold] = ifC
  infl.funAr[s!=vfold] = ifAr
  infl.funAm[s!=vfold] = ifAm

}
  
if.results = matrix(c(mean(IF.U),mean(IF.C),mean(IF.Ar),mean(IF.Am),
                   mean(SD.U), mean(SD.C), mean(SD.Ar), mean(SD.Am)),
                 nrow = 2, byrow = T)
write.csv(results, 'checkfileifresults.csv')

pi.results = matrix(c(mean(PI.U),mean(PI.C), mean(PI.Ar), mean(PI.Am),
                    mean(SD.U),mean(SD.C), mean(SD.Ar), mean(SD.Am)),
                    nrow = 2, byrow = T)
write.csv(results, 'checkfilepiresults.csv')

assig.vecs = cbind(assig.vecU,assig.vecC,assig.vecAr,assig.vecAm, assig.vecT)
write.csv(assig.vecs, 'checkfileassigvecs.csv')

write.csv(pihat.mat, 'checkfilepihat.csv')
write.csv(muhat.mat, 'checkfilemuhat.csv')
  







