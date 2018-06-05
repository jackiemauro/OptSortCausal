# comparing the assignment vectors between constrained and unconstrained

# questions of interest:
#   in what way does predicted recid change; are all high risk ppl getting concentrated?
#   how does the distribution of distance from home change?
#   how many people are moved further from home ("harmed")?
#   is visitation predicted to be higher or lower (or the same)?
#   how does the prison population makeup change?
#     -- demogs: race/age/urban/married/mh/ed
#     -- criminality: length of stay/violent/lsir score/custody level/arrests/inc's/misconducts
#     -- do these become more/less concentrated?

####### load data and vectors (pre May 29) #######
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
cov.loc = c(which(nm=="loslastloc"):which(nm=='ageyrs'),which(nm=='visitslastloc1'):which(nm=='highschoolgrad'),
            which(nm=='numofvisitsever'),which(nm=='child'),which(nm=='parent'),which(nm=='spouse'),
            which(nm=='friendsnonfamily'),which(nm=='numofpriormisconducts'))
covs = dat[,cov.loc]

df <- data.frame(y = dat$NCRecid3, A = dat$A, covs)
to.keep <- complete.cases(df)
df <- df[complete.cases(df),]  # highschool grad the most missing, 63 unobserved values
obsD <- dat$total_time[to.keep]
dist.mat <- dist.mat[to.keep,]

Constr.mat <- read.csv('~jacquelinemauro/Dropbox/sorter/prison_assignment_mu.csv', header = F)

fU <- read.csv("~jacquelinemauro/OptSortCausal/assignmentVectorRg.csv")[,-1]
fC <- apply(Constr.mat,1,which.max)
fO <- df$A

####### load data and vectors (post May 29, better covariates, SL estimates) #######
dat <- read.csv('~jacquelinemauro/MergedData.csv')[,-1]
dat$no.visits.last.loc <- (1 - dat$visitslastlocyn1)
dat$no.childvisits <- (1 - dat$childyn)
dat$maxtime <- apply(dat[,2:26],1,max)
dat <- dat[-which(dat$NCRecid.Event == 'reincarceration'),] #drop if they go back for parole violation

nm = names(dat)
options(na.action='na.pass')
county.f = factor(dat$CountyClass); county.dummies = model.matrix(~county.f)[,-c(1,2)]
minName.f = factor(dat$minTimeName); minName.dummies = model.matrix(~minName.f)[,-c(1,2)]
pris.dummies <- dat[,which(names(dat)=='alb'):which(names(dat)=='pit')]
dist.mat <- dat[,which(nm=='ALB_time'):which(nm=='WAM_time')]
names(dist.mat)<-names(pris.dummies)
dat$A <- apply(pris.dummies,1,which.max)
dat$nmA <- apply(pris.dummies,1,function(x) names(pris.dummies)[which.max(x)])

covs = cbind(dat[,which(nm=="loslastloc"):which(nm=='white')],dat[,which(nm=='urban'):which(nm=='ageyrs')],
             dat[,which(nm=='custody_level'):which(nm=='numofpriorinc')],
             dat[,which(nm=='visitslastloc1'):which(nm=='highschoolgrad')],
             dat[,which(nm=='child')], dat[,which(nm=='parent')], dat[,which(nm=='spouse')],
             dat[,which(nm=='friendsnonfamily')],
             dat[,which(nm=='numofpriormisconducts')]
)

df <- as.data.frame(cbind(dat$NCRecid3, dat$A, covs))
to.keep <- complete.cases(df)
df <- df[complete.cases(df),]  # highschool grad the most missing, 63 unobserved values
names(df) <- c('y', 'A',sapply(c(1:dim(covs)[2]), function(k) paste('x',k,sep = "")))
obsD <- dat$total_time[to.keep]
dist.mat <- as.matrix(dist.mat[to.keep,])
dist.df <- data.frame(dist.mat)

# fU <- read.csv("~jacquelinemauro/Dropbox/sorter/SLassigvecUnconstrNewdat.csv")[,-1]
# fC <- apply(read.csv('~jacquelinemauro/Dropbox/sorter/prison_assignment_sl.csv', header = F),1,which.max)
# fO <- df$A

# from named vector version
fU <- read.csv("~jacquelinemauro/Dropbox/sorter/SLassigvecUnconstrNewdatNm.csv")[,-1]
fC <- apply(read.csv('~jacquelinemauro/Dropbox/sorter/prison_assignment_sl_nm.csv', header = F),1,which.max)
fO <- df$A

# set up datasets for each of the assignments
D <- df; D$dist <- obsD
dfU <- dfC <- D
dfC$A <- names(dist.df)[fC];dfU$A <- names(dist.df)[fU]

# get new distances--not right
tempU = read.csv('~jacquelinemauro/Dropbox/sorter/prison_assignment_sl_nm.csv', header = F)
unc.distance = diag(dist.mat %*% t(tempU))
muhat.mat = read.csv("~jacquelinemauro/Dropbox/sorter/SLmuhatUnconstrNewdatNm.csv")[,-1]
tempC = matrix(rep(0,(dim(muhat)[1]*dim(muhat)[2])), ncol = dim(muhat)[2])
min.mu <- apply(tempC.mat,1,which.min)
for(i in 1:dim(tempC.mat)[1]){tempC[i,min.mu[i]] <- 1}
con.distance = diag(dist.mat %*% t(tempC))

dfU$dist <- unc.distance
dfC$dist <- con.distance

nms <- c('Recidivism','Prison','Length of Stay', 'White',
                               'Urban',"Prior Arrests" , "Married","Violent","lsir Score","Age",
                               "Custody Level","Prior Incarcerations","Visits at Last Location",
                               "Mental Health", "High School","Child Visits",
                               "Parent Visits","Spouse Visits","Friends Visits","Misconducts","Distance")

########## basic distributional changes ##########
# how often do the vectors agree? almost never
mean(fU==fC)
mean(fO==fC)
mean(fO==fU)
mean((fO==fU)[fU==fC])
mean((fO==fU)&(fU==fC))

# how do the counts at each prison change?
orig.dist <- table(D$A)
unc.dist <- table(dfU$A)
con.dist <- table(dfC$A)

unc.change <- unc.dist - orig.dist
con.change <- con.dist - orig.dist

########## do the count distributions change much? #########
png('~jacquelinemauro/Dropbox/sorter/Figures/CountChangeBoth.png')
par(mfrow = c(1,2))
plot(c(orig.dist),c(unc.dist), xlim = c(50,500),
     xlab = "Observed Counts", ylab = "Unconstrained Counts",
     main = "Unconstrained",
     pch= 19)
abline(0,1)

plot(c(orig.dist),c(con.dist), xlim = c(50,500),
     xlab = "Observed Counts", ylab = "Constrained Counts",
     main = "Constrained",
     pch= 19)
abline(0,1)
par(mfrow = c(1,1))
dev.off()

########## study the people who aren't moved #########
unmovedC <- which(fC == fO)
unmovedU <- which(fU == fO)

library(plyr)
library(xtable)
Mode = function(x){
  ta = table(x); tam = max(ta)
  if (all(ta == tam)) mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

unmoved.C.stats <- round(apply(D[unmovedC,-2], 2, mean),2)
unmoved.U.stats <- round(apply(D[unmovedU,-2], 2, mean),2)
all.data.stats <- round(apply(D[,-2], 2, mean),2)
pris.modeC <- names(pris.dummies)[which.max(count(D[unmovedC,2])[,2])]
pris.modeU <- names(pris.dummies)[which.max(count(D[unmovedU,2])[,2])]
pris.modeO <- names(pris.dummies)[which.max(count(D[,2])[,2])]

unmoved.compare <- data.frame(Original = all.data.stats, Unconstrained = unmoved.U.stats, Constrained = unmoved.C.stats)
unmoved.compare <- rbind(unmoved.compare[1,],c(pris.modeO,pris.modeU,pris.modeC),unmoved.compare[-1,])
rownames(unmoved.compare) <- nms
print(xtable(unmoved.compare, caption = 'Comparing those not moved'), file = "~jacquelinemauro/Dropbox/sorter/unmovedStats.tex")

########## compare distance before and after sort #########

png('~jacquelinemauro/Dropbox/sorter/Figures/DistanceHistogram.png')
par(mfrow = c(1,3))
hist(obsD, xlab = "Observed Distance", main = "Observed", xlim = c(0,500))
hist(dfU$dist, xlab = "Unconstrained Distance", main = "Unconstrained", xlim = c(0,500))
hist(dfC$dist, xlab = "Constrained Distance", main = "Constrained", xlim = c(0,500))
par(mfrow = c(1,1))
dev.off()

# how many are harmed -- a littl less than half
mean(obsD < dfU$dist)
mean(obsD < dfC$dist)

########## compare current recid_a vs. predicted after sort #########
# ranger
recid.model <- ranger::ranger(y ~ ., data = D, write.forest = T)
pred.recid.unc <- predict(recid.model, data = dfU, type = 'response')$pre
pred.recid.con <- predict(recid.model, data = dfC, type = 'response')$pre

# superlearner
recid.model <- SuperLearner(Y = D$y, X = D[,-1], family = binomial(), SL.library = c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger"))
pred.recid.unc = c(predict.SuperLearner(recid.model, newdata = dfU, onlySL = TRUE)[[1]])
pred.recid.con = c(predict.SuperLearner(recid.model, newdata = dfC, onlySL = TRUE)[[1]])

# from the original output -- use this one
muhat.mat <- as.matrix(read.csv("~jacquelinemauro/Dropbox/sorter/SLmuhatUnconstrNewdatNm.csv")[,-1])
pred.recid.unc <- apply(muhat.mat,1,min)
pred.recid.con <- diag(as.matrix(muhat.mat) %*% t(tempU)) #check this is right order

# overall dists
summary(D$y)
summary(pred.recid.unc)
summary(pred.recid.con)

curr.recid <- data.frame(A = unique(D$A), Original = round(100*sapply(unique(D$A), function(a) mean(D$y[D$A == a])),2),
                         OriginalSD = round(100*sapply(unique(D$A), function(a) sd(D$y[D$A == a])),2))
unc.recid <- data.frame(A = unique(fU), Unconstrained = round(100*sapply(unique(fU), function(a) mean(pred.recid.unc[fU == a])),2),
                         UnconstrainedSD = round(100*sapply(unique(fU), function(a) sd(pred.recid.unc[fU == a])),2))
con.recid <- data.frame(A = unique(fC), Constrained = round(100*sapply(unique(fC), function(a) mean(pred.recid.con[fC == a])),2),
                        ConstrainedSD = round(100*sapply(unique(fC), function(a) sd(pred.recid.con[fC == a])),2))
compare.recid <- merge(merge(curr.recid,unc.recid), con.recid)

#print(xtable(unmoved.compare, caption = 'Comparing Prediced Recidivism'), file = "~jacquelinemauro/Dropbox/sorter/compPredRecid_ranger.tex")
print(xtable(compare.recid, caption = 'Comparing Prediced Recidivism'), include.rownames = F, file = "~jacquelinemauro/Dropbox/sorter/compPredRecid_sl.tex")

par(mfrow = c(1,2))
plot(unc.change, unc.recid$Unconstrained,
     xlab = "Change in number of Inmates",
     ylab = "Predicted Recidivism at New Prison",
     main = "Unconstrained")

plot(con.change, con.recid$Constrained,
     xlab = "Change in number of Inmates",
     ylab = "Predicted Recidivism at New Prison",
     main = "Constrained")
par(mfrow = c(1,1))

########## compare current visit_a vs. predicted after sort #########
# check that x11 is "Visits at Last Location"
#ranger
visit.model <- ranger::ranger(x11 ~ ., data = D, write.forest = T)
pred.visit.unc <- predict(visit.model, data = dfU, type = 'response')$pre
pred.visit.con <- predict(visit.model, data = dfC, type = 'response')$pre

# superlearner -- think this is worse (has lots of errors and big negatives)
visit.model <- SuperLearner(Y = D$x11, X = D[,-which(names(D)=="x11")], SL.library = c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger"))
pred.visit.unc = c(predict.SuperLearner(visit.model, newdata = dfU, onlySL = TRUE)[[1]])
pred.visit.con = c(predict.SuperLearner(visit.model, newdata = dfC, onlySL = TRUE)[[1]])


# overall dists
summary(D$x11)
summary(pred.visit.unc)
summary(pred.visit.con)

# chance of <1 visit
mean(D$x11==0)
mean(pred.visit.unc<1)
mean(pred.visit.con<1)

# dists of those visited
summary(D$x11[D$x11>0])
summary(pred.visit.unc[pred.visit.unc>=1])
summary(pred.visit.con[pred.visit.con>=1])

par(mfrow = c(1,3))
hist(log(D$x11[D$x11>0]), xlim = c(0,7), main = "Observed", xlab = "Log of Visits Observed")
hist(log(round(pred.visit.unc[pred.visit.unc>=1])), xlim = c(0,7), main = "Unconstrained", xlab = "Log of Visits Predicted")
hist(log(round(pred.visit.con[pred.visit.con>=1])), xlim = c(0,7), main = "Constrained", xlab = "Log of Visits Predicted")
par(mfrow = c(1,1))


# by prison
curr.visit <- data.frame(A = unique(D$A), Original = round(100*sapply(unique(D$A), function(a) mean(D$x11[D$A == a])),2),
                         OriginalSD = round(100*sapply(unique(D$A), function(a) sd(D$x11[D$A == a])),2))
unc.visit <- data.frame(A = unique(fU), Unconstrained = round(100*sapply(unique(fU), function(a) mean(pred.visit.unc[fU == a])),2),
                        UnconstrainedSD = round(100*sapply(unique(fU), function(a) sd(pred.visit.unc[fU == a])),2))
con.visit <- data.frame(A = unique(fC), Constrained = round(100*sapply(unique(fC), function(a) mean(pred.visit.con[fC == a])),2),
                        ConstrainedSD = round(100*sapply(unique(fC), function(a) sd(pred.visit.con[fC == a])),2))
compare.visit <- merge(merge(curr.visit,unc.visit), con.visit)
print(xtable(compare.visits, caption = 'Comparing Prediced Visits'), file = "~jacquelinemauro/Dropbox/sorter/compPredVisits_ranger.tex")

########## compare demographic concentration #############
# using ICC

get.icc <- function(covariate){
  i1 = ICCbare(as.factor(A), D[,covariate], D)
  i2 = ICCbare(as.factor(fU), dfU[,covariate], dfU)
  i3 = ICCbare(as.factor(fC), dfC[,covariate], dfC)
  return(c(i1,i2,i3))
}

#icc's for predicted covariates at new location:
#   visits, recidivism, misconducts
icc.predicted.visit <- round(100*c(ICCbare(as.factor(A),D[,which(nms=="Visits Ever")],D),ICCbare(as.factor(fU),pred.visit.unc),ICCbare(as.factor(fC),pred.visit.con)),2)
icc.predicted.recid <- round(100*c(ICCbare(as.factor(A),D[,which(nms=="Recidivism")],D),ICCbare(as.factor(fU),pred.recid.unc),ICCbare(as.factor(fC),pred.recid.con)),2)

# in paper, drop visits because those can change based on location
baseline.covs <- which(nms %in% c("Length of Stay", "White","Urban",
                                  "Prior Arrests","Married","Violent",
                                  "lsir Score","Age","Custody Level",
                                  "Prior Incarcerations","Mental Health",
                                  "High School","Misconducts","Distance"))
iccs <- as.data.frame(round(100*matrix(unlist(lapply(baseline.covs, function(C) get.icc(C))), ncol = 3, byrow = T),2))
#iccs <- rbind(icc.predicted.recid, icc.predicted.visit, iccs)
names(iccs) <- c("Observed", "Unconstrained", "Constrained")
#rownames(iccs) <- c("Recidivism","Visits Ever", nms[baseline.covs])
rownames(iccs) <- c("Length of Stay", "White","Urban",
                    "Prior Arrests","Married","Violent",
                    "lsir Score","Age","Custody Level",
                    "Prior Incarcerations","Mental Health",
                    "High School","Misconducts","Distance")
print(xtable(iccs,caption = "Intraclass Correlation Coefficients",label = "tab:ICC"),file = "~jacquelinemauro/Dropbox/sorter/ICC.tex")
