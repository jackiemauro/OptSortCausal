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

sec.level <- c('medium','close', 'medium', 'medium', 'unknown', 'medium', 'maximum',
               'maximum','maximum','maximum','unknown','supermax','medium','close',
               'minimum','medium','minimum','maximum','minimum','medium','medium',
               'close','medium','minimum','minimum')
sec.level <- data.frame(A = names(dist.df), level = sec.level)

# fU <- read.csv("~jacquelinemauro/Dropbox/sorter/SLassigvecUnconstrNewdat.csv")[,-1]
# fC <- apply(read.csv('~jacquelinemauro/Dropbox/sorter/prison_assignment_sl.csv', header = F),1,which.max)
# fO <- df$A

# from named vector version -- all outputs saved in EDA May file
# fU <- read.csv("~jacquelinemauro/Dropbox/sorter/SLassigvecUnconstrNewdatNm.csv")[,-1]
# fC <- apply(read.csv('~jacquelinemauro/Dropbox/sorter/prison_assignment_sl_nm.csv', header = F),1,which.max)
# fO <- df$A
# fA <- read.csv("~jacquelinemauro/Dropbox/sorter/RGassigvecApconstrNewdatNmMu.csv")[,-1]

# including A as a covariate
fU <- read.csv("~jacquelinemauro/Dropbox/sorter/SLassigvecUnconstrNewdatNmA.csv")[,-1]
fC <- apply(read.csv('~jacquelinemauro/Dropbox/sorter/prison_assignment_sl_nmA.csv', header = F),1,which.max)
fO <- df$A
fA <- read.csv("~jacquelinemauro/Dropbox/sorter/SLassigvecApconstrNewdatNmMuA.csv")[,-1]
fAm <- read.csv("~jacquelinemauro/Dropbox/sorter/SLassigvecApconstrMatchNewdatNmMuA.csv")[,-1]


# set up datasets for each of the assignments
D <- df; D$dist <- obsD
dfU <- dfC <- dfA <- D
dfC$A <- names(dist.df)[fC];dfU$A <- fU; dfA$A <- names(dist.df)[fA]
D$A <- names(dist.df)[fO]
dfC$A <- factor(dfC$A, levels = names(dist.df))
dfU$A <- factor(dfU$A, levels = names(dist.df))
dfA$A <- factor(dfA$A, levels = names(dist.df))
D$A <- factor(D$A, levels = names(dist.df))

# dfC <- merge(dfC,sec.level); dfU <- merge(dfU,sec.level); D <- merge(D,sec.level)

# get new distances -- i'm sure there's a cleaner way but w/e
unc.distance <- con.distance <- app.distance <- obsD
for(i in 1:dim(dist.df)[1]){
  unc.distance[i] = dist.df[i, which(names(dist.df)==dfU$A[i])]
  con.distance[i] = dist.df[i, which(names(dist.df)==dfC$A[i])]
  app.distance[i] = dist.df[i, which(names(dist.df)==dfA$A[i])]
}

dfU$dist <- unc.distance
dfC$dist <- con.distance
dfA$dist <- app.distance

nms <- c('Recidivism','Prison','Length of Stay', 'White',
                               'Urban',"Prior Arrests" , "Married","Violent","lsir Score","Age",
                               "Custody Level","Prior Incarcerations","Visits at Last Location",
                               "Mental Health", "High School","Child Visits",
                               "Parent Visits","Spouse Visits","Friends Visits","Misconducts","Distance")

########## basic distributional changes ##########
# how often do the vectors agree? almost never
# constrained and approximat constrained agree 10% of the time
mean(dfU$A==dfC$A)
mean(dfU$A==dfA$A)
mean(dfC$A==dfA$A)
mean(D$A==dfC$A)
mean(D$A==dfU$A)
mean(D$A == dfA$A)
mean((D$A==dfU$A)&(dfU$A==dfC$A))

########## do the count distributions change much? #########
orig.dist <- table(D$A)
unc.dist <- table(dfU$A)
con.dist <- table(dfC$A)
app.dist <- table(dfA$A)

unc.change <- unc.dist - orig.dist
con.change <- con.dist - orig.dist
app.change <- app.dist - orig.dist

png('~jacquelinemauro/Dropbox/sorter/Figures/CountChangeBoth.png')
par(mfrow = c(1,3))
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

plot(c(orig.dist),c(app.dist), xlim = c(50,500),
     xlab = "Observed Counts", ylab = "Approx. Constrained Counts",
     main = "Approx. Constrained",
     pch= 19)
abline(0,1)
par(mfrow = c(1,1))
dev.off()

biggest.loser.unc <- dfU[which(dfU$A %in% names(sort(unc.change)[1:5])),]
biggest.winner.unc <- dfU[which(dfU$A %in% names(tail(sort(unc.change),5))),]
biggest.loser.con <- dfU[which(dfC$A %in% names(sort(con.change)[1:5])),]
biggest.winner.con <- dfU[which(dfC$A %in% names(tail(sort(con.change),5))),]
biggest.loser.app <- dfU[which(dfA$A %in% names(sort(app.change)[1:5])),]
biggest.winner.app <- dfU[which(dfA$A %in% names(tail(sort(app.change),5))),]


dist.diff.biglosebigwin.unc <- data.frame(losers = apply(biggest.loser.unc[,-c(1,2,22)],2,mean),
                                          winners = apply(biggest.winner.unc[,-c(1,2,22)],2,mean))
rownames(dist.diff.biglosebigwin.unc) <- nms[-c(1,2,22)]

dist.diff.biglosebigwin.con <- data.frame(losers = apply(biggest.loser.con[,-c(1,2,22)],2,mean),
                                          winners = apply(biggest.winner.con[,-c(1,2,22)],2,mean))
rownames(dist.diff.biglosebigwin.con) <- nms[-c(1,2,22)]

# do counts change by security level?
temp <- as.data.frame(unc.change); names(temp) <- c('A','change')
temp <- merge(temp,sec.level)
library(plyr)
ddply(temp, .(level), summarize, mean.change = mean(change))

temp <- as.data.frame(con.change); names(temp) <- c('A','change')
temp <- merge(temp,sec.level)
library(plyr)
ddply(temp, .(level), summarize, mean.change = mean(change))

temp <- as.data.frame(app.change); names(temp) <- c('A','change')
temp <- merge(temp,sec.level)
library(plyr)
ddply(temp, .(level), summarize, mean.change = mean(change))

########## study the people who aren't moved #########
unmovedC <- which(dfC$A == D$A)
unmovedU <- which(dfU$A == D$A)
unmovedA <- which(dfA$A == D$A)

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

unmoved.C.stats <- round(apply(D[unmovedC,-c(1,2,22)], 2, mean),2)
unmoved.U.stats <- round(apply(D[unmovedU,-c(1,2,22)], 2, mean),2)
unmoved.A.stats <- round(apply(D[unmovedU,-c(1,2,22)], 2, mean),2)
all.data.stats <- round(apply(D[,-c(1,2,22)], 2, mean),2)
pris.modeC <- names(which.max(table(dfC$A)))
pris.modeU <- names(which.max(table(dfU$A)))
pris.modeA <- names(which.max(table(dfA$A)))
pris.modeO <- names(which.max(table(D$A)))

unmoved.compare <- data.frame(Original = all.data.stats, Unconstrained = unmoved.U.stats, Constrained = unmoved.C.stats, Approximate = unmoved.A.stats)
unmoved.compare <- rbind(unmoved.compare[1,],c(pris.modeO,pris.modeU,pris.modeC,pris.modeA),unmoved.compare[-1,])
rownames(unmoved.compare) <- c('Length of Stay', 'Modal Prison', nms[4:21])
print(xtable(unmoved.compare, caption = 'Comparing those not moved'), file = "~jacquelinemauro/Dropbox/sorter/unmovedStats.tex")

########## compare distance before and after sort #########

summary(obsD)
summary(dfU$dist)
summary(dfC$dist)
summary(dfA$dist)

png('~jacquelinemauro/Dropbox/sorter/Figures/DistanceHistogram.png', width = 600, height = 400)
par(mfrow = c(2,2))
hist(obsD, xlab = "Observed Distance", main = "Observed", xlim = c(0,500), breaks = 10)
hist(dfU$dist, xlab = "Unconstrained Distance", main = "Unconstrained", xlim = c(0,500), breaks = 10)
hist(dfC$dist, xlab = "Constrained Distance", main = "Constrained", xlim = c(0,500), breaks = 10)
hist(dfA$dist, xlab = "Constrained Distance", main = "Approximate", xlim = c(0,500), breaks = 10)
par(mfrow = c(1,1))
dev.off()

# how many are harmed -- about a third
mean(obsD < dfU$dist)
mean(obsD < dfC$dist)
mean(obsD < dfA$dist)

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
muhat.mat <- as.matrix(read.csv("~jacquelinemauro/Dropbox/sorter/SLmuhatUnconstrNewdatNmA.csv")[,-1])
pred.recid.unc <- apply(muhat.mat,1,min)
tempU <- sapply(names(pris.dummies), function(a) as.numeric(dfC$A == a))
pred.recid.con <- diag(as.matrix(muhat.mat) %*% t(tempU))
tempA <- sapply(names(pris.dummies), function(a) as.numeric(dfA$A == a))
pred.recid.app <- diag(as.matrix(muhat.mat) %*% t(tempA))

# overall dists
summary(D$y)
summary(pred.recid.unc)
summary(pred.recid.con)
summary(pred.recid.app)

curr.recid <- data.frame(A = unique(D$A), Original = round(100*sapply(unique(D$A), function(a) mean(D$y[D$A == a])),2),
                         OriginalSD = round(100*sapply(unique(D$A), function(a) sd(D$y[D$A == a])),2))
unc.recid <- data.frame(A = unique(dfU$A), Unconstrained = round(100*sapply(unique(dfU$A), function(a) mean(pred.recid.unc[dfU$A == a])),2),
                         UnconstrainedSD = round(100*sapply(unique(dfU$A), function(a) sd(pred.recid.unc[dfU$A == a])),2))
con.recid <- data.frame(A = unique(dfC$A), Constrained = round(100*sapply(unique(dfC$A), function(a) mean(pred.recid.con[dfC$A == a])),2),
                        ConstrainedSD = round(100*sapply(unique(dfC$A), function(a) sd(pred.recid.con[dfC$A == a])),2))
app.recid <- data.frame(A = unique(dfA$A), Approximate = round(100*sapply(unique(dfA$A), function(a) mean(pred.recid.app[dfA$A == a])),2),
                        ApproximateSD = round(100*sapply(unique(dfA$A), function(a) sd(pred.recid.app[dfA$A == a])),2))
compare.recid <- merge(merge(merge(curr.recid,unc.recid), con.recid),app.recid)

#print(xtable(unmoved.compare, caption = 'Comparing Prediced Recidivism'), file = "~jacquelinemauro/Dropbox/sorter/compPredRecid_ranger.tex")
print(xtable(compare.recid, caption = 'Comparing Prediced Recidivism'), include.rownames = F, file = "~jacquelinemauro/Dropbox/sorter/compPredRecid_sl.tex")

# rounding
unc.recid.rd <- data.frame(A = unique(dfU$A), Unconstrained = round(100*sapply(unique(dfU$A), function(a) mean(round(pred.recid.unc)[dfU$A == a])),2),
                        UnconstrainedSD = round(100*sapply(unique(dfU$A), function(a) sd(round(pred.recid.unc)[dfU$A == a])),2))
con.recid.rd <- data.frame(A = unique(dfC$A), Constrained = round(100*sapply(unique(dfC$A), function(a) mean(round(pred.recid.con)[dfC$A == a])),2),
                        ConstrainedSD = round(100*sapply(unique(dfC$A), function(a) sd(round(pred.recid.con)[dfC$A == a])),2))
app.recid.rd <- data.frame(A = unique(dfC$A), Approximate = round(100*sapply(unique(dfA$A), function(a) mean(round(pred.recid.app)[dfA$A == a])),2),
                           ApproximateSD = round(100*sapply(unique(dfA$A), function(a) sd(round(pred.recid.app)[dfA$A == a])),2))
compare.recid.rd <- merge(merge(merge(curr.recid,unc.recid.rd), con.recid.rd),app.recid.rd)

#print(xtable(unmoved.compare, caption = 'Comparing Prediced Recidivism'), file = "~jacquelinemauro/Dropbox/sorter/compPredRecid_ranger.tex")
print(xtable(compare.recid.rd, caption = 'Comparing Prediced Recidivism'), include.rownames = F, file = "~jacquelinemauro/Dropbox/sorter/compPredRecid_sl_rounded.tex")


png("~jacquelinemauro/Dropbox/sorter/Figures/chg_vs_recid.png")
par(mfrow = c(1,3))
d<-data.frame(A = names(unc.change), change = c(unc.change))
d<-merge(d,unc.recid)
plot(d$change, d$Unconstrained,
     xlab = "Change in number of Inmates",
     ylab = "Predicted Recidivism at New Prison",
     main = "Unconstrained")

d<-data.frame(A = names(con.change), change = c(con.change))
d<-merge(d,con.recid)
plot(d$change, d$Constrained,
     xlab = "Change in number of Inmates",
     ylab = "Predicted Recidivism at New Prison",
     main = "Constrained")
par(mfrow = c(1,1))

d<-data.frame(A = names(app.change), change = c(app.change))
d<-merge(d,app.recid)
plot(d$change, d$Approximate,
     xlab = "Change in number of Inmates",
     ylab = "Predicted Recidivism at New Prison",
     main = "Approximate")
par(mfrow = c(1,1))
dev.off()

########## compare current visit_a vs. predicted after sort #########
# check that x11 is "Visits at Last Location"
#ranger
visit.model <- ranger::ranger(x11 ~ ., data = D, write.forest = T)
pred.visit.unc <- predict(visit.model, data = dfU, type = 'response')$pre
pred.visit.con <- predict(visit.model, data = dfC, type = 'response')$pre
pred.visit.app <- predict(visit.model, data = dfA, type = 'response')$pre

# superlearner -- think this is worse (has lots of errors and big negatives)
visit.model <- SuperLearner(Y = D$x11, X = D[,-which(names(D)=="x11")], SL.library = c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger"))
pred.visit.unc = c(predict.SuperLearner(visit.model, newdata = dfU, onlySL = TRUE)[[1]])
pred.visit.con = c(predict.SuperLearner(visit.model, newdata = dfC, onlySL = TRUE)[[1]])


# overall dists
summary(D$x11)
summary(pred.visit.unc)
summary(pred.visit.con)
summary(pred.visit.app)

# overall dists - rounded
summary(D$x11)
summary(round(pred.visit.unc))
summary(round(pred.visit.con))
summary(round(pred.visit.app))

# chance of <1 visit
mean(D$x11==0)
mean(pred.visit.unc<1)
mean(pred.visit.con<1)
mean(pred.visit.app<1)

# dists of those visited
summary(D$x11[D$x11>0])
summary(pred.visit.unc[pred.visit.unc>=1])
summary(pred.visit.con[pred.visit.con>=1])
summary(pred.visit.app[pred.visit.app>=1])

png("~jacquelinemauro/Dropbox/sorter/Figures/VisitsHists.png", width = 600, height = 400)
par(mfrow = c(2,2))
hist(log(D$x11[D$x11>0]), xlim = c(0,7), main = "Observed", xlab = "Log of Visits Observed")
hist(log(round(pred.visit.unc[pred.visit.unc>=1])), xlim = c(0,7), main = "Unconstrained", xlab = "Log of Visits Predicted")
hist(log(round(pred.visit.con[pred.visit.con>=1])), xlim = c(0,7), main = "Constrained", xlab = "Log of Visits Predicted")
hist(log(round(pred.visit.app[pred.visit.app>=1])), xlim = c(0,7), main = "Approximate", xlab = "Log of Visits Predicted")
par(mfrow = c(1,1))
dev.off()


# by prison
curr.visit <- data.frame(A = unique(D$A), Original = round(sapply(unique(D$A), function(a) mean(D$x11[D$A == a])),2),
                         OriginalSD = round(sapply(unique(D$A), function(a) sd(D$x11[D$A == a])),2))
unc.visit <- data.frame(A = unique(dfU$A), Unconstrained = round(sapply(unique(dfU$A), function(a) mean(pred.visit.unc[dfU$A == a])),2),
                        UnconstrainedSD = round(sapply(unique(dfU$A), function(a) sd(pred.visit.unc[dfU$A==a])),2))
con.visit <- data.frame(A = unique(dfC$A), Constrained = round(sapply(unique(dfC$A), function(a) mean(pred.visit.con[dfC$A == a])),2),
                        ConstrainedSD = round(sapply(unique(dfC$A), function(a) sd(pred.visit.con[dfC$A == a])),2))
app.visit <- data.frame(A = unique(dfA$A), Approximate = round(sapply(unique(dfA$A), function(a) mean(pred.visit.app[dfA$A == a])),2),
                        ApproximateSD = round(sapply(unique(dfA$A), function(a) sd(pred.visit.app[dfA$A == a])),2))

compare.visit <- merge(merge(merge(curr.visit,unc.visit), con.visit), app.visit)
print(xtable(compare.visit, caption = 'Comparing Prediced Visits'), file = "~jacquelinemauro/Dropbox/sorter/compPredVisits_ranger.tex")

# by prison- rounded
unc.visit.rd <- data.frame(A = unique(dfU$A), Unconstrained = round(sapply(unique(dfU$A), function(a) mean(round(pred.visit.unc)[dfU$A == a])),2),
                        UnconstrainedSD = round(sapply(unique(dfU$A), function(a) sd(round(pred.visit.unc)[dfU$A==a])),2))
con.visit.rd <- data.frame(A = unique(dfC$A), Constrained = round(sapply(unique(dfC$A), function(a) mean(round(pred.visit.con)[dfC$A == a])),2),
                        ConstrainedSD = round(sapply(unique(dfC$A), function(a) sd(round(pred.visit.con)[dfC$A == a])),2))
app.visit.rd <- data.frame(A = unique(dfA$A), Approximate = round(sapply(unique(dfA$A), function(a) mean(round(pred.visit.app)[dfA$A == a])),2),
                           ApproximateSD = round(sapply(unique(dfA$A), function(a) sd(round(pred.visit.app)[dfA$A == a])),2))

compare.visit.rd <- merge(merge(merge(curr.visit,unc.visit.rd), con.visit.rd),app.visit.rd)
print(xtable(compare.visit, caption = 'Comparing Predicted Visits'), file = "~jacquelinemauro/Dropbox/sorter/compPredVisits_ranger_rounded.tex")


########## compare demographic concentration #############
# using ICC
library(ICC)
get.icc <- function(covariate){
  i1 = ICCbare(as.factor(A), D[,covariate], D)
  i2 = ICCbare(as.factor(dfU$A), dfU[,covariate], dfU)
  i3 = ICCbare(as.factor(dfC$A), dfC[,covariate], dfC)
  i4 = ICCbare(as.factor(dfA$A), dfA[,covariate], dfA)
  return(c(i1,i2,i3,i4))
}

#icc's for predicted covariates at new location:
#   visits, recidivism, misconducts
icc.predicted.visit <- round(100*c(ICCbare(A,x11,D),
                                   ICCbare(dfU$A,pred.visit.unc),
                                   ICCbare(dfC$A,pred.visit.con),
                                   ICCbare(dfA$A,pred.visit.app)),2)
icc.predicted.recid <- round(100*c(ICCbare(A,y,D),
                                   ICCbare(dfU$A,pred.recid.unc),
                                   ICCbare(dfC$A,pred.recid.con),
                                   ICCbare(dfA$A,pred.recid.app)),2)
icc.predicted.recid.round <- round(100*c(ICCbare(A,y,D),
                                         ICCbare(dfU$A,round(pred.recid.unc)),
                                         ICCbare(dfC$A,round(pred.recid.con)),
                                         ICCbare(dfA$A,round(pred.recid.app))),2)

# in paper, drop visits because those can change based on location
baseline.covs <- which(nms %in% c("Length of Stay", "White","Urban",
                                  "Prior Arrests","Married","Violent",
                                  "lsir Score","Age","Custody Level",
                                  "Prior Incarcerations","Mental Health",
                                  "High School","Misconducts","Distance"))
iccs <- as.data.frame(round(100*matrix(unlist(lapply(baseline.covs, function(C) get.icc(C))), ncol = 4, byrow = T),2))
#iccs <- rbind(icc.predicted.recid, icc.predicted.visit, iccs)
names(iccs) <- c("Observed", "Unconstrained", "Constrained","Approximate")
#rownames(iccs) <- c("Recidivism","Visits Ever", nms[baseline.covs])
rownames(iccs) <- c("Length of Stay", "White","Urban",
                    "Prior Arrests","Married","Violent",
                    "lsir Score","Age","Custody Level",
                    "Prior Incarcerations","Mental Health",
                    "High School","Misconducts","Distance")
print(xtable(iccs,caption = "Intraclass Correlation Coefficients",label = "tab:ICC"),file = "~jacquelinemauro/Dropbox/sorter/ICC.tex")

##### redoing with checkfile_afactor outputs ####

library(xtable)
library(ICC)

#load data
dat <- read.csv('~jacquelinemauro/MergedData.csv')[,-1]
dat <- dat[-which(dat$NCRecid.Event == 'reincarceration'),] #drop if they go back for parole violation

nm = names(dat)
pris.dummies <- dat[,which(names(dat)=='alb'):which(names(dat)=='pit')]
avals <- sort(names(pris.dummies))
pris.dummies = pris.dummies[,avals] #this is the important step, sorts pris.dummies alphabetically
dist.df <- dat[,which(nm=='ALB_time'):which(nm=='WAM_time')]
names(dist.df)<-avals
dat$A <- apply(pris.dummies,1,which.max)
dat$A <- factor(avals[dat$A], levels = avals)

covs = cbind(dat[,which(nm=="loslastloc"):which(nm=='white')],dat[,which(nm=='urban'):which(nm=='ageyrs')],
             dat[,which(nm=='custody_level'):which(nm=='numofpriorinc')],
             dat[,which(nm=='visitslastloc1'):which(nm=='highschoolgrad')],
             dat[,which(nm=='child')], dat[,which(nm=='parent')], dat[,which(nm=='spouse')],
             dat[,which(nm=='friendsnonfamily')],
             dat[,which(nm=='numofpriormisconducts')],
             dat[,which(nm=='total_time')])

df <- data.frame(Y = dat$NCRecid3, A = dat$A, covs)
to.keep <- complete.cases(df)
df <- df[complete.cases(df),]  # highschool grad the most missing, 63 unobserved values
names(df) <- c('y', 'A',sapply(c(1:dim(covs)[2]), function(k) paste('x',k,sep = "")))
dist.df <- dist.df[to.keep,]; dist.mat = matrix(dist.df)
pris.dummies <- pris.dummies[to.keep,]

nms <- c('Recidivism','Prison','Length of Stay', 'White',
         'Urban',"Prior Arrests" , "Married","Violent","lsir Score","Age",
         "Custody Level","Prior Incarcerations","Visits at Last Location",
         "Mental Health", "High School","Child Visits",
         "Parent Visits","Spouse Visits","Friends Visits","Misconducts","Distance")
procedures = c('Original', 'Unconstrained', 'Constrained', 'Approximate Regression Constrained', 'Approximate Matching Constrained')

# load outputs
fs <- read.csv('~jacquelinemauro/Dropbox/sorter/checkfileassigvecs.csv')[,-1]
avals <- sort(unique(df$A))
fs <- data.frame(apply(fs,2, function(a) avals[a]))
fs$assig.vecU <- factor(fs$assig.vecU, levels = avals)
A = df$A
A = factor(df$A, levels = avals)

# results
results <- read.csv("~jacquelinemauro/Dropbox/sorter/checkfileifresults.csv")[,-1]
results$Lower95 <- results[,1] - 1.96*results[,2]
results$Upper95 <- results[,1] + 1.96*results[,2]
rownames(results) <- procedures[-1]
colnames(results) <- c("Estimate", "SD", "Lower 95% CI", "Upper 95% CI")
print(xtable(results), '~jacquelinemauro/Dropbox/sorter/results_checkfile.tex',include.rownames = T, type = 'latex')

# how different are the assignments?
diffs <- matrix(rep(NA,dim(fs)[2]*length(avals)), nrow = length(avals))
for(i in 1:dim(fs)[2]){diffs[,i] <- table(fs[,i])-table(df$A)}

quintsU = quantile(diffs[,1],probs = seq(from = 1/5, to = 1,length.out = 5))
top5U = data.frame(A = avals[which(diffs[,1]>=quintsU[4])], change = diffs[which(diffs[,1]>=quintsU[4]),1])
bot5U = data.frame(A = avals[which(diffs[,1]<=quintsU[1])], change = diffs[which(diffs[,1]<=quintsU[1]),1])
print(xtable(top5U),'~jacquelinemauro/Dropbox/sorter/EDAoutputs/top5U.tex',include.rownames = F, type = 'latex')
print(xtable(bot5U),'~jacquelinemauro/Dropbox/sorter/EDAoutputs/bot5U.tex',include.rownames = F, type = 'latex')

quintsC = quantile(diffs[,2],probs = seq(from = 1/5, to = 1,length.out = 5))
top5C = data.frame(A = avals[which(diffs[,2]>=quintsC[4])], change = diffs[which(diffs[,2]>=quintsC[4]),2])
bot5C = data.frame(A = avals[which(diffs[,2]<=quintsC[1])], change = diffs[which(diffs[,2]<=quintsC[1]),2])
print(xtable(top5C),'~jacquelinemauro/Dropbox/sorter/EDAoutputs/top5C.tex',include.rownames = F, type = 'latex')
print(xtable(bot5C),'~jacquelinemauro/Dropbox/sorter/EDAoutputs/bot5C.tex',include.rownames = F, type = 'latex')

quintsAr = quantile(diffs[,3],probs = seq(from = 1/5, to = 1,length.out = 5))
top5Ar = data.frame(A = avals[which(diffs[,3]>=quintsAr[4])], change = diffs[which(diffs[,3]>=quintsAr[4]),3])
bot5Ar = data.frame(A = avals[which(diffs[,3]<=quintsAr[1])], change = diffs[which(diffs[,3]<=quintsAr[1]),3])
print(xtable(top5Ar),'~jacquelinemauro/Dropbox/sorter/EDAoutputs/top5Ar.tex',include.rownames = F, type = 'latex')
print(xtable(bot5Ar),'~jacquelinemauro/Dropbox/sorter/EDAoutputs/bot5Ar.tex',include.rownames = F, type = 'latex')

quintsAm = quantile(diffs[,4],probs = seq(from = 1/5, to = 1,length.out = 5))
top5Am = data.frame(A = avals[which(diffs[,4]>=quintsAm[4])], change = diffs[which(diffs[,4]>=quintsAm[4]),4])
bot5Am = data.frame(A = avals[which(diffs[,4]<=quintsAm[1])], change = diffs[which(diffs[,4]<=quintsAm[1]),4])
print(xtable(top5Am),'~jacquelinemauro/Dropbox/sorter/EDAoutputs/top5Am.tex',include.rownames = F, type = 'latex')
print(xtable(bot5Am),'~jacquelinemauro/Dropbox/sorter/EDAoutputs/bot5Am.tex',include.rownames = F, type = 'latex')

top.changes = cbind(top5U,top5C,top5Ar,top5Am)
print(xtable(top.changes),'~jacquelinemauro/Dropbox/sorter/EDAoutputs/top5s.tex',include.rownames = F, type = 'latex')
bot5C = bot5C[-c(5,6),] #png/pit also lose 5
bot5Am = bot5Am[-c(5,6),] #png/pit also lose 5
bot.changes = cbind(bot5U,bot5C,bot5Ar,bot5Am)
print(xtable(bot.changes),'~jacquelinemauro/Dropbox/sorter/EDAoutputs/bot5s.tex',include.rownames = F, type = 'latex')

png("~jacquelinemauro/Dropbox/sorter/EDAoutputs/RecidvsChange.png")
par(mfrow = c(2,2))
for(i in 2:5){
  plot(tapply(X = recids$original, INDEX = df$A, FUN = mean),diffs[,(i-1)],
       xlab = 'Observed Recidivism', ylab = "Change", main = procedures[i])
}
par(mfrow = c(1,1))
dev.off()

png("~jacquelinemauro/Dropbox/sorter/EDAoutputs/PredRecidvsChange.png")
par(mfrow = c(2,2))
for(i in 2:5){
  plot(tapply(X = recids[,i], INDEX = df$A, FUN = mean),diffs[,(i-1)],
       xlab = 'Predicted Recidivism', ylab = "Change", main = procedures[i])
}
par(mfrow = c(1,1))
dev.off()

mean(df$A==fs$assig.vecU)
mean(df$A==fs$assig.vecC)
mean(df$A==fs$assig.vecAr)
mean(df$A==fs$assig.vecAm)
mean(fs$assig.vecU==fs$assig.vecC)
mean(fs$assig.vecC==fs$assig.vecAr)
mean(fs$assig.vecC==fs$assig.vecAm)

dont.move <- c(mean(df$A==fs$assig.vecU),mean(df$A==fs$assig.vecC),mean(df$A==fs$assig.vecAr),mean(df$A==fs$assig.vecAm))

sim.table <- matrix(rep(NA,16),ncol = 4)
for(i in 1:4){for(j in 1:4){sim.table[i,j] = mean(fs[,i]==fs[,j])}}
sim.table <- round(100*sim.table,2)
sim.table <- rbind(round(100*dont.move,2), sim.table)
rownames(sim.table) <- procedures
colnames(sim.table) <- procedures[-1]

print(xtable(sim.table),'~jacquelinemauro/Dropbox/sorter/EDAoutputs/SimilarityTable.tex',include.rownames = T, type = 'latex')

# is recidivism risk getting concentrated
muhats <- read.csv('~jacquelinemauro/Dropbox/sorter/checkfilemuhat.csv')[,-1]
names(muhats) <- avals
pred.recid.unc <- apply(muhats,1,min)
tempC <- sapply(avals, function(a) as.numeric(fs$assig.vecC == a))
pred.recid.con <- diag(as.matrix(muhats) %*% t(tempC))
tempAr <- sapply(avals, function(a) as.numeric(fs$assig.vecAr == a))
pred.recid.appR <- diag(as.matrix(muhats) %*% t(tempAr))
tempAm <- sapply(avals, function(a) as.numeric(fs$assig.vecAm == a))
pred.recid.appM <- diag(as.matrix(muhats) %*% t(tempAm))

recids <- data.frame(original = df$y, unconstrained = pred.recid.unc, constrained = pred.recid.con,
                     approxR = pred.recid.appR, approxM = pred.recid.appM)
recids.round <- round(recids)

recid.icc <- recid.round.icc <- c()
for(i in 1:5){recid.icc[i] = ICCbare(df$A, recids[,i])}
for(i in 1:5){recid.round.icc[i] = ICCbare(df$A, recids.round[,i])}
allrecid.icc = 100*cbind(recid.icc,recid.round.icc)
rownames(allrecid.icc) = procedures

print(xtable(allrecid.icc),'~jacquelinemauro/Dropbox/sorter/EDAoutputs/recidICCs.tex',include.rownames = T, type = 'latex')



# does distance change?
distO = df$x19
tempU = sapply(avals, function(a) as.numeric(fs$assig.vecU == a))
distU = diag(as.matrix(dist.df) %*% t(tempU))
distC = diag(as.matrix(dist.df) %*% t(tempC))
distAr = diag(as.matrix(dist.df) %*% t(tempAr))
distAm = diag(as.matrix(dist.df) %*% t(tempAm))
dists = data.frame(distO,distU,distC,distAr,distAm); names(dists) <- procedures

dist.harm <- data.frame(Unconstrained = mean(distU > distO),
                        Constrained = mean(distC > distO),
                        Regression = mean(distAr > distO),
                        Matching = mean(distAm > distO))
print(xtable(dist.harm), '~jacquelinemauro/Dropbox/sorter/EDAoutputs/DistanceHarm.tex',include.rownames = T, type = 'latex')

png("~jacquelinemauro/Dropbox/sorter/EDAoutputs/DistanceHistograms.png")
par(mfrow = c(3,2))
for(i in 1:5){hist(dists[,i], main = procedures[i], xlab = "Distance in minutes")}
par(mfrow = c(1,1))
dev.off()
print(xtable(apply(dists,2,summary)), '~jacquelinemauro/Dropbox/sorter/EDAoutputs/DistanceSummaries.tex',include.rownames = T, type = 'latex')

# does predicted visitation change?
visit.model <- ranger::ranger(x11 ~ ., data = df, write.forest = T)
dfU <- df; dfU$A <- fs$assig.vecU; dfU$x19 <- distU
dfC <- df; dfC$A <- fs$assig.vecC; dfU$x19 <- distC
dfAr <- df; dfAr$A <- fs$assig.vecAr; dfU$x19 <- distAr
dfAm <- df; dfAm$A <- fs$assig.vecAm; dfU$x19 <- distAm
pred.visit.unc <- predict(visit.model, data = dfU, type = 'response')$pre
pred.visit.con <- predict(visit.model, data = dfC, type = 'response')$pre
pred.visit.appR <- predict(visit.model, data = dfAr, type = 'response')$pre
pred.visit.appM <- predict(visit.model, data = dfAm, type = 'response')$pre
visits = data.frame(df$x11,pred.visit.unc,pred.visit.con,pred.visit.appR,pred.visit.appM)
names(visits) <- procedures
visits.round <- round(visits)

png("~jacquelinemauro/Dropbox/sorter/EDAoutputs/VisitsHistograms.png")
par(mfrow = c(3,2))
for(i in 1:5){hist(visits[,i], main = procedures[i], xlab = "Visits at Last Location")}
par(mfrow = c(1,1))
dev.off()
png("~jacquelinemauro/Dropbox/sorter/EDAoutputs/LogVisitsHistograms.png")
par(mfrow = c(3,2))
for(i in 1:5){hist(log(visits[,i]), main = procedures[i], xlab = "Visits at Last Location")}
par(mfrow = c(1,1))
dev.off()
png("~jacquelinemauro/Dropbox/sorter/EDAoutputs/LogRoundVisitsHistograms.png")
par(mfrow = c(3,2))
for(i in 1:5){hist(log(visits.round[,i]), main = procedures[i], xlab = "Visits at Last Location")}
par(mfrow = c(1,1))
dev.off()
print(xtable(apply(visits,2,summary)), '~jacquelinemauro/Dropbox/sorter/EDAoutputs/VisitsSummaries.tex',include.rownames = T, type = 'latex')
print(xtable(apply(visits.round,2,summary)), '~jacquelinemauro/Dropbox/sorter/EDAoutputs/RoundVisitsSummaries.tex',include.rownames = T, type = 'latex')
print(xtable(data.frame(apply(visits.round,2,function(a) mean(a<1)))),'~jacquelinemauro/Dropbox/sorter/EDAoutputs/NoVisits.tex',include.rownames = T, type = 'latex')

# why is hou different?
hou.sum = data.frame(Houtzdale = apply(df[df$A=='hou',-c(2)],2,mean), All = apply(df[,-c(2)],2,mean))
rownames(hou.sum) <- nms[-2]
print(xtable(hou.sum),'~jacquelinemauro/Dropbox/sorter/EDAoutputs/HoutzdaleStats.tex',include.rownames = T, type = 'latex')
