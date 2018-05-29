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

####### load data and vectors #######
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

orig.dist <- count(df$A)$freq
unc.dist <- count(fU)$freq
con.dist <- count(fC)$freq

unc.change <- unc.dist - orig.dist
con.change <- con.dist - orig.dist

# set up datasets for each of the assignments -- not run
D <- df; D$dist <- obsD
dfU <- dfC <- D
dfU$A <- fU; dfC$A <- fC
dfU$dist <- diag(dist.mat %*% t(sapply(unique(df$A), function(x) as.numeric(fU == x))))
dfC$dist <- diag(dist.mat %*% t(sapply(unique(df$A), function(x) as.numeric(fC == x))))

########## study the people who aren't moved #########
# how often do the vectors agree? almost never
mean(fU==fC)
mean(fO==fC)
mean(fO==fU)
mean((fO==fU)[fU==fC])
mean((fO==fU)&(fU==fC))

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

unmoved.C.stats <- round(apply(df[unmovedC,], 2, mean),2)
unmoved.C.stats[2] <- names(pris.dummies)[Mode(df[unmovedC,]$A)]
unmoved.C.stats[21] <- round(mean(obsD[unmovedC]),2)
unmoved.U.stats <- round(apply(df[unmovedU,], 2, mean),2)
unmoved.U.stats[2] <- names(pris.dummies)[Mode(df[unmovedU,]$A)]
unmoved.U.stats[21] <- round(mean(obsD[unmovedU]),2)
all.data.stats <- round(apply(df, 2, mean),2)
all.data.stats[2] <- names(pris.dummies)[Mode(df$A)]
all.data.stats[21] <- round(mean(obsD),2)

unmoved.compare <- data.frame(Original = all.data.stats, Unconstrained = unmoved.U.stats, Constrained = unmoved.C.stats)

print(xtable(unmoved.compare, caption = 'Comparing those not moved'), file = "~jacquelinemauro/Dropbox/sorter/unmovedStats.tex")



##

########## compare current recid_a vs. predicted after sort #########
recid.model <- ranger::ranger(y ~ ., data = D, write.forest = T)
pred.recid.unc <- predict(recid.model, data = dfU, type = 'response')$pre
pred.recid.con <- predict(recid.model, data = dfC, type = 'response')$pre

# overall dists
summary(D$y)
summary(pred.recid.unc)
summary(pred.recid.con)


# by prison
curr.recid <- round(ddply(D, .(A), summarize, Recid = mean(y))$Recid,2)
unc.recid <- round(ddply(data.frame(D, pred.recid.unc), .(A), summarize, Recid = mean(pred.recid.unc))$Recid,2)
con.recid <- round(ddply(data.frame(D, pred.recid.con), .(A), summarize, Recid = mean(pred.recid.con))$Recid,2)
compare.recid <- data.frame(Prison = names(pris.dummies), Original = curr.recid, Unconstrained = unc.recid, Constrained = con.recid,
                            UnconstrainedChange = unc.change, ConstrainedChange = con.change)

########## compare current visit_a vs. predicted after sort #########
visit.model <- ranger::ranger(numofvisitsever ~ ., data = D, write.forest = T)
pred.visit.unc <- predict(visit.model, data = dfU, type = 'response')$pre
pred.visit.con <- predict(visit.model, data = dfC, type = 'response')$pre

# overall dists
summary(D$numofvisitsever)
summary(pred.visit.unc)
summary(pred.visit.con)

par(mfrow = c(1,3))
hist(log(D$numofvisitsever), xlim = c(0,7), main = "Observed", xlab = "Log of Visits Observed")
hist(log(round(pred.visit.unc)), xlim = c(0,7), main = "Unconstrained", xlab = "Log of Visits Predicted")
hist(log(round(pred.visit.con)), xlim = c(0,7), main = "Constrained", xlab = "Log of Visits Predicted")
par(mfrow = c(1,1))

# by prison
curr.visit <- round(ddply(D, .(A), summarize, Visits = mean(numofvisitsever))$Visits,2)
unc.visit <- round(ddply(data.frame(D, pred.visit.unc), .(A), summarize, Visits = mean(pred.visit.unc))$Visits,2)
con.visit <- round(ddply(data.frame(D, pred.visit.con), .(A), summarize, Visits = mean(pred.visit.con))$Visits,2)
compare.visits <- data.frame(Prison = names(pris.dummies), Original = curr.visit, Unconstrained = unc.visit, Constrained = con.visit,
                             UnconstrainedChange = unc.change, ConstrainedChange = con.change)

########## compare current distance_a vs. predicted after sort #########
dist.model <- ranger::ranger(dist ~ ., data = D, write.forest = T)
pred.dist.unc <- predict(dist.model, data = dfU, type = 'response')$pre
pred.dist.con <- predict(dist.model, data = dfC, type = 'response')$pre

# overall dists
summary(obsD)
summary(pred.dist.unc)
summary(pred.dist.con)

par(mfrow = c(1,3))
hist(obsD, xlim = c(0,500), main = "Observed", xlab = "Distances Observed")
hist(pred.dist.unc, xlim = c(0,500), main = "Unconstrained", xlab = "Unconstrained Distances")
hist(pred.dist.con, xlim = c(0,500), main = "Constrained", xlab = "Constrained Distances")
par(mfrow = c(1,1))

# by prison
curr.dist <- round(ddply(D, .(A), summarize, dists = mean(dist))$dists,2)
unc.dist <- round(ddply(data.frame(df, pred.dist.unc), .(A), summarize, dists = mean(pred.dist.unc))$dists,2)
con.dist <- round(ddply(data.frame(df, pred.dist.con), .(A), summarize, dists = mean(pred.dist.con))$dists,2)
compare.dists <- data.frame(Prison = names(pris.dummies), Original = curr.dist, Unconstrained = unc.dist, Constrained = con.dist,
                             UnconstrainedChange = unc.change, ConstrainedChange = con.change)

# more than half of prisoners are moved farther from home
unc.harm <- mean(obsD < pred.dist.unc)
constr.harm <- mean(obsD < pred.dist.con)
########## compare demographic concentration #############
# using ICC

get.icc <- function(covariate){
  i1 = ICCbare(as.factor(A), df[,covariate], D)
  i2 = ICCbare(as.factor(fU), df[,covariate], dfU)
  i3 = ICCbare(as.factor(fC), df[,covariate], dfC)
  return(c(i1,i2,i3))
}

# in paper, drop visits because those can change based on location
iccs <- round(100*matrix(unlist(lapply(c(3:dim(df)[2]), function(C) get.icc(C))), ncol = 3, byrow = T),2)
iccs <- data.frame(covariate = names(df)[3:dim(df)[2]], iccs)
names(iccs) <- c("Covariate", "Observed", "Unconstrained", "Constrained")
print(xtable(iccs,caption = "Intraclass Correlation Coefficients",label = "tab:ICC"),file = "~jacquelinemauro/Dropbox/sorter/ICC.tex")
