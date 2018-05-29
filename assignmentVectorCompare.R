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

options(na.action='na.pass')
county.f = factor(dat$CountyClass); county.dummies = model.matrix(~county.f)[,-c(1,2)]
minName.f = factor(dat$minTimeName); minName.dummies = model.matrix(~minName.f)[,-c(1,2)]
pris.dummies <- dat[,which(names(dat)=='alb'):which(names(dat)=='pit')]
dist.mat <- dat[,which(nm=='ALB_time'):which(nm=='WAM_time')]
names(dist.mat)<-names(pris.dummies)
dat$A <- apply(pris.dummies,1,which.max)
dat$nmA <- apply(pris.dummies,1,function(x) names(pris.dummies)[which.max(x)])

nm = names(dat)
covs = cbind(dat[,which(nm=="loslastloc"):which(nm=='white')],dat[,which(nm=='urban'):which(nm=='ageyrs')],
             dat[,which(nm=='custody_level'):which(nm=='numofpriorinc')],
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
dist.mat <- as.matrix(dist.mat[to.keep,])

fU <- read.csv("~jacquelinemauro/Dropbox/sorter/SLassigvecUnconstrNewdat.csv")[,-1]
fC <- apply(read.csv('~jacquelinemauro/Dropbox/sorter/prison_assignment_sl.csv', header = F),1,which.max)
fO <- df$A

# set up datasets for each of the assignments -- not run
D <- df; D$dist <- obsD
dfU <- dfC <- D
dfU$A <- as.factor(fU); dfC$A <- as.factor(fC); D$A <- as.factor(df$A)
dfU$dist <- diag(dist.mat %*% t(sapply(unique(df$A), function(x) as.numeric(fU == x))))
dfC$dist <- diag(dist.mat %*% t(sapply(unique(df$A), function(x) as.numeric(fC == x))))

nms <- c('Recidivism','Prison','Length of Stay', 'White',
                               'Urban',"Prior Arrests" , "Married","Violent","lsir Score","Age",
                               "Custody Level","Prior Incarcerations","Visits at Last Location",
                               "Mental Health", "High School","Visits Ever","Child Visits",
                               "Parent Visits","Spouse Visits","Friends Visits","Misconducts","Distance")


########## basic distributional changes ##########
# how often do the vectors agree? almost never
mean(fU==fC)
mean(fO==fC)
mean(fO==fU)
mean((fO==fU)[fU==fC])
mean((fO==fU)&(fU==fC))

# how do the counts at each prison change?
orig.dist <- count(D$A)$freq
unc.dist <- count(fU)$freq
con.dist <- count(fC)$freq

unc.change <- unc.dist - orig.dist
con.change <- con.dist - orig.dist

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

########## compare current recid_a vs. predicted after sort #########
recid.model <- ranger::ranger(y ~ ., data = D, write.forest = T)
pred.recid.unc <- predict(recid.model, data = dfU, type = 'response')$pre
pred.recid.con <- predict(recid.model, data = dfC, type = 'response')$pre

recid.model <- SuperLearner(Y = D$y, X = D[,-1], family = binomial(), SL.library = c("SL.gam","SL.glm","SL.glm.interaction", "SL.mean","SL.ranger"))
pred.recid.unc = c(predict.SuperLearner(recid.model, newdata = dfU, onlySL = TRUE)[[1]])
pred.recid.con = c(predict.SuperLearner(recid.model, newdata = dfC, onlySL = TRUE)[[1]])

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
#print(xtable(unmoved.compare, caption = 'Comparing Prediced Recidivism'), file = "~jacquelinemauro/Dropbox/sorter/compPredRecid_ranger.tex")
print(xtable(unmoved.compare, caption = 'Comparing Prediced Recidivism'), file = "~jacquelinemauro/Dropbox/sorter/compPredRecid_sl.tex")

########## compare current visit_a vs. predicted after sort #########
# check that x11 is "Visits at Last Location"
visit.model <- ranger::ranger(x11 ~ ., data = D, write.forest = T)
pred.visit.unc <- predict(visit.model, data = dfU, type = 'response')$pre
pred.visit.con <- predict(visit.model, data = dfC, type = 'response')$pre

# overall dists
summary(D$x11)
summary(pred.visit.unc)
summary(pred.visit.con)

par(mfrow = c(1,3))
hist(log(D$x11), xlim = c(0,7), main = "Observed", xlab = "Log of Visits Observed")
hist(log(round(pred.visit.unc)), xlim = c(0,7), main = "Unconstrained", xlab = "Log of Visits Predicted")
hist(log(round(pred.visit.con)), xlim = c(0,7), main = "Constrained", xlab = "Log of Visits Predicted")
par(mfrow = c(1,1))

# by prison
curr.visit <- round(ddply(D, .(A), summarize, Visits = mean(x11))$Visits,2)
unc.visit <- round(ddply(data.frame(D, pred.visit.unc), .(A), summarize, Visits = mean(pred.visit.unc))$Visits,2)
con.visit <- round(ddply(data.frame(D, pred.visit.con), .(A), summarize, Visits = mean(pred.visit.con))$Visits,2)
compare.visits <- data.frame(Prison = names(pris.dummies), Original = curr.visit, Unconstrained = unc.visit, Constrained = con.visit,
                             UnconstrainedChange = unc.change, ConstrainedChange = con.change)
print(xtable(compare.visits, caption = 'Comparing Prediced Visits'), file = "~jacquelinemauro/Dropbox/sorter/compPredVisits_ranger.tex")

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
print(xtable(compare.dists, caption = 'Comparing Prediced Visits'), file = "~jacquelinemauro/Dropbox/sorter/compPredVisits_ranger.tex")

# about half of prisoners are moved farther from home
unc.harm <- mean(obsD < pred.dist.unc)
constr.harm <- mean(obsD < pred.dist.con)
########## compare demographic concentration #############
# using ICC

get.icc <- function(covariate){
  i1 = ICCbare(as.factor(A), D[,covariate], D)
  i2 = ICCbare(as.factor(fU), dfU[,covariate], dfU)
  i3 = ICCbare(as.factor(fC), dfC[,covariate], dfC)
  return(c(i1,i2,i3))
}

# in paper, drop visits because those can change based on location
iccs <- as.data.frame(round(100*matrix(unlist(lapply(c(3:dim(D)[2]), function(C) get.icc(C))), ncol = 3, byrow = T),2))
names(iccs) <- c("Observed", "Unconstrained", "Constrained")
rownames(iccs) <- nms[-c(1,2)]
print(xtable(iccs,caption = "Intraclass Correlation Coefficients",label = "tab:ICC"),file = "~jacquelinemauro/Dropbox/sorter/ICC.tex")
