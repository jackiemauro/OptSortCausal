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
nm  <- names(dat)

options(na.action='na.pass')
county.f = factor(dat$CountyClass); county.dummies = model.matrix(~county.f)[,-c(1,2)]
minName.f = factor(dat$minTimeName); minName.dummies = model.matrix(~minName.f)[,-c(1,2)]
pris.dummies <- dat[,which(names(dat)=='alb'):which(names(dat)=='pit')]
dist.mat <- dat[,which(nm=='ALB_time'):which(nm=='WAM_time')]
names(dist.mat)<-names(pris.dummies)
dat$A <- apply(pris.dummies,1,which.max)
dat$nmA <- apply(pris.dummies,1,function(x) names(pris.dummies)[which.max(x)])
cov.loc <- c(which(nm=="loslastloc"):which(nm=='white'),which(nm=="urban"):which(nm=='ageyrs'),
             which(nm=="custody_level"):which(nm=="numofpriorinc"),
             which(nm=='visitslastloc1'):which(nm=='highschoolgrad'),
             which(nm=='numofvisitsever'),which(nm=='child'),which(nm=='parent'),which(nm=='spouse'),
             which(nm=='friendsnonfamily'),which(nm=='familyother'),which(nm=='numoftotalmisconducts'))
covs <- dat[,cov.loc]

df <- data.frame(y=dat$NCRecid3, A=dat$A, covs)
to.keep <- complete.cases(df)
df <- df[complete.cases(df),]  # highschool grad the most missing, 63 unobserved values
obsD <- dat$total_time[to.keep]
dist.mat <- dist.mat[to.keep,]

Constr.mat <- read.csv('~jacquelinemauro/Dropbox/sorter/prison_assignment_mu.csv', header = F)

fU <- read.csv("~jacquelinemauro/OptSortCausal/assignmentVectorRg.csv")[,-1]
fC <- apply(Constr.mat,1,which.max)
fO <- df$A

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

unmoved.C.stats <- round(apply(df[unmovedC,], 2, mean),2); unmoved.C.stats[2] <- names(pris.dummies)[Mode(df[unmovedC,]$A)]
unmoved.U.stats <- round(apply(df[unmovedU,], 2, mean),2); unmoved.U.stats[2] <- names(pris.dummies)[Mode(df[unmovedU,]$A)]
all.data.stats <- round(apply(df, 2, mean),2); all.data.stats[2] <- names(pris.dummies)[Mode(df$A)]
unmoved.compare <- data.frame(Original = all.data.stats, Unconstrained = unmoved.U.stats, Constrained = unmoved.C.stats)

print(xtable(unmoved.compare, caption = 'Comparing those not moved'), file = "~jacquelinemauro/Dropbox/sorter/unmovedStats.tex")

