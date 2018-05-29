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

#### old, not great dataset. all results before 5/29/2018 10am are on this
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

##### using combined opt and sl for both mu and pi ####
out.combo <- constr.opt.causal(df, aLevel = dist.mat, obsD = obsD)
write.csv(out.combo$ifvals, "~jacquelinemauro/Dropbox/sorter/SLifvalsUnconstr.csv")
write.csv(out.combo$assig.vec, "~jacquelinemauro/Dropbox/sorter/SLassigvecUnconstr.csv")
write.csv(out.combo$phihat, "~jacquelinemauro/Dropbox/sorter/SLphihatUnconstr.csv")
write.csv(out.combo$muhat, "~jacquelinemauro/Dropbox/sorter/SLmuhatUnconstr.csv")
write.csv(out.combo$pihat, "~jacquelinemauro/Dropbox/sorter/SLpihatUnconstr.csv")

##### using combined opt and sl for both mu and pi on better dataset ####
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
dist.mat <- dist.mat[to.keep,]

##### output all nuisance parameters and unconstrained estimate ####
out.combo2 <- constr.opt.causal(df, aLevel = dist.mat, obsD = obsD)
write.csv(out.combo2$ifvals, "~jacquelinemauro/Dropbox/sorter/SLifvalsUnconstrNewdat.csv")
write.csv(out.combo2$assig.vec, "~jacquelinemauro/Dropbox/sorter/SLassigvecUnconstrNewdat.csv")
write.csv(out.combo2$phihat, "~jacquelinemauro/Dropbox/sorter/SLphihatUnconstrNewdat.csv")
write.csv(out.combo2$muhat, "~jacquelinemauro/Dropbox/sorter/SLmuhatUnconstrNewdat.csv")

#### calculate constrained value ####
assig.mat <- read.csv('~jacquelinemauro/Dropbox/sorter/prison_assignment_sl.csv', header = F)
assig.mu <- apply(assig.mat,1,which.max)
muhat.mat <- as.matrix(read.csv("~jacquelinemauro/Dropbox/sorter/SLmuhatUnconstrNewdat.csv")[,-1])
pihat.mat <- as.matrix(read.csv("~jacquelinemauro/Dropbox/sorter/SLpihatUnconstrNewdat.csv")[,-1])

muhat <- diag(muhat.mat %*% t(assig.mat))
plug.in.est <- mean(muhat)

pihat <- diag(pihat.mat %*% t(assig.mat))

ifvals <- (as.numeric(df$A == assig.mu)/pihat)*(df$y - muhat) + muhat
est <- mean(ifvals)
sd <- sd(ifvals)/sqrt(length(ifvals))

