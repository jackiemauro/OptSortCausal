#### load and set up prison data ----
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
             which(nm=='friendsnonfamily'),#which(nm=='familyother'),
             which(nm=='numoftotalmisconducts'))
covs <- dat[,cov.loc]

df <- data.frame(y=dat$NCRecid3, A=dat$A, covs)
to.keep <- complete.cases(df)
df <- df[complete.cases(df),]  # highschool grad the most missing, 63 unobserved values
obsD <- dat$total_time[to.keep]
dist.mat <- dist.mat[to.keep,]

write.csv(df, "~jacquelinemauro/CompleteCasesPrisoners.csv")
write.csv(as.numeric(to.keep), "~jacquelinemauro/CompleteCases.csv")

out.rg <- unconstrained.min.rg(df)
write.csv(out.rg$assig, "~jacquelinemauro/OptSortCausal/assignmentVectorRg.csv")

plot(jitter(out.rg$assig.vec), jitter(df$A),
     xlab = "Assigned Treatment", ylab = "Observed Treatment", main = "Movement patterns")

round(100*(table(out.rg$assig) - table(df$A))/table(df$A),2)

plot(c(table(out.rg$assig)) ~ c(table(df$A)), pch = 19,
     xlab = "Observed Counts", ylab = "Counts after Assignment",
     main = "Change in Prisoner Counts before and after Assignment")
abline(0,1)

##### using combined opt and fuller data ####
out <- constr.opt.causal(df, aLevel = dist.mat, obsD = obsD)
