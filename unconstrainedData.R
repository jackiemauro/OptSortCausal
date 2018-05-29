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
dat$A <- apply(pris.dummies,1,which.max)

# using minimum distance prison as a proxy for home location
nm = names(dat)
covs = cbind(dat[,which(nm=='ALB_time'):which(nm=='WAM_time')],
             dat[,which(nm=="loslastloc"):which(nm=='ageyrs')],dat[,which(nm=='total_time')],
             dat[,which(nm=='visitslastloc1'):which(nm=='highschoolgrad')],
             dat[,which(nm=='numofvisitsever')],
             dat[,which(nm=='child')], dat[,which(nm=='parent')], dat[,which(nm=='spouse')],
             dat[,which(nm=='friendsnonfamily')],
             dat[,which(nm=='numofpriormisconducts')], minName.dummies
)

df <- as.data.frame(cbind(dat$NCRecid3, dat$A, covs))
names(df) <- c('y', 'A',sapply(c(1:dim(covs)[2]), function(k) paste('x',k,sep = "")))
df <- df[complete.cases(df),]  # highschool grad the most missing, 63 unobserved values

#
# write.csv(df, "~jacquelinemauro/CompleteCasesPrisoners.csv")
# write.csv(as.numeric(to.keep), "~jacquelinemauro/CompleteCases.csv")

out.rg <- unconstrained.min.rg(df)
write.csv(out.rg$assig, "~jacquelinemauro/OptSortCausal/assignmentVectorRg.csv")

plot(jitter(out.rg$assig.vec), jitter(df$A),
     xlab = "Assigned Treatment", ylab = "Observed Treatment", main = "Movement patterns")

round(100*(table(out.rg$assig) - table(df$A))/table(df$A),2)

plot(c(table(out.rg$assig)) ~ c(table(df$A)), pch = 19,
     xlab = "Observed Counts", ylab = "Counts after Assignment",
     main = "Change in Prisoner Counts before and after Assignment")
abline(0,1)


