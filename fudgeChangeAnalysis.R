# Increase "fudge" factor

# for fudge in fudge.range
#   output new constraint
#   optimize in matlab
#   calculate plug in and if

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

df <- as.data.frame(cbind(dat$NCRecid3, dat$nmA, covs))
to.keep <- complete.cases(df)
df <- df[complete.cases(df),]  # highschool grad the most missing, 63 unobserved values
names(df) <- c('y', 'A',sapply(c(1:dim(covs)[2]), function(k) paste('x',k,sep = "")))
obsD <- dat$total_time[to.keep]
dist.mat <- as.matrix(dist.mat[to.keep,])
pris.dummies <- pris.dummies[to.keep,]

nms <- c('Recidivism','Prison','Length of Stay', 'White',
         'Urban',"Prior Arrests" , "Married","Violent","lsir Score","Age",
         "Custody Level","Prior Incarcerations","Visits at Last Location",
         "Mental Health", "High School","Child Visits",
         "Parent Visits","Spouse Visits","Friends Visits","Misconducts","Distance")

# constraint is binding  up to about a factor 18 (at 18, estimate is like unconstrained)
fudge.range <- c(1.05, 1.25, 1.5, 2, 5, 10, 20)
muhat.mat <- as.matrix(read.csv("~jacquelinemauro/Dropbox/sorter/SLmuhatUnconstrNewdatNmA.csv")[,-1])
pihat.mat <- as.matrix(read.csv("~jacquelinemauro/Dropbox/sorter/SLpihatUnconstrNewdatNmA.csv")[,-1])
fhat <- read.csv("~jacquelinemauro/Dropbox/sorter/SLassigvecUnconstrNewdatNmA.csv")[,-1]
fhat <- factor(fhat, levels = names(pris.dummies))

if.out <- pi.out <- matrix(rep(NA,2*length(fudge.range)), ncol = 2)
j = 1
for(fudge in fudge.range){
  constr <- round(apply(pris.dummies,2,sum)*fudge)
  write.csv(constr, "~jacquelinemauro/Dropbox/sorter/temp_constr.csv")

  run_matlab_script("~jacquelinemauro/Dropbox/sorter/constrained_assign.m")

  f.mat <- read.csv("~jacquelinemauro/Dropbox/sorter/temp_optfhat.csv", header = F)
  f.con <- names(pris.dummies)[apply(f.mat,1,which.max)]

  muhat <- diag(muhat.mat %*% t(f.mat))
  plug.in.est <- mean(muhat)
  output.namePI <- paste("~jacquelinemauro/Dropbox/sorter/PIout",fudge,".csv", sep = "")
  write.csv(cbind(plug.in.est, sd(muhat)), output.namePI)

  pihat <- diag(pihat.mat %*% t(f.mat))

  ifvals <- (as.numeric(df$A == f.con)/pihat)*(df$y - muhat) + muhat
  est <- mean(ifvals)
  sd <- sd(ifvals)/sqrt(length(ifvals))
  output.nameIF <- paste("~jacquelinemauro/Dropbox/sorter/IFout",fudge,".csv", sep = "")
  write.csv(cbind(est, sd), output.nameIF)

  if.out[j,] <- c(est,sd)
  pi.out[j,] <- c(plug.in.est, sd(muhat))
  j = j+1
}

write.csv(if.out, '~jacquelinemauro/Dropbox/sorter/fudgeChangeIF.csv')
write.csv(pi.out, '~jacquelinemauro/Dropbox/sorter/fudgeChangePI.csv')

unconstrained <- read.csv('~jacquelinemauro/Dropbox/sorter/SLestsUnconstrNewdatNmA.csv')[,-1]

plot.df <- data.frame(if.out, fudge.range)
names(plot.df) <- c('Estimate', 'SD', 'Constraint')

require(ggplot2)
g <- ggplot(plot.df, aes(x = Constraint, y = Estimate)) +
  geom_errorbar(aes(ymin = Estimate - SD, ymax = Estimate + SD), width=.5) +
  geom_point() +
  geom_hline(yintercept = unconstrained[1], col = 'red')+
  xlab("Loosening of constraint") + ylab("Estimate") + theme_bw()
ggsave(filename = '~jacquelinemauro/Dropbox/sorter/FudgeChange.png',plot = g,height = 5, width = 7)
