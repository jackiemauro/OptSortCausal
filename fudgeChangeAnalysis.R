# Increase "fudge" factor

# for fudge in fudge.range
#   output new constraint
#   optimize in matlab
#   calculate plug in and if


dat <- read.csv('~jacquelinemauro/MergedData.csv')[,-1]
dat <- dat[-which(dat$NCRecid.Event == 'reincarceration'),] #drop if they go back for parole violation

nm = names(dat)
pris.dummies <- dat[,which(names(dat)=='alb'):which(names(dat)=='pit')]
avals <- sort(names(pris.dummies))
pris.dummies = pris.dummies[,avals] #sorts pris.dummies alphabetically so it matches w A
dat$A <- apply(pris.dummies,1,which.max)
dat$A <- factor(avals[dat$A], levels = avals)
dist.df <- dat[,which(nm=='ALB_time'):which(nm=='WAM_time')]
min.prison <- apply(dist.df,1,which.min)
min.prison <- factor(avals[min.prison], levels = avals)

# leave visits and distance out
covs = cbind(dat[,which(nm=="loslastloc"):which(nm=='white')],dat[,which(nm=='urban'):which(nm=='ageyrs')],
             dat[,which(nm=='custody_level'):which(nm=='numofpriorinc')],
             dat[,which(nm=='mh'):which(nm=='highschoolgrad')],
             dat[,which(nm=='numofpriormisconducts')],min.prison
)
# standardize <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}
# covsS = apply(covs[,-c(2,3,5,6,11,12,14)],2,standardize)

df <- data.frame(Y = dat$NCRecid3, A = dat$A, covs)
df[,dim(df)[2]] <- factor(df[,dim(df)[2]], levels = sort(unique(df[,dim(df)[2]] )))
to.keep <- complete.cases(df)
df <- df[complete.cases(df),]  # highschool grad the most missing, 63 unobserved values
names(df) <- c('y', 'A',sapply(c(1:dim(covs)[2]), function(k) paste('x',k,sep = "")))
pris.dummies <- pris.dummies[to.keep,]
obsD <- dat$total_time[to.keep]
visits <- dat$visitslastloc1[to.keep]

nms <- c('Recidivism','Prison','Length of Stay', 'White',
         'Urban',"Prior Arrests" , "Married","Violent","lsir Score","Age",
         "Custody Level","Prior Incarcerations",
         "Mental Health", "High School","Misconducts","Commit County")
procedures = c('Original', 'Unconstrained', 'Constrained', 'Regression', 'Matching')

# constraint is binding  up to about a factor 18 (at 18, estimate is like unconstrained)
fudge.range <- c(1.05, 1.25, 1.5, 2, 5, 10, 20)

pihat.mat <- as.matrix(read.csv('~jacquelinemauro/Dropbox/sorter/NoMediatorsSLpihat.csv')[,-1])
muhat.mat <- as.matrix(read.csv('~jacquelinemauro/Dropbox/sorter/NoMediatorsSLmuhat.csv')[,-1])
fhat <- read.csv('~jacquelinemauro/Dropbox/sorter/NoMediatorsSLassigvecs.csv')[,-1]
fhat <- factor(avals[fhat$assig.vecC], levels = sort(unique(df$A)))

if.out <- pi.out <- matrix(rep(NA,2*length(fudge.range)), ncol = 2)
j = 1
for(fudge in fudge.range){
  constr <- round(table(df$A)*fudge)
  write.csv(constr, "~jacquelinemauro/Dropbox/sorter/temp_constr.csv")

  run_matlab_script("~jacquelinemauro/Dropbox/sorter/constrained_assign.m")

  fC.mat <- read.csv("~jacquelinemauro/Dropbox/sorter/temp_optfhat.csv", header = F)
  f.con <- apply(fC.mat,1,which.max)

  muhat <- diag(muhat.mat %*% t(fC.mat))
  plug.in.est <- mean(muhat)
  output.namePI <- paste("~jacquelinemauro/Dropbox/sorter/PIout",fudge,".csv", sep = "")
  write.csv(cbind(plug.in.est, sd(muhat)), output.namePI)

  pihat <- diag(pihat.mat %*% t(fC.mat))

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

unconstrained <- read.csv('~jacquelinemauro/Dropbox/sorter/NoMediatorsSLifresults.csv')[1,2]

plot.df <- data.frame(if.out, fudge.range)
names(plot.df) <- c('Estimate', 'SD', 'Constraint')

require(ggplot2)
g <- ggplot(plot.df, aes(x = Constraint, y = Estimate)) +
  geom_errorbar(aes(ymin = Estimate - SD, ymax = Estimate + SD), width=.5) +
  geom_point() +
  geom_hline(yintercept = unconstrained, col = 'red')+
  xlab("Loosening of constraint") + ylab("Estimate") + theme_bw()
ggsave(filename = '~jacquelinemauro/Dropbox/sorter/FudgeChange.png',plot = g,height = 5, width = 7)
