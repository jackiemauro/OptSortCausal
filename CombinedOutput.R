#### old, not great dataset. all results before 5/29/2018 10am are on this####
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

#### June 1: using better dataset ####
# leaving out visits at last location
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
df$A <- factor(df$A, levels = names(pris.dummies))
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

sum.stats.means <- round(apply(df[,-2],2,mean),2)
sum.stats.sds <- round(apply(df[,-2],2,sd),2)
sum.stats.tab <- data.frame(Mean = sum.stats.means, SD = sum.stats.sds)
rownames(sum.stats.tab) <- nms[-c(2,21)]
print(xtable(sum.stats.tab, caption = 'Summary Statistics'), file = "~jacquelinemauro/Dropbox/sorter/summarystats.tex")

# allow 5% wiggle room
D <- df; D$dist <- obsD

# this will be in the order of the prison names
# ie the first entry of the vector is 'alb', the last is 'pit'
constr.nm <- round(apply(pris.dummies[to.keep,],2,sum) * 1.05)
write.csv(constr.nm, '~jacquelinemauro/Dropbox/sorter/capacityFudgeNm.csv')


##### output all nuisance parameters and unconstrained estimate using ranger ####
# bad bad bad
out.combo.rg <- constr.opt.causal(df, aLevel = dist.mat, obsD = obsD,mu.algo = 'ranger', pi.algo = 'ranger2')
write.csv(out.combo.rg$ifvals, "~jacquelinemauro/Dropbox/sorter/RGifvalsUnconstrNewdat.csv")
write.csv(out.combo.rg$assig.vec, "~jacquelinemauro/Dropbox/sorter/RGassigvecUnconstrNewdat.csv")
write.csv(out.combo.rg$phihat, "~jacquelinemauro/Dropbox/sorter/RGphihatUnconstrNewdat.csv")
write.csv(out.combo.rg$muhat, "~jacquelinemauro/Dropbox/sorter/RGmuhatUnconstrNewdat.csv")


##### output all nuisance parameters and unconstrained estimate ####
# June 1, reran this with new code and superlearner and replaced old files
out.combo2 <- constr.opt.causal(df, aLevel = dist.mat, obsD = obsD)
write.csv(out.combo2$ifvals, "~jacquelinemauro/Dropbox/sorter/SLifvalsUnconstrNewdat.csv")
write.csv(out.combo2$assig.vec, "~jacquelinemauro/Dropbox/sorter/SLassigvecUnconstrNewdat.csv")
write.csv(out.combo2$phihat, "~jacquelinemauro/Dropbox/sorter/SLphihatUnconstrNewdat.csv")
write.csv(out.combo2$muhat, "~jacquelinemauro/Dropbox/sorter/SLmuhatUnconstrNewdat.csv")
write.csv(cbind(out.combo2$psi, out.combo2$sd), "~jacquelinemauro/Dropbox/sorter/SLestsdUnconstrNewdat.csv")


##### output all nuisance parameters and unconstrained estimate using named vec, rg ####
dist.df <- data.frame(dist.mat)
out.combo.nm <- constr.opt.causal.nm(df, aLevel = dist.df, obsD = obsD,mu.algo = 'ranger', pi.algo = 'ranger')
write.csv(out.combo.nm$ifvals, "~jacquelinemauro/Dropbox/sorter/RGifvalsUnconstrNewdatNm.csv")
write.csv(out.combo.nm$assig.vec, "~jacquelinemauro/Dropbox/sorter/RGassigvecUnconstrNewdatNm.csv")
write.csv(out.combo.nm$phihat, "~jacquelinemauro/Dropbox/sorter/RGphihatUnconstrNewdatNm.csv")
write.csv(out.combo.nm$muhat, "~jacquelinemauro/Dropbox/sorter/RGmuhatUnconstrNewdatNm.csv")

##### output all nuisance parameters and unconstrained estimate using named vec, sl ####
dist.df <- data.frame(dist.mat)
out.combo.nm <- constr.opt.causal.nm(df, aLevel = dist.df, obsD = obsD,mu.algo = 'superlearner', pi.algo = 'superlearner')
write.csv(out.combo.nm$ifvals, "~jacquelinemauro/Dropbox/sorter/SLifvalsUnconstrNewdatNm.csv")
write.csv(out.combo.nm$assig.vec, "~jacquelinemauro/Dropbox/sorter/SLassigvecUnconstrNewdatNm.csv")
write.csv(out.combo.nm$phihat, "~jacquelinemauro/Dropbox/sorter/SLphihatUnconstrNewdatNm.csv")
write.csv(out.combo.nm$muhat, "~jacquelinemauro/Dropbox/sorter/SLmuhatUnconstrNewdatNm.csv")
write.csv(out.combo.nm$pihat, "~jacquelinemauro/Dropbox/sorter/SLpihatUnconstrNewdatNm.csv")
write.csv(c(out.combo.nm$psi,out.combo.nm$sd),"~jacquelinemauro/Dropbox/sorter/SLestsUnconstrNewdatNm.csv")

#### calculate constrained value ####
assig.mat <- read.csv('~jacquelinemauro/Dropbox/sorter/prison_assignment_sl.csv', header = F)
assig.mu <- apply(assig.mat,1,which.max)
muhat.mat <- as.matrix(read.csv("~jacquelinemauro/Dropbox/sorter/SLmuhatUnconstrNewdat.csv")[,-1])
pihat.mat <- as.matrix(read.csv("~jacquelinemauro/Dropbox/sorter/SLpihatUnconstrNewdat.csv")[,-1])

muhat <- diag(muhat.mat %*% t(assig.mat))
plug.in.est <- mean(muhat)
write.csv(cbind(plug.in.est, sd(muhat)), "~jacquelinemauro/Dropbox/sorter/SLestsdConstrPINewdat.csv")

pihat <- diag(pihat.mat %*% t(assig.mat))

ifvals <- (as.numeric(df$A == assig.mu)/pihat)*(df$y - muhat) + muhat
est <- mean(ifvals)
sd <- sd(ifvals)/sqrt(length(ifvals))
write.csv(cbind(est, sd), "~jacquelinemauro/Dropbox/sorter/SLestsdConstrIFNewdat.csv")

#### calculate constrained value nm ####
assig.mat <- read.csv('~jacquelinemauro/Dropbox/sorter/prison_assignment_sl_nm.csv', header = F)
assig.mu <- apply(assig.mat,1,which.max)
muhat.mat <- as.matrix(read.csv("~jacquelinemauro/Dropbox/sorter/SLmuhatUnconstrNewdatNm.csv")[,-1])
pihat.mat <- as.matrix(read.csv("~jacquelinemauro/Dropbox/sorter/SLpihatUnconstrNewdatNm.csv")[,-1])

muhat <- diag(muhat.mat %*% t(assig.mat))
plug.in.est <- mean(muhat)
write.csv(cbind(plug.in.est, sd(muhat)), "~jacquelinemauro/Dropbox/sorter/SLestsdConstrPINewdat.csv")

pihat <- diag(pihat.mat %*% t(assig.mat))

ifvals <- (as.numeric(df$A == names(dist.df)[assig.mu])/pihat)*(df$y - muhat) + muhat
est <- mean(ifvals)
sd <- sd(ifvals)/sqrt(length(ifvals))
write.csv(cbind(est, sd), "~jacquelinemauro/Dropbox/sorter/SLestsdConstrIFNewdat.csv")

#### calculate approximate constrained value nm predicting fhat by covariates (all distance matrix)####
dist.df <- data.frame(dist.mat)
out.approx.nm <- approx.constr.opt.causal.nm(df, aLevel = dist.df, obsD = obsD, nsplits = 5, mu.algo = 'ranger', pi.algo = 'ranger')
write.csv(out.approx.nm$ifvals, "~jacquelinemauro/Dropbox/sorter/RGifvalsApconstrNewdatNm.csv")
write.csv(out.approx.nm$fhat, "~jacquelinemauro/Dropbox/sorter/RGassigvecApconstrNewdatNm.csv")
write.csv(out.approx.nm$muhat, "~jacquelinemauro/Dropbox/sorter/RGmuhatApconstrNewdatNm.csv")
write.csv(out.approx.nm$pihat, "~jacquelinemauro/Dropbox/sorter/RGpihatApconstrNewdatNm.csv")
write.csv(c(out.approx.nm$psi,out.approx.nm$sd),"~jacquelinemauro/Dropbox/sorter/RGestsApconstrNewdatNm.csv")

assig.mu <- read.csv("~jacquelinemauro/Dropbox/sorter/RGassigvecApconstrNewdatNm.csv")[,-1]
muhat.mat <- as.matrix(read.csv("~jacquelinemauro/Dropbox/sorter/RGmuhatApconstrNewdatNm.csv")[,-1])
assig.mat <- model.matrix(~names(pris.dummies)[assig.mu]-1)

muhat <- diag(muhat.mat %*% t(assig.mat))
plug.in.est <- mean(muhat)
write.csv(cbind(plug.in.est, sd(muhat)), "~jacquelinemauro/Dropbox/sorter/RGestsdApConstrPINewdat.csv")

# with SL
out.approx.nm.sl <- approx.constr.opt.causal.nm(df, aLevel = dist.df, obsD = obsD, nsplits = 2, mu.algo = 'superlearner', pi.algo = 'superlearner')
write.csv(out.approx.nm.sl$ifvals, "~jacquelinemauro/Dropbox/sorter/SLifvalsApconstrNewdatNm.csv")
write.csv(out.approx.nm.sl$fhat, "~jacquelinemauro/Dropbox/sorter/SLassigvecApconstrNewdatNm.csv")
write.csv(out.approx.nm.sl$muhat, "~jacquelinemauro/Dropbox/sorter/SLmuhatApconstrNewdatNm.csv")
write.csv(out.approx.nm.sl$pihat, "~jacquelinemauro/Dropbox/sorter/SLpihatApconstrNewdatNm.csv")
write.csv(c(out.approx.nm.sl$psi,out.approx.nm.sl$sd),"~jacquelinemauro/Dropbox/sorter/SLestsApconstrNewdatNm.csv")

assig.mu <- read.csv("~jacquelinemauro/Dropbox/sorter/SLassigvecApconstrNewdatNm.csv")[,-1]
muhat.mat <- as.matrix(read.csv("~jacquelinemauro/Dropbox/sorter/SLmuhatApconstrNewdatNm.csv")[,-1])
assig.mat <- sapply(Avals, function(x) as.numeric(Avals[assig.mu] == x))

muhat <- diag(muhat.mat %*% t(assig.mat))
plug.in.est <- mean(muhat)
write.csv(cbind(plug.in.est, sd(muhat)), "~jacquelinemauro/Dropbox/sorter/SLestsdApConstrPINewdat.csv")


#### calculate approximate constrained value nm predicting fhat by muhat####
dist.df <- data.frame(dist.mat)
out.approx.nm <- approx.constr.opt.causal.nm(df, aLevel = dist.df, obsD = obsD, nsplits = 2, mu.algo = 'ranger', pi.algo = 'ranger')
write.csv(out.approx.nm$ifvals, "~jacquelinemauro/Dropbox/sorter/RGifvalsApconstrNewdatNmMu.csv")
write.csv(out.approx.nm$fhat, "~jacquelinemauro/Dropbox/sorter/RGassigvecApconstrNewdatNmMu.csv")
write.csv(out.approx.nm$muhat, "~jacquelinemauro/Dropbox/sorter/RGmuhatApconstrNewdatNmMu.csv")
write.csv(out.approx.nm$pihat, "~jacquelinemauro/Dropbox/sorter/RGpihatApconstrNewdatNmMu.csv")
write.csv(c(out.approx.nm$psi,out.approx.nm$sd),"~jacquelinemauro/Dropbox/sorter/RGestsApconstrNewdatNmMu.csv")

# if you want the plug in
muhat.mat <- as.matrix(read.csv("~jacquelinemauro/Dropbox/sorter/RGmuhatApconstrNewdatNmMu.csv")[,-1])
Avals <- names(pris.dummies)
assig.mat <- sapply(Avals, function(x) as.numeric(Avals[assig.mu] == x))
muhat <- diag(muhat.mat %*% t(assig.mat))
plug.in.est <- mean(muhat)
write.csv(cbind(plug.in.est, sd(muhat)), "~jacquelinemauro/Dropbox/sorter/RGestsdApConstrPINewdat.csv")

# using SL -- note: fhat still predicted by ranger classifier based on muhat
dist.df <- data.frame(dist.mat)
out.approx.nm.sl <- approx.constr.opt.causal.nm(df, aLevel = dist.df, obsD = obsD, nsplits = 2, mu.algo = 'superlearner', pi.algo = 'superlearner')
write.csv(out.approx.nm.sl$ifvals, "~jacquelinemauro/Dropbox/sorter/SLifvalsApconstrNewdatNmMu.csv")
write.csv(out.approx.nm.sl$fhat, "~jacquelinemauro/Dropbox/sorter/SLassigvecApconstrNewdatNmMu.csv")
write.csv(out.approx.nm.sl$muhat, "~jacquelinemauro/Dropbox/sorter/SLmuhatApconstrNewdatNmMu.csv")
write.csv(out.approx.nm.sl$pihat, "~jacquelinemauro/Dropbox/sorter/SLpihatApconstrNewdatNmMu.csv")
write.csv(c(out.approx.nm.sl$psi,out.approx.nm.sl$sd),"~jacquelinemauro/Dropbox/sorter/SLestsApconstrNewdatNmMu.csv")

# if you want the plug in
muhat.mat <- as.matrix(read.csv("~jacquelinemauro/Dropbox/sorter/SLmuhatApconstrNewdatNmMu.csv")[,-1])
assig.mu <- read.csv("~jacquelinemauro/Dropbox/sorter/SLassigvecApconstrNewdatNmMu.csv")[,-1]
Avals <- names(pris.dummies)
assig.mat <- sapply(Avals, function(x) as.numeric(Avals[assig.mu] == x))
muhat <- diag(muhat.mat %*% t(assig.mat))
plug.in.est <- mean(muhat)
write.csv(cbind(plug.in.est, sd(muhat)), "~jacquelinemauro/Dropbox/sorter/SLestsdApConstrPINewdatMu.csv")


##### output all nuisance parameters and unconstrained estimate using named vec, sl, 5 splits ####
dist.df <- data.frame(dist.mat)
out.combo.nm <- constr.opt.causal.nm(df, aLevel = dist.df, nsplits = 5, obsD = obsD,mu.algo = 'superlearner', pi.algo = 'superlearner')
write.csv(out.combo.nm$ifvals, "~jacquelinemauro/Dropbox/sorter/SLifvalsUnconstrNewdatNm5.csv")
write.csv(out.combo.nm$assig.vec, "~jacquelinemauro/Dropbox/sorter/SLassigvecUnconstrNewdatNm5.csv")
write.csv(out.combo.nm$phihat, "~jacquelinemauro/Dropbox/sorter/SLphihatUnconstrNewdatNm5.csv")
write.csv(out.combo.nm$muhat, "~jacquelinemauro/Dropbox/sorter/SLmuhatUnconstrNewdatNm5.csv")
write.csv(out.combo.nm$pihat, "~jacquelinemauro/Dropbox/sorter/SLpihatUnconstrNewdatNm5.csv")
write.csv(c(out.combo.nm$psi,out.combo.nm$sd),"~jacquelinemauro/Dropbox/sorter/SLestsUnconstrNewdatNm5.csv")

##### make a figure of the results #####
unconstrained <- read.csv("~jacquelinemauro/Dropbox/sorter/SLestsUnconstrNewdatNm.csv")[,-1]
constrained <- read.csv("~jacquelinemauro/Dropbox/sorter/SLestsdConstrIFNewdat.csv")[,-1]
approx <- read.csv("~jacquelinemauro/Dropbox/sorter/SLestsApconstrNewdatNm.csv")[,-1]
observed <- c(mean(df$y), NA)

plot.df <- data.frame(rbind(unconstrained,constrained,approx,observed))
plot.df$type <- c('Unconstrained', 'Constrained', 'Approx. Constrained','Observed')
plot.df$type <- factor(plot.df$type,
                       levels = c('Observed','Unconstrained', 'Constrained', 'Approx. Constrained'))

require(ggplot2)
g <- ggplot(plot.df, aes(x=type, y=est)) +
  geom_errorbar(aes(ymin = est - sd, ymax = est + sd), width=.1) +
  geom_line() +
  geom_point()+
  geom_hline(yintercept = plot.df$est[plot.df$type=='Observed'], col = 'red')+
  xlab("Procedure") + ylab("Estimate") + theme_bw()
ggsave(filename = '~jacquelinemauro/Dropbox/sorter/OutputScatter.png',plot = g,height = 5, width = 7)


##### June 23: output all using named vec, sl, A as covariate ####
# june 16 versions in dropbox may have fhat ordered wrong
dist.df <- data.frame(dist.mat)
out.combo.nmA <- constr.opt.causal.nm(df, aLevel = dist.df, obsD = obsD, mu.algo = 'superlearner', pi.algo = 'superlearner')
write.csv(out.combo.nmA$ifvals, "~jacquelinemauro/Dropbox/sorter/SLifvalsUnconstrNewdatNmA.csv")
write.csv(out.combo.nmA$assig.vec, "~jacquelinemauro/Dropbox/sorter/SLassigvecUnconstrNewdatNmA.csv")
write.csv(out.combo.nmA$assig.vec_c, "~jacquelinemauro/Dropbox/sorter/SLassigvecConstrNewdatNmA.csv")
write.csv(out.combo.nmA$muhat, "~jacquelinemauro/Dropbox/sorter/SLmuhatUnconstrNewdatNmA.csv")
write.csv(out.combo.nmA$pihat, "~jacquelinemauro/Dropbox/sorter/SLpihatUnconstrNewdatNmA.csv")
write.csv(c(out.combo.nmA$psi,out.combo.nmA$sd),"~jacquelinemauro/Dropbox/sorter/SLestsUnconstrNewdatNmA.csv")
write.csv(c(out.combo.nmA$psi_c,out.combo.nmA$sd_c),"~jacquelinemauro/Dropbox/sorter/SLestsConstrNewdatNmA.csv")

# plug ins
Avals <- names(pris.dummies)
assig.mat <- sapply(Avals, function(x) as.numeric(Avals[out.combo.nmA$assig.vec_c] == x))
muhat_c <- diag(out.combo.nmA$muhat %*% t(assig.mat))
muhat <- mean(apply(out.combo.nmA$muhat,1,min))
write.csv(cbind(mean(muhat_c), sd(muhat_c)), "~jacquelinemauro/Dropbox/sorter/SLestsdConstrPINewdatMuA.csv")
write.csv(cbind(mean(muhat), sd(apply(out.combo.nmA$muhat,1,min))), "~jacquelinemauro/Dropbox/sorter/SLestsUnconstrPINewdatNmA.csv")

#### June 23: calculate approximate constrained value ####
# june 16 versions in dropbox may have fhat ordered wrong
dist.df <- data.frame(dist.mat)
out.approx.nm.slA <- approx.constr.opt.causal.nm(df, aLevel = dist.df, obsD = obsD, nsplits = 2, mu.algo = 'superlearner', pi.algo = 'superlearner')
write.csv(out.approx.nm.slA$ifvals, "~jacquelinemauro/Dropbox/sorter/SLifvalsApconstrNewdatNmMuA.csv")
write.csv(out.approx.nm.slA$fhat, "~jacquelinemauro/Dropbox/sorter/SLassigvecApconstrNewdatNmMuA.csv")
write.csv(out.approx.nm.slA$muhat, "~jacquelinemauro/Dropbox/sorter/SLmuhatApconstrNewdatNmMuA.csv")
write.csv(out.approx.nm.slA$pihat, "~jacquelinemauro/Dropbox/sorter/SLpihatApconstrNewdatNmMuA.csv")
write.csv(c(out.approx.nm.slA$psi,out.approx.nm.slA$sd),"~jacquelinemauro/Dropbox/sorter/SLestsApconstrNewdatNmMuA.csv")

# plug in
muhat.mat <- as.matrix(read.csv("~jacquelinemauro/Dropbox/sorter/SLmuhatApconstrNewdatNmMuA.csv")[,-1])
assig.mu <- read.csv("~jacquelinemauro/Dropbox/sorter/SLassigvecApconstrNewdatNmMuA.csv")[,-1]
Avals <- names(pris.dummies)
assig.mat <- sapply(Avals, function(x) as.numeric(Avals[assig.mu] == x))
muhat <- diag(muhat.mat %*% t(assig.mat))
plug.in.est <- mean(muhat)
write.csv(cbind(plug.in.est, sd(muhat)), "~jacquelinemauro/Dropbox/sorter/SLestsdApConstrPINewdatMuA.csv")

#### June 23: calculate approximate constrained value using matching ####
dist.df <- data.frame(dist.mat)
out.approx.match.nm.slA <- approx.constr.opt.causal.nm(df, aLevel = dist.df, obsD = obsD, nsplits = 2, mu.algo = 'superlearner', pi.algo = 'superlearner', f.algo = 'match')
write.csv(out.approx.match.nm.slA$ifvals, "~jacquelinemauro/Dropbox/sorter/SLifvalsApconstrMatchNewdatNmMuA.csv")
write.csv(out.approx.match.nm.slA$fhat, "~jacquelinemauro/Dropbox/sorter/SLassigvecApconstrMatchNewdatNmMuA.csv")
write.csv(out.approx.match.nm.slA$muhat, "~jacquelinemauro/Dropbox/sorter/SLmuhatApconstrMatchNewdatNmMuA.csv")
write.csv(out.approx.match.nm.slA$pihat, "~jacquelinemauro/Dropbox/sorter/SLpihatApconstrMatchNewdatNmMuA.csv")
write.csv(c(out.approx.match.nm.slA$psi,out.approx.match.nm.slA$sd),"~jacquelinemauro/Dropbox/sorter/SLestsApconstrMatchNewdatNmMuA.csv")

# if you want the plug in
muhat.mat <- as.matrix(read.csv("~jacquelinemauro/Dropbox/sorter/SLmuhatApconstrMatchNewdatNmMuA.csv")[,-1])
assig.mu <- read.csv("~jacquelinemauro/Dropbox/sorter/SLassigvecApconstrMatchNewdatNmMuA.csv")[,-1]
Avals <- names(pris.dummies)
assig.mat <- sapply(Avals, function(x) as.numeric(Avals[assig.mu] == x))
muhat <- diag(muhat.mat %*% t(assig.mat))
plug.in.est <- mean(muhat)
write.csv(cbind(plug.in.est, sd(muhat)), "~jacquelinemauro/Dropbox/sorter/SLestsdApConstrMatchPINewdatMuA.csv")

##### make a figure of the results using A#####
unconstrained <- read.csv("~jacquelinemauro/Dropbox/sorter/SLestsUnconstrNewdatNmA.csv")[,-1]
constrained <- read.csv("~jacquelinemauro/Dropbox/sorter/SLestsdConstrIFNewdatNmA.csv")[,-1]
approx <- read.csv("~jacquelinemauro/Dropbox/sorter/SLestsApconstrNewdatNmMuA.csv")[,-1]
approx.match <- read.csv("~jacquelinemauro/Dropbox/sorter/SLestsApconstrMatchNewdatNmMuA.csv")[,-1]
observed <- c(mean(df$y), NA)

plot.df <- data.frame(rbind(unconstrained,constrained,approx,approx.match,observed))
plot.df$type <- c('Unconstrained', 'Constrained', 'Approx. Constr. Reg','Approx. Constr. Match', 'Observed')
plot.df$type <- factor(plot.df$type,
                       levels = c('Observed','Unconstrained', 'Constrained','Approx. Constr. Reg','Approx. Constr. Match'))

require(ggplot2)
g <- ggplot(plot.df, aes(x=type, y=est)) +
  geom_errorbar(aes(ymin = est - sd, ymax = est + sd), width=.1) +
  geom_line() +
  geom_point()+
  geom_hline(yintercept = plot.df$est[plot.df$type=='Observed'], col = 'red')+
  xlab("Procedure") + ylab("Estimate") + theme_bw()
ggsave(filename = '~jacquelinemauro/Dropbox/sorter/OutputScatterA.png',plot = g,height = 5, width = 7)

##### make a figure of the results using A, "checkfile_afactor" output #####
results <- read.csv("~jacquelinemauro/Dropbox/sorter/checkfileifresults.csv")[,-1]
pi.results <- read.csv("~jacquelinemauro/Dropbox/sorter/checkfilepiresults.csv")[,-1]
observed <- c(mean(df$y), NA)

plot.df <- data.frame(rbind(results,observed)); names(plot.df) <- c('est','sd' )
plot.df$type <- c('Unconstrained', 'Constrained', 'Approx. Constr. Reg','Approx. Constr. Match', 'Observed')
plot.df$type <- factor(plot.df$type,levels = c('Observed','Unconstrained', 'Constrained','Approx. Constr. Reg','Approx. Constr. Match'))

require(ggplot2)
g <- ggplot(plot.df, aes(x=type, y=est)) +
  geom_errorbar(aes(ymin = est - sd, ymax = est + sd), width=.1) +
  geom_line() +
  geom_point()+
  geom_hline(yintercept = plot.df$est[plot.df$type=='Observed'], col = 'red')+
  xlab("Procedure") + ylab("Estimate") + theme_bw()
ggsave(filename = '~jacquelinemauro/Dropbox/sorter/Figures/OutputScatterAfactor.png',plot = g,height = 5, width = 7)


