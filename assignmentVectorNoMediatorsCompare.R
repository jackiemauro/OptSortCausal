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


##### mediators excluded outputs ####

library(xtable)
library(ICC)
library(plyr)
library(ggplot2)

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

# load outputs
fs <- read.csv('~jacquelinemauro/Dropbox/sorter/NoMediatorsSLassigvecs.csv')[,-1]
avals <- sort(unique(df$A))
fs <- data.frame(apply(fs,2, function(a) avals[a]))
fs$assig.vecU <- factor(fs$assig.vecU, levels = avals)
A = df$A
A = factor(df$A, levels = avals)
write.csv(data.frame(A,fs), '~jacquelinemauro/Dropbox/sorter/fs_for_app.csv') #old analysis is pre july 13

# results
results <- read.csv("~jacquelinemauro/Dropbox/sorter/NoMediatorsSLifresults.csv")[,-1]
results$Lower95 <- results[,1] - 1.96*results[,2]
results$Upper95 <- results[,1] + 1.96*results[,2]
rownames(results) <- procedures[-1]
colnames(results) <- c("Estimate", "SD", "Lower 95% CI", "Upper 95% CI")
print(xtable(round(results,4)), '~jacquelinemauro/Dropbox/sorter/results_NoMediatorsSL.tex',include.rownames = T, type = 'latex')

plot.df <- data.frame(type = c("Unconstrained", "Constrained","Regression","Matching"),
                      est = results[,1],
                      lower = results[,1] - results[,2],
                      upper = results[,1] + results[,2])
plot.df$type = factor(plot.df$type, levels = c("Unconstrained", "Constrained","Regression","Matching"))

res.plot <- ggplot(plot.df,aes(x = type, y = est)) + geom_point()+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1)+
  geom_hline(yintercept = mean(df$y), col = 'red')+
  #annotate("text", label = "Observed Rate", x = 1, y = mean(df$y), size = 3, colour = "black") +
  theme_bw()+
  labs(x = "Procedure", y = "Estimate and SE bars")
ggsave(plot = res.plot, filename = '~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/OutputScatterAfactor.png', width = 6, height = 4)

pi.results <- read.csv('~jacquelinemauro/Dropbox/sorter/NoMediatorsSLPI.csv')[,-1]
pi.plot.df <- data.frame(type = c("Unconstrained", "Constrained","Regression","Matching"),
                      est = pi.results[,1],
                      lower = pi.results[,1] - pi.results[,2],
                      upper = pi.results[,1] + pi.results[,2])
pi.plot.df$type = factor(pi.plot.df$type, levels = c("Unconstrained", "Constrained","Regression","Matching"))

pi.plot <- ggplot(pi.plot.df,aes(x = type, y = est)) + geom_point()+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1)+
  geom_hline(yintercept = mean(df$y), col = 'red')+
  #annotate("text", label = "Observed Rate", x = 1, y = mean(df$y), size = 3, colour = "black") +
  theme_bw()+
  labs(x = "Procedure", y = "Plug In Estimate and SE bars")
ggsave(plot = pi.plot, filename = '~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/OutputScatterAfactorPI.png', width = 6, height = 4)


# basic scatters of observed vs. optimized counts
count.obs <- c(table(df$A))
countU <- c(table(fs[,1]))
countC <- c(table(fs[,2]))
countAr <- c(table(fs[,3]))
countAm <- c(table(fs[,4]))

obsVu <- ggplot(data.frame(Observed = count.obs, Unconstrained = countU)) +
  geom_point(aes(x = Observed, y = Unconstrained))+
  theme_bw()
obsVc <- ggplot(data.frame(Observed = count.obs, Constrained = countC)) +
  geom_point(aes(x = Observed, y = Constrained))+
  theme_bw()
obsVAr <- ggplot(data.frame(Observed = count.obs, Regression = countAr)) +
  geom_point(aes(x = Observed, y = Regression))+
  theme_bw()
obsVAm <- ggplot(data.frame(Observed = count.obs, Matching = countAm)) +
  geom_point(aes(x = Observed, y = Matching))+
  theme_bw()
cVAm <- ggplot(data.frame(Constrained = countC, Matching = countAm)) +
  geom_point(aes(x = Constrained, y = Matching))+
  theme_bw()
cVAr <- ggplot(data.frame(Constrained = countC, Regression = countAr)) +
  geom_point(aes(x = Constrained, y = Regression))+
  theme_bw()

ggsave(obsVu, filename = '~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/obsVu.png', height = 4, width = 6)
ggsave(obsVc, filename = '~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/obsVc.png', height = 4, width = 6)
ggsave(obsVAr, filename = '~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/obsVAr.png', height = 4, width = 6)
ggsave(obsVAm, filename = '~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/obsVAm.png', height = 4, width = 6)
ggsave(cVAm, filename = '~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/cVAm.png', height = 4, width = 6)
ggsave(cVAr, filename = '~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/cVAr.png', height = 4, width = 6)

#### assignment differences ####
diffs <- matrix(rep(NA,dim(fs)[2]*length(avals)), nrow = length(avals))
for(i in 1:dim(fs)[2]){diffs[,i] <- table(fs[,i])-table(df$A)}

prop.diffs <- diffs
for(i in 1:5){prop.diffs[,i] <- (table(fs[,i])-table(df$A))/table(df$A)}


quintsU = quantile(diffs[,1],probs = seq(from = 1/5, to = 1,length.out = 5))
top5U = data.frame(A = avals[which(diffs[,1]>=quintsU[4])], change = diffs[which(diffs[,1]>=quintsU[4]),1])
bot5U = data.frame(A = avals[which(diffs[,1]<=quintsU[1])], change = diffs[which(diffs[,1]<=quintsU[1]),1])
print(xtable(top5U),'~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/top5U.tex',include.rownames = F, type = 'latex')
print(xtable(bot5U),'~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/bot5U.tex',include.rownames = F, type = 'latex')

# these accidentall got replaced, to get back the old ones resore EDA file outputs before july 13
quintsC = quantile(diffs[,2],probs = seq(from = 1/5, to = 1,length.out = 5))
top5C = data.frame(A = avals[which(diffs[,2]>=quintsC[4])], change = diffs[which(diffs[,2]>=quintsC[4]),2])
bot5C = data.frame(A = avals[which(diffs[,2]<=quintsC[1])], change = diffs[which(diffs[,2]<=quintsC[1]),2])
print(xtable(top5C),'~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/top5C.tex',include.rownames = F, type = 'latex')
print(xtable(bot5C),'~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/bot5C.tex',include.rownames = F, type = 'latex')

quintsAr = quantile(diffs[,3],probs = seq(from = 1/5, to = 1,length.out = 5))
top5Ar = data.frame(A = avals[which(diffs[,3]>=quintsAr[4])], change = diffs[which(diffs[,3]>=quintsAr[4]),3])
bot5Ar = data.frame(A = avals[which(diffs[,3]<=quintsAr[1])], change = diffs[which(diffs[,3]<=quintsAr[1]),3])
print(xtable(top5Ar),'~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/top5Ar.tex',include.rownames = F, type = 'latex')
print(xtable(bot5Ar),'~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/bot5Ar.tex',include.rownames = F, type = 'latex')

quintsAm = quantile(diffs[,4],probs = seq(from = 1/5, to = 1,length.out = 5))
top5Am = data.frame(A = avals[which(diffs[,4]>=quintsAm[4])], change = diffs[which(diffs[,4]>=quintsAm[4]),4])
bot5Am = data.frame(A = avals[which(diffs[,4]<=quintsAm[1])], change = diffs[which(diffs[,4]<=quintsAm[1]),4])
print(xtable(top5Am),'~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/top5Am.tex',include.rownames = F, type = 'latex')
print(xtable(bot5Am),'~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/bot5Am.tex',include.rownames = F, type = 'latex')

top5C = top5C[-c(6),] #roc also gains 14
top5Am = top5Am[-c(6),] #roc also gains 14
top.changes = cbind(top5U,top5C,top5Ar,top5Am)
print(xtable(top.changes),'~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/top5s.tex',include.rownames = F, type = 'latex')
bot5C = bot5C[-c(6,7),] #png/wam also lose 5
bot5Am = bot5Am[-c(6,7),] #png/wam also lose 5
bot.changes = cbind(bot5U,bot5C,bot5Ar,bot5Am)
print(xtable(bot.changes),'~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/bot5s.tex',include.rownames = F, type = 'latex')

top5.prop <- bot5.prop <- c(1:5)
for(i in 1:4){
  quints <- quantile(prop.diffs[,i], probs = seq(from = 1/5, to = 1,length.out = 5))
  temp.top = (data.frame(A = avals[which(prop.diffs[,i]>=quints[4])], change = round(prop.diffs[which(prop.diffs[,i]>=quints[4]),i],2)))[1:5,]
  temp.bot = (data.frame(A = avals[which(prop.diffs[,i]<=quints[1])], change = round(prop.diffs[which(prop.diffs[,i]<=quints[1]),i],2)))[1:5,]
  top5.prop <- cbind(top5.prop, temp.top)
  bot5.prop <- cbind(bot5.prop, temp.bot)
}
write.csv(top5.prop[,-1], '~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/top5_props.csv')
write.csv(bot5.prop[,-1], '~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/bot5_props.csv')

print(xtable((top5.prop[,-1])),'~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/top5Am.tex',include.rownames = F, type = 'latex')
print(xtable((bot5.prop[,-1])),'~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/bot5Am.tex',include.rownames = F, type = 'latex')


##### recidivism ####
# is recidivism risk getting concentrated
muhats <- read.csv('~jacquelinemauro/Dropbox/sorter/NoMediatorsSLmuhat.csv')[,-1]
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



require(plyr)
recids.round2 <- data.frame(recids.round, df$A, fs)


recid.concentration <- data.frame(Original = ddply(recids.round2, .(df.A), summarize, mean = mean(original)),
                                  #Unconstrained = ddply(recids.round2, .(assig.vecU), summarize, mean = mean(unconstrained)),
                                  Constrained = ddply(recids.round2, .(assig.vecC), summarize, mean = mean(constrained))[,2],
                                  Regression = ddply(recids.round2, .(assig.vecAr), summarize, mean = mean(approxR))[,2],
                                  Matching = ddply(recids.round2, .(assig.vecAm), summarize, mean = mean(approxM))[,2]
                                  )
Unconstrained.concentration = ddply(recids.round2, .(assig.vecU), summarize, Unconstrained = mean(unconstrained))
print(xtable(merge(recid.concentration, Unconstrained.concentration,
                   by.x = names(recid.concentration)[1], by.y = "assig.vecU", all = TRUE)
             ),include.rownames = FALSE)

png("~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/RecidvsChange.png")
par(mfrow = c(2,2))
for(i in 2:5){
  plot(tapply(X = recids$original, INDEX = df$A, FUN = mean),diffs[,(i-1)],
       xlab = 'Observed Recidivism', ylab = "Change in number of inmates", main = procedures[i],
       pch = 19)
}
par(mfrow = c(1,1))
dev.off()

png("~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/PredRecidvsChange.png")
par(mfrow = c(2,2))
for(i in 2:5){
  plot(tapply(X = recids[,i], INDEX = df$A, FUN = mean),diffs[,(i-1)],
       xlab = 'Predicted Recidivism', ylab = "Change in number of inmates", main = procedures[i],
       pch = 19)
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

print(xtable(sim.table),'~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/SimilarityTable.tex',include.rownames = T, type = 'latex')

recid.icc <- recid.round.icc <- c()
for(i in 1:5){recid.icc[i] = ICCbare(df$A, recids[,i])}
for(i in 1:5){recid.round.icc[i] = ICCbare(df$A, recids.round[,i])}
allrecid.icc = 100*cbind(recid.icc,recid.round.icc)
rownames(allrecid.icc) = procedures

print(xtable(allrecid.icc),'~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/recidICCs.tex',include.rownames = T, type = 'latex')

##### distance ####
distO = obsD
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
print(xtable(dist.harm), '~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/DistanceHarm.tex',include.rownames = T, type = 'latex')

png("~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/DistanceHistograms.png")
par(mfrow = c(3,2))
for(i in 1:5){hist(dists[,i], main = procedures[i], xlab = "Distance in minutes")}
par(mfrow = c(1,1))
dev.off()
print(xtable(apply(dists,2,summary)), '~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/DistanceSummaries.tex',include.rownames = T, type = 'latex')

apply(dists[,-1],2, function(a) mean(a - dists$Original) )
among.harmed <- rep(NA,4)
for(i in 1:4){
  among.harmed[i] = mean((dists[,(i+1)] - dists$Original)[dists[,(i+1)]>dists$Original])
}

highdistU <- which(distU > 250)
length(highdistU)
mean(df$y[highdistU])
mean(obsD[highdistU]<distU[highdistU])

apply(dists,2,function(x) mean(x<100))
apply(dists,2,function(x) round(mean(x - dists$Original),2))


#### visitation ####
vis.dat <- data.frame(df,visits,distance = obsD)
visit.model <- ranger::ranger(visits ~ ., data = vis.dat, write.forest = T)
dfU <- vis.dat; dfU$A <- fs$assig.vecU; dfU$distance <- distU
dfC <- vis.dat; dfC$A <- fs$assig.vecC; dfU$distance <- distC
dfAr <- vis.dat; dfAr$A <- fs$assig.vecAr; dfU$distance <- distAr
dfAm <- vis.dat; dfAm$A <- fs$assig.vecAm; dfU$distance <- distAm
pred.visit.unc <- predict(visit.model, data = dfU, type = 'response')$pre
pred.visit.con <- predict(visit.model, data = dfC, type = 'response')$pre
pred.visit.appR <- predict(visit.model, data = dfAr, type = 'response')$pre
pred.visit.appM <- predict(visit.model, data = dfAm, type = 'response')$pre
visits = data.frame(visits,pred.visit.unc,pred.visit.con,pred.visit.appR,pred.visit.appM)
names(visits) <- procedures
visits.round <- round(visits)


# scatter plots, observed. vs predicted
cutoff = quantile(visits.round$Original, probs = .99)
vis.plot.df <- data.frame(Observed = rep(visits.round$Original,4),
                          Assigned = c(visits.round$Unconstrained,
                                       visits.round$Constrained,
                                       visits.round$Regression,
                                       visits.round$Matching),
                          type = rep(procedures[-1], each = dim(visits.round)[1]))
vis.plot.df$type <- factor(vis.plot.df$type, levels = c("Unconstrained", "Constrained", "Regression", "Matching"))
library(ggplot2)
vp <- ggplot(vis.plot.df) +
  geom_point(aes(x = Observed, y = Assigned), alpha = .05)+
  xlim(0,cutoff) + ylim(0,cutoff)+
  facet_wrap(~type)+
  geom_abline(slope = 1, intercept = 0)+
  theme_bw()
ggsave( vp, filename ="~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/VisitsScatter.png",
        height = 4, width = 6)

# visits histograms
png("~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/VisitsHistograms.png")
par(mfrow = c(3,2))
for(i in 1:5){hist(visits[,i], main = procedures[i], xlab = "Visits at Last Location")}
par(mfrow = c(1,1))
dev.off()
png("~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/LogVisitsHistograms.png")
par(mfrow = c(3,2))
for(i in 1:5){hist(log(visits[,i]), main = procedures[i], xlab = "Visits at Last Location")}
par(mfrow = c(1,1))
dev.off()
png("~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/LogRoundVisitsHistograms.png")
par(mfrow = c(3,2))
for(i in 1:5){hist(log(visits.round[,i]), main = procedures[i], xlab = "Visits at Last Location")}
par(mfrow = c(1,1))
dev.off()
print(xtable(apply(visits,2,summary)), '~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/VisitsSummaries.tex',include.rownames = T, type = 'latex')
print(xtable(apply(visits.round,2,summary)), '~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/RoundVisitsSummaries.tex',include.rownames = T, type = 'latex')
print(xtable(data.frame(apply(visits.round,2,function(a) mean(a<1)))),'~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/NoVisits.tex',include.rownames = T, type = 'latex')

no.visits <- c(apply(visits.round,2,function(a) mean(a==0)))

# why is hou different?
hou.sum = data.frame(Houtzdale = apply(df[df$A=='hou',-c(2,16)],2,mean), All = apply(df[,-c(2,16)],2,mean))
rownames(hou.sum) <- nms[-c(2,16)]
print(xtable(hou.sum),'~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/HoutzdaleStats.tex',include.rownames = T, type = 'latex')

# baseline covariates
big.changes <- list(top5U,bot5U,top5C,bot5C,top5Ar,bot5Ar,top5Am,bot5Am)
baseline.covs <- data.frame(matrix(rep(NA,(dim(df)[2]-2)*8),ncol = (dim(df)[2]-2)))
for(i in 1:length(big.changes)){
  temp.df <- df[which(df$A %in% big.changes[[i]]$A),-c(2,16)]
  baseline.covs[i,] <- apply(temp.df,2,mean)
}
baseline.covs <- rbind(apply(df[,-c(2,16)],2,mean), baseline.covs)
names(baseline.covs) <- nms[-c(2,16)]
rownames(baseline.covs) <- c('Overall Average',
                             'Top 5 Unconstrained', 'Bottom 5 Unconstrained',
                             'Top 5 Constrained', 'Bottom 5 Constrained',
                             'Top 5 Regression-based', 'Bottom 5 Regression-based',
                             'Top 5 Matching-based', 'Bottom 5 Matching-based'
                             )

cov.name <- rep(nms[-c(2,16)],8)
est.type <- rep(rep(c("1","2","3","4"),each = (dim(df)[2]-2)),2)
pos.neg <- c(rep("Positive",(dim(df)[2]-2)*4),rep("Negative",(dim(df)[2]-2)*4))
orig.values <- apply(df[,-c(2,16)],2,mean)
pos.vals <- neg.vals <- NA
for(i in 1:4){pos.vals <- c(pos.vals,apply(df[which(df$A %in% sort(unique(df$A))[which(diffs[,i]>0)]),-c(2,16)],2,mean)-orig.values)}
for(i in 1:4){neg.vals <- c(neg.vals,apply(df[which(df$A %in% sort(unique(df$A))[which(diffs[,i]<=0)]),-c(2,16)],2,mean)-orig.values)}
pos.vals <- pos.vals[-1]; neg.vals <- neg.vals[-1]

baseline.covs <- data.frame(Value = c(pos.vals,neg.vals),Covariate = cov.name, Procedure = est.type, Change = pos.neg)

library(ggplot2)
library(RColorBrewer)
g <- ggplot(baseline.covs, aes(Procedure, Value, fill = Change)) +
  geom_col()+
  facet_wrap(~Covariate, scales = "free")+
  theme_bw()+
  theme(legend.position = 'bottom', axis.title.y=element_blank(),
        text = element_text(size=8))+
  scale_fill_brewer(palette = 'Paired')

ggsave(g, filename = '~jacquelinemauro/Dropbox/sorter/NoMediatorsOutputs/baselineCovariates.png', height = 6, width = 6)

#### security level ####

