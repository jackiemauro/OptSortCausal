# Increase "fudge" factor

# for fudge in fudge.range
#   output new constraint
#   optimize in matlab
#   calculate plug in and if

fudge.range <- c(1.05, 1.25, 1.5, 2, 4)
muhat.mat <- as.matrix(read.csv("~jacquelinemauro/Dropbox/sorter/SLmuhatUnconstrNewdatNmA.csv")[,-1])
pihat.mat <- as.matrix(read.csv("~jacquelinemauro/Dropbox/sorter/SLpihatUnconstrNewdatNmA.csv")[,-1])

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
  geom_errorbar(aes(ymin = Estimate - SD, ymax = Estimate + SD), width=.1) +
  geom_point() +
  geom_hline(yintercept = unconstrained[1], col = 'red')+
  xlab("Loosening of constraint") + ylab("Estimate") + theme_bw()
ggsave(filename = '~jacquelinemauro/Dropbox/sorter/FudgeChange.png',plot = g,height = 5, width = 7)
