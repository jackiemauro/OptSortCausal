# trying to find better estimates for pi

rm(list = ls())
devtools::load_all()
nsim = 100
N = 5000
true.psi <- 0.25

df.list <- lapply(1:nsim, function(x) simsort3(N))
out.list <- lapply(df.list, function(x) unconstrained.min.lg(x))
f.list <- lapply(df.list, function(d) 1*(d$x1<1) + 2*(d$x1>=1 & d$x1 <2) + 3*(d$x1>=2))
fhat.list <- lapply(out.list, function(x) x$assig)

rmse <- sqrt(sum(unlist(lapply(out.list, function(x) (x$psi - true.psi)^2))))
bias <- mean(unlist(lapply(out.list, function(x) x$psi - true.psi)))
coverage <- mean(unlist(lapply(out.list, function(x) (x$psi - 1.96*x$sd <= true.psi) & (x$psi + 1.96*x$sd > true.psi)  )))
tables <- round(matrix(apply(mapply(function(x,y) prop.table(table(x,y),1), f.list, fhat.list),1,mean),ncol=3),3)

write.csv(tables, 'simulation3StudyPropTable.csv')