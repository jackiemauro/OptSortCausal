rm(list = ls())
devtools::load_all()

#### simplest case
df <- simsort(5000, k = .75)
f <- 1*(df$x<1) + 2*(df$x>=1 & df$x <2) + 3*(df$x>=2)
out = unconstrained.min(df)
est.psi = out$psi
oracle.psi = .5# if I knew the right f and E(Y|A,X)
f.psi = mean(.5*as.numeric(out$assig == f) + .75*as.numeric(out$assig != f))# if I take this f as fixed, but I knew E(Y|A,X)
table(out$assig, f)

#### repeat with unequal A probs and p* = 0.25 vs p = 0.75
df <- simsort2(5000)
f <- 1*(df$x<1) + 2*(df$x>=1 & df$x <2) + 3*(df$x>=2)
out2 = unconstrained.min(df)
est.psi = out2$psi
oracle.psi = .25# if I knew the right f and E(Y|A,X)
f.psi = mean(.25*as.numeric(out2$assig == f) + .75*as.numeric(out2$assig != f))# if I take this f as fixed, but I knew E(Y|A,X)
table(out2$assig, f)

#### repeat w A that depends on X and 2 covariates
df <- simsort3(5000)
f <- 1*(df$x1<1) + 2*(df$x1>=1 & df$x1 <2) + 3*(df$x1>=2)
out3 = unconstrained.min(df)
est.psi = out3$psi
oracle.psi = .25# if I knew the right f and E(Y|A,X)
f.psi = mean(.25*as.numeric(out3$assig == f) + .75*as.numeric(out3$assig != f))# if I take this f as fixed, but I knew E(Y|A,X)
table(out3$assig, f)

#### repeat w logistic as predictor and A that depends on X and 2 covariates
df <- simsort3(5000)
f <- 1*(df$x1<1) + 2*(df$x1>=1 & df$x1 <2) + 3*(df$x1>=2)
out4 = unconstrained.min.lg(df)
est.psi = out4$psi
oracle.psi = .25# if I knew the right f and E(Y|A,X)
f.psi = mean(.25*as.numeric(out4$assig == f) + .75*as.numeric(out4$assig != f))# if I take this f as fixed, but I knew E(Y|A,X)
table(out4$assig, f)

#### repeat w ranger as predictor and A that depends on X and 2 covariates
df <- simsort3(5000)
f <- 1*(df$x1<1) + 2*(df$x1>=1 & df$x1 <2) + 3*(df$x1>=2)
out5 = unconstrained.min.rg(df)
est.psi = out5$psi
oracle.psi = .25# if I knew the right f and E(Y|A,X)
f.psi = mean(.25*as.numeric(out5$assig == f) + .75*as.numeric(out5$assig != f))# if I take this f as fixed, but I knew E(Y|A,X)
table(out5$assig, f)
