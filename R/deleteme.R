get.psiU.if <- function(D){
  # don't fluctuate either pi or mu
  dat = D$data; mumat = D$true.mumat; pimat = D$true.pimat; f.vec = D$a.star
  Avals = sort(unique(dat$a))
  temp = sapply(Avals, function(x) as.numeric(Avals[f.vec] == x))
  mu.hat = diag(mumat %*% t(temp))
  pi.hat = diag(pimat %*% t(temp))
  ifs = (as.numeric(dat$a == f.vec)*pi.hat) * (dat$y - mu.hat) + mu.hat
  return(list(ifs = ifs, est = mean(ifs), sd = sd(ifs)/sqrt(length(f.vec))))
}
