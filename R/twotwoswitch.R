# switching 2 by 2

# B = 10000
#
# N = 1000
# x = rnorm(N,0,1)                                # covariates
# s = sample(c(1,3), size = N, replace = T)       # security level
# p = sample(c(1:5), size = N, replace = T)       # prison assigned
# pi = exp(s + x - p/2) / (1 + exp(s + x - p/2))  # true prob recid at observed
# y = rbinom(N, 1, pi)                            # recidivate
#
# df <- data.frame(y,p,s,x)
# pot.pi = sapply(c(1:5), function(y) exp(s + x - y/2) / (1 + exp(s + x - y/2)))


switch22 <- function(df,B){
  library(ranger)
  k = 0
  out.df <- df
  N <- dim(df)[2]
  ch <- rep(0,B+1)
  new.mu <- df$y
  rate <- mu.hist <- rep(NA, B)
  pb <- txtProgressBar(min = 0, max = B, style = 3)
  for(j in 1:B){
    p1      <- sample(dim(df)[1], size = 1, replace = F)
    p2      <- sample(c(1:dim(df)[1])[-p1], size = 1, replace = F)
    pair    <- c(p1,p2)
    D       <- df[-pair,]
    pair.covs <- out.df[pair,3:dim(df)[2]]
    pair.a <- out.df[pair,2]
    if(pair.a[1]==pair.a[2]){
      ch[j+1] = 0
      rate[j] = mean(ch[(j+1 - k)]:ch[(j+1)])
    }
    else{
      newdata <- as.data.frame(cbind(rep(pair.a, each = 2), rbind(pair.covs,pair.covs)))
      names(newdata) <- names(df)[2:dim(df)[2]]

      mu.hat <- predict(glm(y~., data = D, family = binomial), newdata = newdata, type = 'response')
      orig    <- mu.hat[1]+mu.hat[4]
      new     <- mu.hat[2]+mu.hat[3]
      if(orig <= new){
        P = out.df$a[pair]
        new.mu[pair] = mu.hat[c(1,4)]
        ch[j+1] = 0
      } else{
        P = c(out.df$a[p2],out.df$a[p1])
        new.mu[pair] = mu.hat[2:3]
        ch[j+1] = 1
      }
      out.df$a[pair] <- P
      setTxtProgressBar(pb, j)
      k = min(j,10)
      rate[j] = mean(ch[(j+1 - k)]:ch[(j+1)])
      mu.hist[j] <- mean(new.mu)
    }
    }
    return(list(out.df = out.df, new.mu = new.mu, changes = ch))
    close(pb)
}

