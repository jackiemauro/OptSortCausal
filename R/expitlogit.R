#' Expit Logit functions
#' 
#' simple helper functions
#' @param x the quantity you want to take expit or logit of

expit <- function(x){exp(x)/(1+exp(x))}
logit <- function(x){log(x/(1-x))}