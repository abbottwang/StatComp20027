## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
knitr::opts_chunk$set(comment = "#", warning = FALSE, eval = TRUE, message = FALSE)
set.seed(1)
library(StatComp20027)

## ---- eval=FALSE--------------------------------------------------------------
#  
#  # generate multivariate normal distribution
#  r_mvnorm <- function(n, mu, Sigma) {
#      m = length(mu)
#      mu.mat <- matrix(rep(mu, n), ncol=m, byrow=TRUE)
#      # svd decomposition
#      decop <- svd(Sigma)
#      D <- decop$d
#      dd <- diag(sqrt(D))
#  
#      # compute Sigma^(-1/2)
#      A <- decop$u %*% dd %*% t(decop$v)
#      u <- matrix(rnorm(m*n), ncol=m)
#      # mr normal distribution random number
#      rn <- u %*% A + mu.mat
#      rn
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  
#  dmvnorm <- function(x, mu, Sigma) {
#    n <- length(x)
#    s <- -0.5 * ((x - mu) %*% solve(Sigma) %*% (x - mu))
#    d.value <- 1/(sqrt(det(Sigma))*(pi^(n/2)))
#    d.value*exp(s)
#  }

