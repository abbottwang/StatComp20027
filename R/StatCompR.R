#' @title R functions.
#' @name functions
#' @description  functions used in my homework
#' @examples
#' \dontrun{
#' print("Hello, world !!!")
#' }
#' @importFrom stats dnorm pchisq qexp qf qt rnorm runif var
#' @useDynLib StatComp20027
NULL


#' @title Multivariate normal distribution random number
#' @description generate multivariate normal distribution random number
#' @param n the size of random number we need
#' @param mu the mean of the multivariate normal distribution
#' @param Sigma the variance of the multivariate normal distribution
#' @return the random numbers yield N(mu, Sigma)
#' @examples
#' \dontrun{
#' x <- r_mvnorm(100, mu=c(1, 2), Sigma=matrix(c(1,0,0,2), ncol=2))
#' mean.hat <- apply(x, 2, mean)
#' var.hat <- apply(x, 2, var)
#' }
#' @export
r_mvnorm <- function(n, mu, Sigma) {
  m = length(mu)
  mu.mat <- matrix(rep(mu, n), ncol = m, byrow = TRUE)
  decop <- svd(Sigma)
  D <- decop$d
  dd <- diag(sqrt(D))
  
  A <- decop$u %*% dd %*% t(decop$v)
  u <- matrix(rnorm(m*n), ncol = m)
  rn <- u %*% A + mu.mat
  rn
}


#' @title Multivariate normal distribution density function
#' @description multivariate normal distribution probability density function
#' @param x at which the density is wanted
#' @param mu the mean of the multivariate normal distribution
#' @param Sigma the variance of the multivariate normal distribution
#' @return the density at \code{x}
#' @examples
#' \dontrun{
#' d_mvnorm(c(0,0), c(0, 0), Sigma=matrix(c(1,0,0,1), ncol=2))
#' }
#' @export
d_mvnorm <- function(x, mu, Sigma) {
  n <- length(x)
  s <- -0.5 * ((x - mu) %*% solve(Sigma) %*% (x - mu))
  d.value <- 1/(sqrt(det(Sigma))*(pi^(n/2)))
  d.value*exp(s)
}



