# generate numbers yield the chosen distribution
generate <- function(n){
  result <- rep(0, n)
  u1 <- 2 * runif(n) - 1
  u2 <- 2 * runif(n) - 1
  u3 <- 2 * runif(n) - 1
  for(i in 1:n){
    if((abs(u3[i]) > abs(u2[i])) && (abs(u3[i]) > abs(u1[i]))){
      result[i] <- u2[i]
    }
    else {
      result[i] <- u3[i]
    }
  }
  result
}


rate.phi <- function(n, anti=TRUE) {
  u <- runif(n/2)
  if(anti) u <- c(u, 1-u) else u <- c(u, runif(n/2))
  x <- exp(u)
  value1 <- mean(x)
  value1
}


g <- function(x) {
  1/sqrt(2*pi)*(x^2)*exp(-0.5*(x^2))*(x>1)
}

f1 <- function(x) {
  2/sqrt(2*pi)*exp(-0.5*(x^2)+x-0.5)*(x>1)
}

f2 <- function(x) {
  exp(-(x-1))*(x>1)
}

count5test <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  # return 1 (reject) or 0 (do not reject H0)
  return(as.integer(max(c(outx, outy)) > 5))
}

f.test <- function(x, y, alpha=0.055) {
  N <- length(x)
  M <- length(y)
  F <- var(x)/var(y)
  (F < qf(alpha/2, N-1, M-1))||(F>qt(1-alpha/2, N-1, M-1))
}

mardia.test <- function(x,alpha=0.05) {
  m <- nrow(x)
  p <- ncol(x)
  var.hat <- var(x)*(m-1)/m
  inve <- solve(var.hat)
  ave <- apply(x, 2, mean)
  mat <- matrix(rep(ave, m), ncol=p,byrow = TRUE)
  x <- x-mat
  S <- x%*%inve%*%t(x)
  M <- sum(S^3)/(m*6)
  degree <- p*(p+1)*(p+2)/6
  pchisq(M, degree)
}

permutation <- function(z, R=1e3) {
  N = length(z)
  s = replicate(R,{
    idx = sample(1:N, N/2, replace = FALSE)
    x = z[idx]
    y = z[-idx]
    count5test(x,y)
  })
  mean(s)
}

dlaplace <- function(x) {
  0.5*exp(-abs(x))
}
qlaplace <- function(x) {
  y <- x
  for (i in 1:length(x)) {
    if(x[i]>=0.5) {
      y[i] <- qexp(2*x[i]-1, rate=1)
    }
    else {
      y[i] <- -qexp(1-2*x[i], rate=1)
    }
  }
  return(y)
}

rw.Metropolis <- function(sigma, x0, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= (dlaplace(y) / dlaplace(x[i-1])))
      x[i] <- y
    else {
      x[i] <- x[i-1]
      k <- k + 1
    }
  }
  return(list(x=x, k=k))
}

Gelman.Rubin <- function(psi) {
  # psi[i,j] is the statistic psi(X[i,1:j])
  # for chain in i-th row of X
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  
  psi.means <- rowMeans(psi)     #row means
  B <- n * var(psi.means)        #between variance est.
  psi.w <- apply(psi, 1, "var")  #within variances
  W <- mean(psi.w)               #within est.
  v.hat <- W*(n-1)/n + (B/n)     #upper variance est.
  r.hat <- v.hat / W             #G-R statistic
  return(r.hat)
}


laplace.chain <- function(sigma, N, X1) {
  #generates a Metropolis chain for Laplace
  #with Normal(X[t], sigma) proposal distribution
  #and starting value X1
  x <- rep(0, N)
  x[1] <- X1
  u <- runif(N)
  
  for (i in 2:N) {
    xt <- x[i-1]
    y <- rnorm(1, xt, sigma)     #candidate point
    r1 <- dlaplace(y) * dnorm(xt, y, sigma)
    r2 <- dlaplace(xt) * dnorm(y, xt, sigma)
    r <- r1 / r2
    if (u[i] <= r) x[i] <- y else
      x[i] <- xt
  }
  return(x)
}

