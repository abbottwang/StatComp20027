## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
knitr::opts_chunk$set(comment = "#", warning = FALSE, eval = TRUE, message = FALSE)
set.seed(1)
library(StatComp20027)

## ---- eval= FALSE, out.width='25%', fig.align='center', fig.cap='Donald Trump'----
#  knitr::include_graphics('donald.jpg')

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(Titanic)

## ---- include=TRUE------------------------------------------------------------
# homework 3.3

rm(list = ls())
set.seed(1234)

n_sample <- 200

# generate n.sample random numbers subject to U(0, 1)
u <- runif(n_sample)

# generate Pareto distribution random numbers
pareto <- 2. * (u^(-0.5))

x <- seq(2, 10, .01)
y <- 8 / (x^3)

# draw a histogram of these numbers
hist(pareto, freq = F, col = 'red', breaks = 50, main = "Histogram of Pareto(2, 2) Distribution")
lines(x, y, col = "blue")


## -----------------------------------------------------------------------------
rm(list=ls())
set.seed(1234)

# homework 3.9

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

b <- generate(1000)
x <- seq(-1, 1, 0.01)
y <- 0.75 * (1 - x^2)

# make a histogram of @vector b
hist(b, main = "Sample Density Histogram, Funciton Curve of fe", freq = F, breaks = 50)
# line the density function
lines(x, y, col = "red")

# homework 3.10

# CDF of the chosen distribution
F <- function(x) {
    0.75 * x - 0.25 * (x^3) + 0.5
}

# use Kolmogorov-Smirnov tests to test whether b yileds F
ks.test(b, F)


## -----------------------------------------------------------------------------
# homework 3.13

rm(list=ls())
set.seed(1234)

# function that generates random numbers
generate <- function(n) {
    result <- rep(0, n)
    lambda <- rgamma(n=n, shape=4, rate=2)
    result <- rexp(n=n, rate=lambda)
    result
}

b <- generate(1000)
x <- seq(0.2, 6, 0.2)
y <- 64*(2+x)^(-5)

# draw histogram and line
hist(b, freq=F, breaks=50)
lines(x, y, col="red")

## -----------------------------------------------------------------------------
rm(list=ls())
set.seed(111)
# generate random numbers
u <- runif(1e5, 0, pi/3)

# use Monte-Carlo method to compute the integration
y <- sin(u)
value <- pi/3*mean(y); value

## -----------------------------------------------------------------------------
rm(list=ls())
set.seed(111)

# @Function used to compute integration
# @Param anti: 	if anti=TRUE use "Antithetic" method
# 				otherwise use "Monte-Carlo" method.
# @Param n: 	number of random variates we generate  
rate.phi <- function(n, anti=TRUE) {
	u <- runif(n/2)
	if(anti) u <- c(u, 1-u) else u <- c(u, runif(n/2))
	x <- exp(u)
	value1 <- mean(x)
	value1
}

m <- 1e3
MC1 <- rep(0., m) 
MC2 <- rep(0., m) 

for(i in 1:m) {
	MC1[i] <- rate.phi(1e3)
	MC2[i] <- rate.phi(1e3, anti=FALSE)
}

# empirical percent reduction in variance using antithetic variate
c(mean(MC1), mean(MC2), var(MC1)/var(MC2))

## -----------------------------------------------------------------------------
e <- 2.718281828
theo.rate <- (-3*e^2+10*e-5)/(-2*e^2+8*e-6); theo.rate


## -----------------------------------------------------------------------------
rm(list=ls())
set.seed(123)
g <- function(x) {
	1/sqrt(2*pi)*(x^2)*exp(-0.5*(x^2))*(x>1)
}

f1 <- function(x) {
	2/sqrt(2*pi)*exp(-0.5*(x^2)+x-0.5)*(x>1)
}

f2 <- function(x) {
	exp(-(x-1))*(x>1)
}

n <- 1e4
# generate samples from distribution f1
u <- abs(rnorm(n))+1
fg1 <-g(u)/f1(u)
# generate samples from distribution f2
v <- rexp(n,1)+1
fg2 <- g(v)/f2(v)

# estimation use importance function f1
mean(fg1)
# estimation use importance function f2
mean(fg2)
# the variance of the two estimations
var(fg1)
var(fg2)


## -----------------------------------------------------------------------------
rm(list=ls())
set.seed(7)
g <- function(x) {
	exp(-x - log(1+x^2)) * (x > 0) * (x < 1)
}

f <- function(x, k) {
	C <- (1-exp(-1))/(exp(-0.2*k+0.2)-exp(-0.2*k))
	C/(1-exp(-1))*exp(-x)*(x>(0.2*k-0.2))*(x<(0.2*k))
}

rf <- function(m, k) {
	C <- (1-exp(-1))/(exp(-0.2*k+0.2)-exp(-0.2*k))
	u <- runif(m)
	x <- -log(exp(-0.2*k+0.2)-u*(1-exp(-1))/C)
	x
}

m=2e4
theta <- numeric(5)
var.theta <- numeric(5)
for (k in 1:5){
	v <- rf(m, k)
	xx <- g(v)/f(v,k)
	theta[k] <- mean(xx)
	var.theta[k] <- var(xx)
}

# stratified importance sampling
sum(theta)
5*sum(var.theta)

# importance sampling
u <- runif(5*m) #inverse transform method
x <- - log(1 - u * (1 - exp(-1)))
fg <- g(x) / (exp(-x) / (1 - exp(-1)))

mean(fg)
var(fg)

## -----------------------------------------------------------------------------
rm(list=ls())
set.seed(123)
n <- 20
m <- 1e4
mu <- 1
sigma <- 1
alpha <- 0.05
u <- numeric(n)
theta.hat <-data.frame(L=numeric(m), U=numeric(m))

for (i in 1:m) {
	u <- rlnorm(n, mu, sigma)
	x <- log(u)
	len <- sd(x)*qt(1-alpha/2, n-1)
	theta.hat[i, 1:2] <- c(mean(x)-len,mean(x)+len)
}

# 95% confidence interval of parameter mu
# use Monte-Carlo method
apply(theta.hat,2,mean)


## -----------------------------------------------------------------------------
rm(list=ls())
set.seed(123)

n <- 100
m <- 1e4
alpha <-0.05

x <- rchisq(n, 2)
len <- sd(x)*qt(1-alpha/2, n-1)
# t-interval
L <- mean(x)-len; L
U <- mean(x)+len; U

p.hat <-numeric(m)

for (i in 1:m) {
	# generate samples from chi-square distribution with df=2
	u <- rchisq(n, 2)
	# frequency elements of u in [L, U]
	p.hat[i] <- sum((u>=L)*(u<=U))/n
}

# coverage probability of the t-interval 
mean(p.hat)

## -----------------------------------------------------------------------------
rm(list=ls())

sk <- function(x) {
    #computes the sample skewness coeff.
    xbar <- mean(x)
    m3 <- mean((x - xbar)^3)
    m2 <- mean((x - xbar)^2)
    return( m3 / m2^1.5 )
}

alpha <- .05
n <- 30
m <- 2500

pwr <- numeric(2)
#critical value for the skewness test
cv <- qnorm(1-alpha/2, 0, sqrt(6*(n-2) / ((n+1)*(n+3))))

sktests <- numeric(m)
for (i in 1:m) { #for each replicate
    x <- rbeta(n, 4, 4)
    sktests[i] <- as.integer(abs(sk(x)) >= cv)
}
# power of the skewness test of normality
# against beta(4, 4)
pwr[1] <- mean(sktests)

for (i in 1:m) { #for each replicate
    x <- rt(n, 8)
    sktests[i] <- as.integer(abs(sk(x)) >= cv)
}
# power of the skewness test of normality
# against t(8)
pwr[2] <- mean(sktests)
pwr


## -----------------------------------------------------------------------------
rm(list=ls())

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

m <- 1e3
# sample size for small, medium, large
n <- c(10, 50, 1e3)
# generate samples under H1 to estimate power
sigma1 <- 1
sigma2 <- 1.5
power <- matrix(0, ncol=2,nrow=3)

for (j in 1:3){
    power[j,1] <- mean(replicate(m, expr={
        x <- rnorm(n[j], 0, sigma1)
        y <- rnorm(n[j], 0, sigma2)
        count5test(x, y)
    }))

    power[j, 2] <- mean(replicate(m, expr={
        x <- rnorm(n[j], 0, sigma1)
        y <- rnorm(n[j], 0, sigma2)
        f.test(x, y)
    }))
}

power

## -----------------------------------------------------------------------------
rm(list=ls())
library(MASS)
# set.seed(124)
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

n <- c(10, 20, 30, 50, 100, 500) # sample sizes
alpha <- 0.05 #
p.reject <- numeric(length(n)) # to store sim. results
mm <- 1000 # num. repl. each sim.
for (i in 1:length(n)) {
    sktests <- numeric(mm) # test decisions
    for (j in 1:mm) {
        x <- as.matrix(mvrnorm(n[i], c(0,0),matrix(c(1,0.3,0.3,1.5),ncol=2)))
        # test decision is 1 (reject) or 0
        sktests[j] <- ((mardia.test(x)) >= (1-alpha) )
    }
    p.reject[i] <- mean(sktests) # proportion rejected
}
p.reject

## -----------------------------------------------------------------------------
rm(list=ls())
library(MASS)
mardia.test <- function(x,alpha=0.05) {
    m <- nrow(x)
    p <- ncol(x)
    # mle of covariance matrix
    var.hat <- var(x)*(m-1)/m
    inve <- solve(var.hat)
    ave <- apply(x, 2, mean)
    mat <- matrix(rep(ave, m), ncol=p,byrow = TRUE)
    x <- x-mat
    S <- x%*%inve%*%t(x)
    # compute mardia statistic
    M <- sum(S^3)/(m*6)
    # degree of freedom of asymptotic distribution
    degree <- p*(p+1)*(p+2)/6
    # p-value of the mardia statistic
    pchisq(M, degree)
}

alpha <- .1
n <- 100
mm <- 1000
epsilon <- c(seq(0.0, .15, .01), seq(.15, 0.95, .05))
N <- length(epsilon)
pwr <- numeric(N)
#critical value for the skewness test

for (j in 1:N) { #for each epsilon
    e <- epsilon[j]
    sktests <- numeric(mm)
    for (i in 1:mm) { #for each replicate
        sig <- sample(c(0, 1), replace = TRUE, size = n, prob = c(1-e, e))
        n1 <- sum(sig)
        n2 <- n - n1
        x1 <- mvrnorm(n, c(0,0),matrix(c(1,0.3,0.3,1.5),ncol=2))
        x2 <- mvrnorm(n, c(0,0), matrix(c(10,0.3,0.3,15),ncol=2))
        x <- rbind(x1[1:n1,],x2[1:n2,])
        sktests[i] <- ((mardia.test(x)) >= 1-alpha)
    }
    pwr[j] <- mean(sktests)
}
#plot power vs epsilon
plot(epsilon, pwr, type = "b",
     xlab = bquote(epsilon), ylim = c(0,1))
abline(h = .1, lty = 3)
se <- sqrt(pwr * (1-pwr) / mm) #add standard errors
lines(epsilon, pwr+se, lty = 3)
lines(epsilon, pwr-se, lty = 3)


## -----------------------------------------------------------------------------
rm(list=ls())
n <- 1e4

X <- sample(c(0, 1), size=n, replace = TRUE, prob=c(0.349, 0.651))
Y <- sample(c(0, 1), size=n, replace = TRUE, prob=c(0.324, 0.676))
minu <- (X-Y)
t.test(minu,conf.level=0.95)

## -----------------------------------------------------------------------------
library(bootstrap)
n <- nrow(law)
theta.jack <- numeric(n)

for (i in 1:n) {
    data <- law[-i,]
    theta.jack[i] <- cor(data[,1],data[,2])
}

theta.hat <- cor(law[,1],law[,2])

# estimate bias of theta.hat using jackknife method
bias.jack <- (n-1)*(mean(theta.jack)-theta.hat); round(bias.jack,4)
# estimate standard error of theta.hat using jackknife method
se.jack <- sqrt((n-1)^2*var(theta.jack)/n); round(se.jack,4)


## -----------------------------------------------------------------------------
library(boot)
set.seed(123)
data <- aircondit
alpha <- 0.05

N <- 1e3

est <- function(dat, ind) {
    1/mean(dat[ind,1])
}

boot.obj <- boot(data=data,statistic = est,R=N)
boot.ci(boot.obj,conf=1-alpha,type=c("basic", "norm", "perc", "bca"))

## -----------------------------------------------------------------------------
rm(list=ls())
library(bootstrap)
data <- scor

estim <- function (dat) {
  n <- nrow(dat)
  sig <- (n-1)*var(dat)/n
  vs <- eigen(sig)
  lambdas <- vs$values
  lambdas[1]/sum(lambdas)
}

n <- nrow(data)
theta.hat <- estim(data)
theta.jack <- numeric(n)
for (i in 1:n) {
  x <- data[-i,]
    theta.jack[i] <- estim(x)
}

# estimate the bias of theta.hat using jackknife method
bias.jack <- (n-1)*(mean(theta.jack)-theta.hat); round(bias.jack,4)
# estimate the standard error using jackknife method
se.jack <- sqrt((n-1)^2*var(theta.jack)/n); round(se.jack,4)

## -----------------------------------------------------------------------------
rm(list=ls())
library(DAAG); attach(ironslag)
a <- seq(10, 40, .1) #sequence for plotting fits
L1 <- lm(magnetic ~ chemical)
yhat1 <- L1$coef[1] + L1$coef[2] * a

L2 <- lm(magnetic ~ chemical + I(chemical^2))
yhat2 <- L2$coef[1] + L2$coef[2] * a + L2$coef[3] * a^2

L3 <- lm(log(magnetic) ~ chemical)
logyhat3 <- L3$coef[1] + L3$coef[2] * a
yhat3 <- exp(logyhat3)

L4 <- lm(log(magnetic) ~ log(chemical))
logyhat4 <- L4$coef[1] + L4$coef[2] * log(a)

n <- length(magnetic) #in DAAG ironslag

# divide the data into to N groups
# each group contains two data point
N <- round(n/2)
e1 <- e2 <- e3 <- e4 <- matrix(0, nrow=N, ncol=2)
num <- matrix(sample(1:n,2*N),ncol=2,nrow=N)

# for N-fold cross validation
# fit models on leave-two-out samples
for (k in 1:N) {
    y <- magnetic[-num[k,]]
    x <- chemical[-num[k,]]
    J1 <- lm(y ~ x)
    yhat1 <- J1$coef[1] + J1$coef[2] * chemical[num[k,]]
    e1[k,] <- magnetic[num[k]] - yhat1
    J2 <- lm(y ~ x + I(x^2))
    yhat2 <- J2$coef[1] + J2$coef[2] * chemical[num[k,]] +
        J2$coef[3] * chemical[num[k,]]^2
    e2[k,] <- magnetic[num[k,]] - yhat2
    J3 <- lm(log(y) ~ x)
    logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[num[k,]]
    yhat3 <- exp(logyhat3)
    e3[k,] <- magnetic[num[k,1]] - yhat3
    J4 <- lm(log(y) ~ log(x))
    logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[num[k,]])
    yhat4 <- exp(logyhat4)
    e4[k,] <- magnetic[num[k,]] - yhat4
}
lapply(list(e1,e2,e3,e4), function(x) {
  a <- apply(x^2,1,sum)
  mean(a)
})

detach(ironslag)

## -----------------------------------------------------------------------------
rm(list=ls())
set.seed(1234)
count5test <- function(x, y) {
    X <- x - mean(x)
    Y <- y - mean(y)
    outx <- sum(X > max(Y)) + sum(X < min(Y))
    outy <- sum(Y > max(X)) + sum(Y < min(X))
    # return 1 (reject) or 0 (do not reject H0)
    return(as.integer(max(c(outx, outy)) > 5))
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

n1 <- 50
n2 <- 20
N <- n1 + n2
mu1 <- mu2 <- 0
sigma1 <- 1
sigma2 <- 1
m <- 1e3

# use permutation
permutation.test <- replicate(m, expr = {
    x <- rnorm(n1, mu1, sigma1)
    y <- rnorm(n2, mu2, sigma2)
    x <- x - mean(x)
    y <- y - mean(y)
    z <- c(x, y)
    permutation(z)
} )

count5.test <- replicate(m, expr = {
    x <- rnorm(n1, mu1, sigma1)
    y <- rnorm(n2, mu2, sigma2)
    x <- x - mean(x)
    y <- y - mean(y)
    count5test(x,y)
} )


mean(permutation.test<0.05)
mean(count5.test)

## ---- eval=FALSE--------------------------------------------------------------
#  rm(list=ls())
#  library(energy); library(Ball); library(boot)
#  library(MASS); library(RANN)
#  set.seed(12345)
#  
#  Tn <- function(z, ix, sizes,k) {
#      n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
#      if(is.vector(z)) z <- data.frame(z,0);
#      z <- z[ix, ];
#      NN <- nn2(data=z, k=k+1) # what's the first column?
#      block1 <- NN$nn.idx[1:n1,-1]
#      block2 <- NN$nn.idx[(n1+1):n,-1]
#      i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5)
#      (i1 + i2) / (k * n)
#  }
#  
#  eqdist.nn <- function(z,sizes,k){
#      boot.obj <- boot(data=z,statistic=Tn,R=R,
#                       sim = "permutation", sizes = sizes,k=k)
#      ts <- c(boot.obj$t0,boot.obj$t)
#      p.value <- mean(ts>=ts[1])
#      list(statistic=ts[1],p.value=p.value)
#  }
#  
#  m <- 100; k<-3; p<-2; mu <- 0.5
#  R<-999;alpha <- 0.05;
#  n <- seq(10,100,10);
#  
#  
#  # Unequal variances and equal expectations
#  p.values <- matrix(NA,m,3)
#  pows <- matrix(NA, length(n), 3)
#  
#  for (k in 1:length(n)){
#      for(i in 1:m){
#          n1<-n2<- n[k]
#          N <- c(n1,n2)
#          x <- mvrnorm(n1, mu=c(0,0), Sigma=diag(c(3,2)))
#          y <- mvrnorm(n2, mu=c(0,0), Sigma=diag(c(1,1)))
#          z <- rbind(x,y)
#          p.values[i,1] <- eqdist.nn(z,N,k)$p.value
#          p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
#          p.values[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.value
#      }
#      pows[k,1:3] = colMeans(p.values<alpha)
#  }
#  
#  
#  plot(n, pows[1:10,1], type = "b", pch=20, col='red', lwd=1)
#  lines(n, pows[1:10,2], type='b',lty=2, col='blue')
#  lines(n, pows[1:10,3], type='b',lty=2,col='green')
#  
#  # Unequal variances and unequal expectations
#  pow2 <- matrix(NA, length(n), 3)
#  p.values2 <- matrix(NA, m, 3)
#  for (k in 1:length(n)){
#      for(i in 1:m){
#          n1<-n2<- n[k]
#          N <- c(n1,n2)
#          x <- mvrnorm(n1, mu=c(1,0.5), Sigma=diag(c(3,2)))
#          y <- mvrnorm(n2, mu=c(0,0), Sigma=diag(c(1,1)))
#          z <- rbind(x,y)
#          p.values2[i,1] <- eqdist.nn(z,N,k)$p.value
#          p.values2[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
#          p.values2[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.value
#      }
#      pow2[k,1:3] = colMeans(p.values2<alpha)
#  }
#  
#  plot(n, pow2[1:10,1], type = "b", pch=20, col='red', lwd=1)
#  lines(n, pow2[1:10,2], type='b',lty=2, col='blue')
#  lines(n, pow2[1:10,3], type='b',lty=2,col='green')
#  
#  
#  # t-distribution with 1 dim and bimodel distribution
#  pow3 <- matrix(NA, length(n), 3)
#  p.values3 <- matrix(NA, m, 3)
#  for (k in 1:length(n)){
#      for(i in 1:m){
#          n1<-n2<- n[k]
#          N <- c(n1,n2)
#          x <- matrix(rt(n1*2,1),ncol=2)
#          y1 <- mvrnorm(n2/2, mu=c(0,0), Sigma=diag(c(1,1)))
#          y2 <- mvrnorm(n2-n2/2, mu=c(1,0), Sigma=diag(c(1,2)))
#          y <- rbind(y1,y2)
#          z <- rbind(x,y)
#          p.values3[i,1] <- eqdist.nn(z,N,k)$p.value
#          p.values3[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
#          p.values3[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.value
#      }
#      pow3[k,1:3] = colMeans(p.values3<alpha)
#  }
#  
#  plot(n, pow3[1:10,1], type = "b", pch=20, col='red', lwd=1)
#  lines(n, pow3[1:10,2], type='b',lty=2, col='blue')
#  lines(n, pow3[1:10,3], type='b',lty=2,col='green')
#  
#  
#  # unbalanced samples
#  pow4 <- matrix(NA, length(n), 3)
#  p.values4 <- matrix(NA, m, 3)
#  for (k in 1:length(n)){
#      for(i in 1:m){
#          n1<- n[k]/2
#          n2<- n[k]
#          N <- c(n1,n2)
#          x <- mvrnorm(n1, mu=c(0,0), Sigma=diag(c(1,1)))
#          y <- mvrnorm(n2, mu=c(1,2), Sigma=diag(c(2,3)))
#          z <- rbind(x,y)
#          p.values4[i,1] <- eqdist.nn(z,N,k)$p.value
#          p.values4[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
#          p.values4[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.value
#      }
#      pow4[k,1:3] = colMeans(p.values4<alpha)
#  }
#  
#  plot(n, pow4[1:10,1],type = "b", pch=20, col='red', lwd=1)
#  lines(n, pow4[1:10,2], type='b',lty=2, col='blue')
#  lines(n, pow4[1:10,3], type='b',lty=2,col='green')

## -----------------------------------------------------------------------------
rm(list = ls())
set.seed(7)
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

N <- 2000
sigma <- c(.05, .5, 2,  16)

x0 <- 25
rw1 <- rw.Metropolis(sigma[1], x0, N)
rw2 <- rw.Metropolis(sigma[2], x0, N)
rw3 <- rw.Metropolis(sigma[3], x0, N)
rw4 <- rw.Metropolis(sigma[4], x0, N)

#number of candidate points rejected
no.reject <- data.frame(sigma=sigma,no.reject=c(rw1$k, rw2$k, rw3$k, rw4$k))
knitr::kable(no.reject,format='latex')

## ---- eval=FALSE--------------------------------------------------------------
#  par(mfrow=c(2,2))  #display 4 graphs together
#  refline <- qlaplace(c(.025, .975))
#  rw <- cbind(rw1$x, rw2$x, rw3$x,  rw4$x)
#  for (j in 1:4) {
#      plot(rw[,j], type="l",
#              xlab=bquote(sigma == .(round(sigma[j],3))),
#              ylab="X", ylim=range(rw[,j]))
#      abline(h=refline)
#  }
#  par(mfrow=c(1,1)) #reset to default

## -----------------------------------------------------------------------------
a <- c(.05, seq(.1, .9, .1), .95)
Q <- qlaplace(a)
rw <- cbind(rw1$x, rw2$x, rw3$x, rw4$x)
mc <- rw[501:N, ]
Qrw <- apply(mc, 2, function(x) quantile(x, a))
qq <- data.frame(round(cbind(Q, Qrw), 3))
names(qq) <- c('True','sigma=0.05','sigma=0.5','sigma=2','sigma=16')
knitr::kable(qq,format='latex',) #latex format


## -----------------------------------------------------------------------------
rm(list=ls())
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


dlaplace <- function(x) {
    #Laplace p.d.f.
      0.5*exp(-abs(x))
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

sigma <- 1     #parameter of proposal distribution
k <- 4          #number of chains to generate
n <- 15000      #length of chains
b <- 1000       #burn-in length

    #choose overdispersed initial values
x0 <- c(-10, -5, 5, 10)

#generate the chains
set.seed(12345)
X <- matrix(0, nrow=k, ncol=n)
for (i in 1:k)
    X[i, ] <- laplace.chain(sigma, n, x0[i])

#compute diagnostic statistics
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
    psi[i,] <- psi[i,] / (1:ncol(psi))

#plot psi for the four chains
#par(mfrow=c(2,2))
for (i in 1:k)
    if(i==1){
        plot((b+1):n,psi[i, (b+1):n],ylim=c(-0.2,0.2), type="l",
            xlab='Index', ylab=bquote(phi))
      }else{
        lines(psi[i, (b+1):n], col=i)
    }
par(mfrow=c(1,1)) #restore default


## -----------------------------------------------------------------------------
#plot the sequence of R-hat statistics
rhat <- rep(0, n)
for (j in (b+1):n)
    rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
rm(list=ls())

inte.fun <- function(a,k){
 ck <- sqrt(a^2*k/(k+1-a^2))
 part1 <- integrate(function(u){(1+u^2/k)^(-(k+1)/2)},0,ck)$value
 part2 <- 2/sqrt(pi*k)*exp(lgamma((k+1)/2)-lgamma(k/2))
 part1*part2
}

root.solve <- function(k){
  output = uniroot(function(a){inte.fun(a,k)-inte.fun(a,k-1)},lower=1,upper=2)
  output$root
}

k <- c(4,25,100,500,1000)
roots <- matrix(0,2,length(k))

for (i in 1:length(k)){
  roots[2,i] <- round(root.solve(k[i]),4)
}

roots[1,] <- k
rownames(roots) <- c('df','root')
roots


## ---- eval=FALSE--------------------------------------------------------------
#  formulas <- list(
#      mpg ~ disp,
#      mpg ~ I(1 / disp),
#      mpg ~ disp + wt,
#      mpg ~ I(1 / disp) + wt
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  trials <- replicate(
#      100,
#      t.test(rpois(10, 10), rpois(7, 10)),
#      simplify = FALSE
#  )

## -----------------------------------------------------------------------------
rm(list=ls())
library(nloptr)

mle.fun <- function(x,y,n.A=444,n.B=132,nOO=361,nAB=63) {
  r1 = 1-sum(y); r = 1-sum(x) 
  nBB = n.B*y[2]^2/(2*y[2]*r1+y[2]^2); nAA = n.A*y[1]^2/(y[1]^2+2*y[1]*r1)
  result <- -2*nBB*log(x[2])-2*nOO*log(r)-(n.A-nAA)*log(2*x[1]*r)-
      nAB*log(2*x[1]*x[2])-2*nAA*log(x[1])-(n.B-nBB)*log(2*x[2]*r)
  result
}

constrain <- function(x,y,n.A=444,n.B=132,nOO=361,nAB=63) {
  return(sum(x)-0.999999)
}

opts = list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel"=1.0e-8)
r = matrix(0,1,2); r = rbind(r,c(0.2,0.35))#
j = 2; mle = NULL
while (sum(abs(r[j,]-r[j-1,]))>1e-8) {
    res = nloptr( x0=c(0.3,0.25), eval_f=mle.fun,
               lb = c(0,0), ub = c(1,1), eval_g_ineq = constrain, 
               opts = opts, y=r[j,],n.A=444,n.B=132,nOO=361,nAB=63 )
    j = j+1
    r = rbind(r,res$solution)
    mle = c(mle,constrain(x=r[j,],y=r[j-1,]))
}
r 

## -----------------------------------------------------------------------------
rm(list=ls())
attach(mtcars)

formulas <- list(
        mpg ~ disp,
        mpg ~ I(1 / disp),
        mpg ~ disp + wt,
        mpg ~ I(1 / disp) + wt
    )

regressions <- vector("list", length(formulas))
for (i in length(formulas)) {
    regressions[[i]]<-lm(formulas[[i]], data=mtcars)
}
regressions

regs <- lapply(formulas, function(x) lm(x, data=mtcars))
detach(mtcars)
regs

## -----------------------------------------------------------------------------
rm(list=ls())
trials <- replicate(
    100, t.test(rpois(10, 10), rpois(7, 10)),
    simplify = FALSE)

a <- sapply(trials, function(x) x$p.value)
head(a)

## -----------------------------------------------------------------------------
rm(list=ls())
My_lapply <- function(X) {
    n <- nrow(X)
    result <- vector("list", n)
    for (i in 1:n) result[i] <- Map(sapply, as.vector(X[i,]), c(mean))
    result
}
xx <- matrix(rnorm(30), ncol=3)
a <-My_lapply(xx); a

## ---- eval=FALSE--------------------------------------------------------------
#  library(Rcpp); library(microbenchmark)
#  set.seed(7)
#  
#  d.laplace <- function(x) {
#      0.5*exp(-abs(x))
#  }
#  
#  rw.Metropolis <- function(sigma, x0, N) {
#      x <- numeric(N)
#      x[1] <- x0
#      u <- runif(N)
#      k <- 0
#      for (i in 2:N) {
#          y <- rnorm(1, x[i-1], sigma)
#          if (u[i] <= (d.laplace(y) / d.laplace(x[i-1])))
#              x[i] <- y
#          else {
#              x[i] <- x[i-1]
#              k <- k + 1
#          }
#      }
#      return(list(x=x, k=k))
#  }
#  
#  N <- 2000
#  sigma <- 4
#  x0 <- 25.0
#  rwR = rw.Metropolis(sigma,x0,N)$x[-(1:500)]
#  rwC = rw_Metropolis(sigma,x0,N)$x[-(1:500)]
#  qqplot(rwR,rwC)
#  abline(a=0,b=1,col='black')

## ---- eval=FALSE--------------------------------------------------------------
#  time.interval = microbenchmark(rwS=rw.Metropolis(sigma,x0,N),
#                                 rwT=rw_Metropolis(sigma,x0,N))
#  time.interval
#  

