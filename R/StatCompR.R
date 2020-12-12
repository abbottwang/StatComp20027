#' @title A illustration dataset
#' @name data
#' @description A dataset used to illustrate the performance of \code{vaccR} and \code{vaccC}.
#' @examples
#' \dontrun{
#' data(data)
#' attach(data)
#' tm <- microbenchmark::microbenchmark(
#'   vR = vaccR(age,female,ily),
#'   vC = vaccC(age,female,ily)
#' )
#' print(summary(tm)[,c(1,3,5,6)])
#' }
NULL

#' @title Benchmark R and Rcpp functions.
#' @name benchmarks
#' @description Use R package \code{microbenchmark} to compare the performance of C functions (\code{gibbsR} and \code{vaccR}) and Cpp functions (\code{gibbsC} and \code{vaccC}).
#' @examples
#' \dontrun{
#' data(data)
#' attach(data)
#' tm1 <- microbenchmark::microbenchmark(
#'   rnR = gibbsR(100,10),
#'   rnC = gibbsC(100,10)
#' )
#' print(summary(tm1)[,c(1,3,5,6)])
#' 
#' tm2 <- microbenchmark::microbenchmark(
#'   vR = vaccR(age,female,ily),
#'   vC = vaccC(age,female,ily)
#' )
#' print(summary(tm2)[,c(1,3,5,6)])
#' }
#' @import microbenchmark
#' @importFrom Rcpp evalCpp
#' @importFrom stats rnorm rgamma
#' @useDynLib StatComp20027
NULL

#' @title A dataset used for illustration.
#' @name data
#' @description This dataset is used to compare the performance of C function \code{vaccR}) and C++ function \code{vaccC}.
#' @examples
#' \dontrun{
#' data(data)
#' attach(data)
#' tm <- microbenchmark::microbenchmark(
#'   vR = vaccR(age,female,ily),
#'   vC = vaccC(age,female,ily)
#' )
#' print(summary(tm)[,c(1,3,5,6)])
#' }
NULL

#' @title knn use kernel density function
#' @description knn use kernel density function
#' @param x data 1
#' @param y data 2
#' @return distance between \code{x} and \code{y}
#' @examples
#' \dontrun{
#' x <- rnorm(12)
#' y <- rnorm(12, 1, 3)
#' Eucildean.dist(x, y)
#' }
#' @export
Euclidean.dist <- function(x, y) {
  sqrt(sum((x-y)^2))
}

#' @title A Gibbs sampler using R
#' @description A Gibbs sampler using R
#' @param x a vector
#' @return the L2 norm of \code{x}
#' @examples
#' \dontrun{
#' x <- rnorm(12)
#' d <- Euclidean.norm(x)
#' d
#' }
#' @export
Euclidean.norm <- function(x) {
  sqrt(sum(x^2))
}


