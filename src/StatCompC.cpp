#include <Rcpp.h>
using namespace Rcpp;

//' @title Laplace denstiy function
//' @description Laplace denstiy function using Rcpp
//' @param x variable(vector)
//' @return the density at \code{x}
//' @examples
//' \dontrun{
//' x <- seq(1, 3, length.out=10)
//' pdf <- d_lapalce(); pdf
//' }
//' @export
// [[Rcpp::export]]
NumericVector d_laplace(NumericVector x) {
  NumericVector y = 0.5*exp(-abs(x));
  return y;
}

// only used in this file
double d_laplace(double x) {
  double y = 0.5*exp(-abs(x));
  return y;
}

//' @title Metropolis method
//' @description using Metropolis method implementing MCMC
//' @param sigma variance
//' @param x0 initial values
//' @param N maximum number of iteration
//' @return a list of \code{x} and iterator \code{k}
//' @examples
//' \dontrun{
//' sigma = 6.
//' x0 = 36.
//' N = 2000
//' model <- rw_Metropolis(sigma, x0, N)$x
//' plot(model)
//' }
//' @export
// [[Rcpp::export]]
List rw_Metropolis(double sigma, double x0, int N) {
  NumericVector x(N);
  x[0] = x0;
  NumericVector u = runif(N);
  int k = 0;
  for (int i=1; i<N; i++) {
    double y = rnorm(1, x[i-1], sigma)[0];
    if (u[i] <= (d_laplace(y) / d_laplace(x[i-1])))
      x[i] = y;
    else {
      x[i] = x[i-1];
      k = k + 1;
    }
  }
  return(List::create(Named("x")=x,
                      Named("k")=k));
}




