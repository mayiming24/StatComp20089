#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;
//' @title standard Laplace distribution
//' @description Implement a random walk Metropolis sampler for generating the standard Laplace distribution
//' @param a variance
//' @param xx the initial value
//' @param  N length of the chain
//' @return a random walk of the standard Laplace distribution
//' @examples
//' \dontrun{
//' cla(1,0,2000)
//' }
//' @export
//[[Rcpp::export]]
NumericVector cla(double a, double xx, int N) {
	NumericVector x(N);
	x[0] = xx;
	NumericVector u = runif(N);
	for (int i = 1; i < N;i++) {
		NumericVector y = rnorm(1, x[i - 1], a);
		if (u[i] <= (exp(-abs(y[0])) / exp(-abs(x[i-1])))) {
			x[i] = y[0];
		}
		else {
			x[i] = x[i - 1];
		}
	}
	return(x);
}
