#include <Rcpp.h>
using namespace Rcpp;

// Some simple utilities for Rcpp


double rgammadouble(int a, double b, double c)
{   Rcpp::NumericVector x = rgamma(a,b,1/c);
  return x(0);
}

// Given a maximum integer n, simulates an integer from 1:n
int randint(int n) {
  if(n == 0) {
    return(0);
  }
  int ind;
  double rando;
  rando = runif(1)[0];
  ind = ceil(rando * n);
  return(ind);
}

//' Given a vector of different categories in 1...n and a prior,
//' simulate a Dirichlet random vector
//'
//' Takes a vector of collection indices to which individuals (vector elements) were assigned,
//' and returns a Dirichlet random variable generated by adding the prior to the sum
//' of each collection's occurrences, and simulating an alpha from a gamma distribution
//' with this shape parameter.
//'
//' The categories are labeled in C from 1 up to n.  n is the length of \code{lambda},
//' which is a vector of priors. Note that all elements of \code{lambda}
//' must be strictly greater than 0.
//'
//' @keywords internal
//'
//' @param C  a vector giving different categories of individual
//' (not counts of categories - untabulated)
//' @param lambda priors for the categories
//' @export
// [[Rcpp::export]]
NumericVector dirch_from_allocations(IntegerVector C, NumericVector lambda) {
  int i;
  int n = lambda.size();
  int N = C.size();
  NumericVector out = clone(lambda);

  for(i = 0; i < N; i++) {
    out[C[i] - 1] += 1.0;
  }
  for(i = 0; i < n; i++) {
    out[i] = rgammadouble(1L, out[i], 1.0);
  }

  return(out / sum(out));
}

//' Given a vector of counts for different categories in 1...n and a prior,
//' simulate a Dirichlet random vector
//'
//' Takes a vector of counts for 1:n collections,
//' and returns a Dirichlet random variable generated by adding the prior to each
//' collection value, and simulating an alpha from a gamma distribution
//' with this shape parameter.
//'
//' The categories are labeled in C from 1 up to n.  n is the length of \code{lambda},
//' which is a vector of priors. Note that all elements of \code{lambda}
//' must be strictly greater than 0.
//' @keywords internal
//' @param C  a vector giving counts of categories
//' @param lambda priors for the categories
//' @export
// [[Rcpp::export]]
NumericVector dirch_from_counts(IntegerVector C, NumericVector lambda) {
  int i;
  int n = lambda.size();
  NumericVector out = clone(lambda);

  for(i = 0; i < n; i++) {
    out[i] += C[i];
  }
  for(i = 0; i < n; i++) {
    out[i] = rgammadouble(1L, out[i], 1.0);
  }

  return(out / sum(out));
}
