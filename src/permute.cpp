#include <Rcpp.h>
using namespace Rcpp;

// wrapper around R's RNG such that we get a uniform distribution over [0,n)
inline int randWrapper(const int n) { return floor(unif_rand()*n); }

// [[Rcpp::export]]
Rcpp::NumericVector permute_cpp(Rcpp::NumericVector a) {

  // clone a into b to leave a alone
  Rcpp::NumericVector b = Rcpp::clone(a);
  std::random_shuffle(b.begin(), b.end(), randWrapper);
  return b;
}