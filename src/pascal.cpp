#include <Rcpp.h>
using namespace Rcpp;

//' @keywords internal
// [[Rcpp::export]]
Rcpp::NumericMatrix pascal(Rcpp::NumericMatrix m, int popsize, int set) {
    for (int i = 1; i < popsize; i++) {
        for (int j = 1; j < set; j++) {
            m(i, j) = m(i-1, j-1) + m(i-1, j);
        }
    }
  return m;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//
//
// /*** R
// pascal(matrix(1, 2, 2), 2, 2)
// */
