#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
double prod_c(NumericVector x) {
    return std::accumulate(x.begin(), x.end(), 1.0, std::multiplies<double>());
}

// [[Rcpp::export]]
NumericMatrix secondmat(NumericMatrix firstorder, NumericVector prods, int popsize) {
    NumericMatrix secondorder(popsize, popsize);
    NumericVector fi, fj;
    double prod;

    for (int i = 0; i < popsize; i++) {
        fi = firstorder(_, i);
        for (int j = i+1; j < popsize; j++) {
            fj = firstorder(_, j);
            prod = 1 - prods[i] - prods[j] + prod_c(1 - fi - fj);
            secondorder(i, j) = prod;
            secondorder(j, i) = prod;
        }
    }
    return secondorder;
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

// # /*** R
// # #second(1:40)
// # */
