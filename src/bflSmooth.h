#ifndef BFLSMOOTH_H_INCLUDED
#define BFLSMOOTH_H_INCLUDED

#include <Rcpp.h>
Rcpp::NumericVector Cpp_bflSmooth(Rcpp::NumericVector const& lfserie,unsigned int const& nfrequency);

#endif // BFLSMOOTH_H_INCLUDED
