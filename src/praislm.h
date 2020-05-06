#ifndef PRAISLM_H_INCLUDED
#define PRAISLM_H_INCLUDED

#include <Rcpp.h>
Rcpp::List praislm(Rcpp::NumericMatrix& X,Rcpp::NumericVector& y,bool const& includerho,bool const& includedifferenciation,Rcpp::NumericVector const& set_coefficients,SEXP const& cl);

#endif // PRAISLM_H_INCLUDED
