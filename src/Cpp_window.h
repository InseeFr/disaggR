#ifndef CPP_WINDOW_H_INCLUDED
#define CPP_WINDOW_H_INCLUDED

#include <Rcpp.h>
Rcpp::NumericVector Cpp_window_NumericVector(Rcpp::NumericVector const& x,double const& tseps,Rcpp::NumericVector const& start,Rcpp::NumericVector const& end);
Rcpp::NumericMatrix Cpp_window_NumericMatrix(Rcpp::NumericMatrix const& x,double const& tseps,Rcpp::NumericVector const& start,Rcpp::NumericVector const& end);
#endif // CPP_WINDOW_H_INCLUDED
