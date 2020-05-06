#ifndef CPP_AGGREGATE_H_INCLUDED
#define CPP_AGGREGATE_H_INCLUDED

#include <Rcpp.h>
Rcpp::NumericVector Cpp_aggregate_NumericVector(Rcpp::NumericVector const& serie,double const& ratio,double const& len);
Rcpp::NumericMatrix Cpp_aggregate_NumericMatrix(Rcpp::NumericMatrix const& serie,double const& ratio,double const& len);

#endif // CPP_AGGREGATE_H_INCLUDED
