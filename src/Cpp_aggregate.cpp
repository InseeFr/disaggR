#include <Rcpp.h>
using namespace Rcpp;
NumericVector Cpp_aggregate_NumericVector(NumericVector const& serie,double const& ratio,double const& len) {
  NumericVector res(len);
  for (R_len_t i=0;i<len;i++) {
    double agreg=0;
    for (R_len_t j=ratio*i;j<ratio*(i+1);j++) agreg+=serie(j);
    res(i)=agreg;
  }
  if ((serie.hasAttribute("names"))) res.attr("names") = serie.attr("names");
  return res;
}

NumericMatrix Cpp_aggregate_NumericMatrix(NumericMatrix const& serie,double const& ratio,double const& len) {
  NumericMatrix res(len,serie.ncol());
  for (int i=0;i<len;i++) {
    NumericVector agreg(serie.ncol(),(double)0);
    for (int j=ratio*i;j<ratio*(i+1);j++) agreg+=serie(j,_);
    res(i,_)=agreg;
  }
  if ((serie.hasAttribute("dimnames"))) res.attr("dimnames") = serie.attr("dimnames");
  return res;
}