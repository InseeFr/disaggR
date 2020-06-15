#include <Rcpp.h>
#include "bflSmooth.h"
#include "praislm.h"
#include "Cpp_window.h"
#include "Cpp_aggregate.h"

using namespace Rcpp;

List Cpp_twoStepsBenchmark_impl(NumericMatrix const& hfserie,NumericVector const& lfserie,
                                    bool const& includedifferenciation,bool const& includerho,
                                    NumericVector set_coefficients,
                                    NumericVector const& startcoeffcalc,NumericVector const& endcoeffcalc,
                                    double const& tseps,SEXP const& cl) {
  if (! Rf_inherits(lfserie,"ts") || ! Rf_inherits(hfserie,"ts")) stop("Not a ts object");
  if (lfserie.hasAttribute("dim")) if (((NumericVector)lfserie.attr("dim"))[1] != 1) stop("The low frequency serie must be one-dimensional");

  NumericVector const& tsphf=hfserie.attr("tsp");
  NumericVector const& tsplf=lfserie.attr("tsp");
  int const ratio = (int)tsphf[2]/(int)tsplf[2];
  
  // Estimation of the reg coefficients

  NumericVector y = Cpp_window_NumericVector(lfserie,tseps,startcoeffcalc,endcoeffcalc);
  NumericVector const& tspy=y.attr("tsp");
  NumericMatrix hfserie_wcalc = Cpp_window_NumericMatrix(hfserie,tseps,NumericVector::create(tspy[0]),NumericVector::create(tspy[1]+1/tspy[2]-1/tsphf[2]));
  NumericMatrix x=Cpp_aggregate_NumericMatrix(hfserie_wcalc,ratio,y.size());
  
  List regresults = praislm(x,y,includerho,includedifferenciation,set_coefficients,cl);
  
  NumericVector const& coefficients=regresults["coefficients"];
  
  // Application of the reg coefficients
  NumericVector const startdomain_extended=NumericVector::create(floor(tsphf[0]*tsplf[2])/tsplf[2]);
  NumericVector const enddomain_extended=NumericVector::create(ceil((tsphf[1]+1/tsphf[2])*tsplf[2])/tsplf[2]-1/tsphf[2]);
                // This window is the smallest that is all around the domain of the hfserie.

  NumericVector lfserie_wbench=Cpp_window_NumericVector(lfserie,tseps,startdomain_extended,enddomain_extended);
  NumericMatrix hfserie_wbench=Cpp_window_NumericMatrix(hfserie,tseps,startdomain_extended,enddomain_extended);
  
  R_len_t const& A=lfserie_wbench.size();
  double const& rho=regresults["rho"];

  NumericVector hfserie_fitted(hfserie_wbench.nrow(),(double)0);
  hfserie_fitted.attr("class")=StringVector::create("ts");
  hfserie_fitted.attr("tsp")=hfserie_wbench.attr("tsp");
  
  for (R_len_t i=0;i<coefficients.size();i++) hfserie_fitted = hfserie_fitted + coefficients[i] * hfserie_wbench(_,i);  

  NumericVector hfserie_fitted_aggreg(A);
  hfserie_fitted_aggreg=Cpp_aggregate_NumericVector(hfserie_fitted,ratio,A);

  NumericVector lfresiduals=lfserie_wbench-hfserie_fitted_aggreg;
  R_len_t firstnonna=0;
  for (;firstnonna<A && NumericVector::is_na(lfresiduals[firstnonna]);firstnonna++) { }
  if (firstnonna!=A) {
    double rhoinverse;
    if (rho==0) rhoinverse=0; else rhoinverse=1/rho;
    if (includedifferenciation) {
      for (R_len_t i=firstnonna+2;i<A;i++) {
        if (NumericVector::is_na(lfresiduals[i])) lfresiduals[i]=(1+rho)*lfresiduals[i-1]-rho*lfresiduals[i-2];
      }
      for (R_len_t i=firstnonna+1;i>1;i--) {
        lfresiduals[i-2]=(1+rhoinverse)*lfresiduals[i-1]-rhoinverse*lfresiduals[i];}
    } else {
      for (R_len_t i=firstnonna+1;i<A;i++) {
        if (NumericVector::is_na(lfresiduals[i])) lfresiduals[i]=rho*lfresiduals[i-1];
      }
      for (R_len_t i=firstnonna;i>0;i--) {lfresiduals[i-1]=rhoinverse*lfresiduals[i];}
    }
  }

  lfresiduals.attr("class")=StringVector::create("ts");
  lfresiduals.attr("tsp")=lfserie_wbench.attr("tsp");
  NumericVector hfresiduals=Cpp_bflSmooth(lfresiduals,tsphf[2]);
  NumericVector rests = hfserie_fitted+hfresiduals;
  rests.attr("class")=StringVector::create("ts");
  rests.attr("tsp")=hfresiduals.attr("tsp");
  
  List res=List::create(_["benchmarked.serie"] = Cpp_window_NumericVector(rests,tseps,NumericVector::create(tsphf[0]),NumericVector::create(tsphf[1])),
                        _["fitted.values"] = Cpp_window_NumericVector(hfserie_fitted,tseps,NumericVector::create(tsphf[0]),NumericVector::create(tsphf[1])),
                        _["regression"] = regresults,
                        _["model.list"] = List::create(_["hfserie"] = hfserie,
                                                       _["lfserie"] = lfserie,
                                                       _["include.rho"] = includerho,
                                                       _["include.differenciation"] = includedifferenciation,
                                                       _["set.coefficients"] = set_coefficients,
                                                       _["start.coeff.calc"] = startcoeffcalc,
                                                       _["end.coeff.calc"] = endcoeffcalc),
                        _["call"] = cl);
  res.attr("class") = StringVector::create("twoStepsBenchmark","list");
  return res;
}

// [[Rcpp::export]]
List Cpp_twoStepsBenchmark(NumericMatrix const& hfserie,NumericVector const& lfserie,
                           bool const& includedifferenciation,bool const& includerho,
                           NumericVector set_coefficients,
                           SEXP const& startcoeffcalc,SEXP const& endcoeffcalc,
                           double const& tseps,SEXP const& cl) {
  NumericVector startcoeffcalc_b;
  NumericVector endcoeffcalc_b;
  if (Rf_isNull(startcoeffcalc)) startcoeffcalc_b=NumericVector::create(NA_REAL);
  else if (Rf_isNumeric(startcoeffcalc)) startcoeffcalc_b=startcoeffcalc;
  else stop("The coefficients calculation window should be numeric");
  if (Rf_isNull(endcoeffcalc)) endcoeffcalc_b=NumericVector::create(NA_REAL);
  else if (Rf_isNumeric(endcoeffcalc)) endcoeffcalc_b=endcoeffcalc;
  else stop("The coefficients calculation window should be numeric");
  return Cpp_twoStepsBenchmark_impl(hfserie,lfserie,includedifferenciation,includerho,
                                    set_coefficients,startcoeffcalc_b,endcoeffcalc_b,
                                    tseps,cl);
}