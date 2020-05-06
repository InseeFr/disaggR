#include <RcppEigen.h>

using namespace Rcpp;
using Eigen::MatrixXd;
using Eigen::VectorXd;
using Eigen::ArrayXd;
using Eigen::Map;
using Eigen::ColPivHouseholderQR;
using Eigen::Upper;

// These overloaded functions multiply by omega_inv_sqrt without
// having to create the matrix (it is helpful if there are a lot of obs)
inline MatrixXd omega_inv_sqrt (MatrixXd const& x,double const& rho) {
  MatrixXd res(x.rows(),x.cols());
  res.row(0) = x.row(0)*pow(1-pow(rho,2),0.5);
  res.bottomRows(x.rows()-1) = x.bottomRows(x.rows()-1)-rho*x.topRows(x.rows()-1);
  return res;
}

inline VectorXd omega_inv_sqrt (VectorXd const& x,double const& rho) {
  VectorXd res(x.size());
  res << x[0]*pow(1-pow(rho,2),0.5),
                     x.tail(x.size()-1)-rho*x.head(x.size()-1);
  return res;
}


//' @export
// [[Rcpp::export]]
List praislm(NumericMatrix& X,NumericVector& y,bool const& includerho,bool const& includedifferenciation,NumericVector const& set_coefficients,SEXP const& cl) {
  List modellist = List::create(_["X"]=clone(X),_["y"]=clone(y),_["include.rho"]=includerho,_["include.differenciation"]=includedifferenciation,_["set.coefficients"]=set_coefficients);
  if (! Rf_inherits(y,"ts")) stop("Not a ts object");
  if (includedifferenciation) {
    NumericMatrix X_diff(X.nrow()-1,X.ncol());
    for (int i=0;i<X.ncol();i++) {X_diff(_,i)=diff(X(_,i));}
    if ((X.hasAttribute("dimnames"))) X_diff.attr("dimnames") = X.attr("dimnames");
    X = X_diff;
    NumericVector y_diff=diff(y);
    NumericVector tsp_diffy=y.attr("tsp");
    tsp_diffy[0]+=1/tsp_diffy[2];
    y_diff.attr("tsp")=tsp_diffy;
    y_diff.attr("class")=y.attr("class");
    y = y_diff;
  }
  
  if (!(X.hasAttribute("dimnames"))) stop("The high frequency serie must have column names");
  StringVector set_coefficients_names(set_coefficients.size());
  if (set_coefficients.size()==0) set_coefficients_names=StringVector::create();
  else {
    if (!(set_coefficients.hasAttribute("names"))) stop("The coefficients setter must be empty or have names");
    set_coefficients_names=set_coefficients.names();
  }

  StringVector const& X_colnames=colnames(X);
  NumericVector coefficients(X.ncol(),NA_REAL);
  coefficients.names()=X_colnames;
  IntegerVector match_set=match(set_coefficients_names,X_colnames)-1;
  for (R_len_t i=0;i<set_coefficients.size();i++) {
    if (NumericVector::is_na(set_coefficients[i])) stop("A coefficient can't be set to NA");
    if (NumericVector::is_na(match_set[i])) stop("The names of the setted coefficients must be a real one");
    coefficients(match_set[i])=set_coefficients[i];
  }
  
  IntegerVector match_notset(coefficients.size()-set_coefficients.size());
  int j=0;
  for (R_len_t i=0;i<coefficients.size();i++) {
    if (NumericVector::is_na(coefficients[i])) {
      match_notset(j)=i;
      j++;
    }
  }

  NumericVector offset(X.nrow(),(double)0);
  for (int i=0;i<match_set.size();i++) {
    int j=match_set[i];
    offset=offset+coefficients[j]*(NumericVector)X(_,j);
  }
  NumericVector y_offset_excluded = y-offset;
  
  NumericMatrix X_offset_excluded(y.size(),match_notset.size());
  for (int i=0;i<match_notset.size();i++) X_offset_excluded(_,i)=X(_,match_notset(i));
  
  const Map<VectorXd> y_eigen(as<Map<Eigen::VectorXd> >(y_offset_excluded));
  const Map<MatrixXd> X_eigen(as<Map<Eigen::MatrixXd> >(X_offset_excluded));
  const Map<MatrixXd> offset_eigen(as<Map<Eigen::MatrixXd> >(offset));

  int const nrowX=X_eigen.rows();
  int const ncolX=X_eigen.cols();
  double rho=0;
  VectorXd coefficients_eigen(ncolX);
  ColPivHouseholderQR<MatrixXd> PQR = X_eigen.colPivHouseholderQr();
  if (PQR.rank() !=ncolX) stop("The regressed series should have a perfect rank");
  coefficients_eigen=PQR.solve(y_eigen);
  if (includerho) {
    double drho=1;
    double rho_prec=0;
    int const i_max = 50;
    int i=1;
    
    for (;drho>0.001 && i<i_max;i++) {
      const VectorXd residuals = y_eigen-X_eigen*coefficients_eigen;
      const VectorXd residuals_centered = residuals-VectorXd::Constant(nrowX,residuals.mean());
      VectorXd tailresc = residuals_centered.tail(nrowX-1);
      VectorXd headresc = residuals_centered.head(nrowX-1);
      rho=tailresc.transpose()*headresc;
      rho/=pow(tailresc.transpose()*tailresc,0.5);
      rho/=pow(headresc.transpose()*headresc,0.5);
      drho=std::abs(rho-rho_prec);
      rho_prec=rho;
      
      VectorXd y_star = omega_inv_sqrt((VectorXd)y_eigen,rho);
      MatrixXd X_star = omega_inv_sqrt((MatrixXd)X_eigen,rho);
      
      ColPivHouseholderQR<MatrixXd> PQR = X_star.colPivHouseholderQr();
      coefficients_eigen=PQR.solve(y_star);
    }
    if (i >= i_max) warning("Maximum iterations without convergence");
  }

  VectorXd const fitted_eigen = X_eigen*coefficients_eigen;
  VectorXd const residuals_eigen = y_eigen-fitted_eigen;
  VectorXd const residuals_decor_eigen = omega_inv_sqrt((VectorXd)residuals_eigen,rho);
  double df_residual = nrowX-ncolX;
  double resvar=residuals_eigen.norm() / std::sqrt(df_residual);
  
  VectorXd se_eigen=resvar*(PQR.colsPermutation() * PQR.matrixQR().topRows(ncolX).
                                  triangularView<Upper>().solve(MatrixXd::Identity(ncolX, ncolX)).rowwise().norm());
  
  NumericVector se(coefficients.size(),NA_REAL);
  se.names()=coefficients.names();
  for (R_len_t i=0;i<match_notset.size();i++) {
    R_len_t j=match_notset[i];
    se[j]=se_eigen[i];
    coefficients[j]=coefficients_eigen[i];
  }

  NumericVector residuals=wrap(residuals_eigen);
  NumericVector residuals_decor=wrap(residuals_decor_eigen);
  NumericVector fitted=wrap(fitted_eigen);
  NumericVector fitted_decor=wrap(omega_inv_sqrt((VectorXd)fitted_eigen,rho));
  
  residuals.attr("tsp")=y.attr("tsp");
  residuals_decor.attr("tsp")=y.attr("tsp");
  fitted.attr("tsp")=y.attr("tsp");
  fitted_decor.attr("tsp")=y.attr("tsp");

  residuals.attr("class")=y.attr("class");
  residuals_decor.attr("class")=y.attr("class");
  fitted.attr("class")=y.attr("class");
  fitted_decor.attr("class")=y.attr("class");
  
  List res = List::create(_["coefficients"]=coefficients,
                          _["residuals"]=residuals,
                          _["fitted.values"]=fitted,
                          _["se"]=se,
                          _["df.residual"]=df_residual,
                          _["rho"]=rho,
                          _["residuals.decorrelated"]=residuals_decor,
                          _["fitted.values.decorrelated"]=fitted_decor,
                          _["model.list"]=modellist,
                          _["call"]=cl);
  res.attr("class") = StringVector::create("praislm");
  return res;
}