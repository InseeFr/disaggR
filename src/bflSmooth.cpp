#include <RcppEigen.h>

using namespace Rcpp;
using Eigen::MatrixXd;
using Eigen::VectorXd;
using Eigen::Map;

// [[Rcpp::export]]
NumericVector Cpp_bflSmooth(NumericVector const& lfserie,unsigned int const& nfrequency) {
  if (! Rf_inherits(lfserie,"ts")) stop("Not a ts object");
  NumericVector const& tspold=lfserie.attr("tsp");
  unsigned int oldfreq=(unsigned int)tspold[2];
  if ((double) oldfreq != tspold[2]) stop("The frequency of the smoothed serie must be an integer");
  if (nfrequency==0) stop("The new frequency must be strictly positive");
  if (nfrequency==oldfreq) return lfserie;
  if (nfrequency%oldfreq!=0) stop("The new frequency must be a multiple of the lower one");
  if (lfserie.hasAttribute("dim")) if (((NumericVector)lfserie.attr("dim"))[1] != 1) stop("The low frequency serie must be one-dimensional");
  
  unsigned int const ratio = nfrequency/oldfreq;
  
  const Map<VectorXd> lfserie_eigen(as<Map<Eigen::VectorXd> >(lfserie));

  unsigned int const& A=(lfserie.size());
  unsigned int const& n=ratio*A;

  // Initialization of the matrix M
  MatrixXd M(A,n);
  M=MatrixXd::Zero(A,n);
  for (unsigned int i=0;i<A;i++) {
    for (unsigned int j=ratio*i;j<ratio*(i+1);j++) M(i,j)=1;
  }

  // Initialization of the matrix T
  MatrixXd T(n,n);
  T=MatrixXd::Zero(n,n);
  for (unsigned int i=0;i<n;i++) {
    for(unsigned int j=0;j<=i;j++) T(i,j)=1;
  }

  // Calculus of the product MT
  MatrixXd MT(A,n);
  MT = M*T;

  // Definition of m1 and tildem
  VectorXd m1=MT.leftCols<1>();
  MatrixXd tildem=MT.rightCols(n-1);
  
  // Calculus of the inverse of tildem t_tildem
  MatrixXd inversemm=(tildem*tildem.transpose()).inverse();
  
  // Calculus of x11
  
  double x11=((m1.transpose()*inversemm*lfserie_eigen)(0,0))/((m1.transpose()*inversemm*m1)(0,0));
  
  // Calculus of lambda
  
  VectorXd lambda(A);
  lambda=inversemm*(lfserie_eigen-m1*x11);
  
  // Calculus of result
  
  VectorXd primey(n);
  primey << x11,tildem.transpose()*lambda;
  VectorXd x(n);
  x=T*primey;
  NumericVector res=wrap(x);
  res.attr("class")=StringVector::create("ts");
  res.attr("tsp")=NumericVector::create(tspold[0],
                                        tspold[0]+(double)(n-1)/(double)nfrequency,
                                        (double)nfrequency);
  return res;
}
