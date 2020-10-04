#include <Rcpp.h>

using namespace Rcpp;

template <class VectorOrMatrix>
void winparam(VectorOrMatrix x,double const& tseps,NumericVector const& start,NumericVector const& end,
              R_len_t& lengthy,unsigned int& casenumber,int& i0,int& i1,int& i1minusi0,int& max1,int& max2,double& ystart,double& yend,double& xfreq) {
  if (! Rf_inherits(x,"ts")) stop("Not a ts object");
  NumericVector const& xtsp=x.attr("tsp");
  double const xstart=(double)xtsp[0];
  double const xend=(double)xtsp[1];
  xfreq=(double)xtsp[2];
  double pstart=xstart;
  double pend=xend;
  switch (start.size()) {
  case 1:
    if (!(NumericVector::is_na(start[0]))) {pstart=start[0];}
    break;
  case 2: 
    pstart=start[0]+(start[1] - 1)/xfreq;
    break;
  default:
    stop("bad value for 'start'");
  break;
  }
  switch (end.size()) {
  case 1:
    if (!(NumericVector::is_na(end[0]))) {pend=end[0];}
    break;
  case 2: 
    pend=end[0]+(end[1] - 1)/xfreq;
    break;
  default:
    stop("bad value for 'end'");
  break;
  }
  if (pstart > pend) stop("'start' cannot be after 'end'");
  double stoff=ceil((pstart-xstart)*xfreq-tseps);
  ystart=stoff/xfreq+xstart;
  double enoff=floor((pend-xend)*xfreq+tseps);
  yend=enoff/xfreq+xend;
  int nold=round(xfreq * (xend - xstart))+1;
  
  if (pstart > xend + tseps/xfreq || pend < xstart - tseps/xfreq) {
    lengthy=floor(1 + (pend - pstart) * xfreq + tseps);
    casenumber = 1;
  }
  else {
    i0=1 + std::max(0, (int)stoff);
    i1=nold + std::min(0, (int)enoff);
    max1=std::max(0, -(int)stoff);
    max2=std::max(0,(int)enoff);
    i1minusi0=i1-i0;
    if (i1minusi0>=0) {
      lengthy=max1+max2+i1minusi0+1;
      casenumber=2;
    }
    else {
      lengthy=max1+max2;
      casenumber=3;
    }
  }
}

NumericVector window_impl(NumericVector const& x,double const& tseps,NumericVector const& start,NumericVector const& end) {
  R_len_t lengthy;
  unsigned int casenumber;
  int i0=0,i1=0,i1minusi0=0,max1=0,max2=0;
  double ystart,yend,xfreq;
  winparam(x,tseps,start,end,
           lengthy,casenumber,i0,i1,i1minusi0,max1,max2,ystart,yend,xfreq);
  NumericVector y(lengthy);
  
  switch(casenumber) {
  case 1:
    y=NumericVector(lengthy,NA_REAL);
    break;
  case 2: {
      for (R_len_t j=0;j<max1;j++) y[j]=NA_REAL;
      for (R_len_t j=0;j<=i1minusi0;j++) y[max1+j]=x[i0+j-1];
      for (R_len_t j=0;j<max2;j++) y[max1+i1minusi0+1+j]=NA_REAL;
      break;      
    }
  case 3:
    y=NumericVector(lengthy,NA_REAL);
    break;
  }
  y.attr("tsp")= NumericVector::create(ystart,yend,xfreq);
  y.attr("class")= StringVector::create("ts");
  if ((x.hasAttribute("names"))) y.attr("names") = x.attr("names");
  return y;
}

NumericMatrix window_impl(NumericMatrix const& x,double const& tseps,NumericVector const& start,NumericVector const& end) {
  R_len_t lengthy;
  unsigned int casenumber;
  int i0=0,i1=0,i1minusi0=0,max1=0,max2=0;
  double ystart,yend,xfreq;
  int const ny=x.ncol();
  winparam(x,tseps,start,end,
           lengthy,casenumber,i0,i1,i1minusi0,max1,max2,ystart,yend,xfreq);
  NumericMatrix y(lengthy,ny);
  
  switch(casenumber) {
  case 1: {
    NumericVector v(lengthy*ny,NA_REAL);
    y=NumericMatrix(lengthy,ny,v.begin());
    break;
  }
  case 2: {
      NumericVector v(ny,NA_REAL);
      for (R_len_t j=0;j<max1;j++) y(j,_)=v;
      for (R_len_t j=0;j<=i1minusi0;j++) y(max1+j,_)=x(i0+j-1,_);
      for (R_len_t j=0;j<max2;j++) y(max1+i1minusi0+1+j,_)=v;
      break;      
  }
  case 3:
    NumericVector v(lengthy*ny,NA_REAL);
    y=NumericMatrix(lengthy,ny,v.begin());
    break;
  }
  y.attr("tsp")= NumericVector::create(ystart,yend,xfreq);
  y.attr("class")= StringVector::create("ts");
  if ((x.hasAttribute("dimnames"))) y.attr("dimnames") = x.attr("dimnames");
  return y;
}

RcppExport SEXP Cpp_window(SEXP x,SEXP tseps,SEXP start,SEXP end) {
  SEXP res;
  if (Rf_isMatrix(x)) res = window_impl((NumericMatrix const&)x,as<double>(tseps),(NumericVector const&)start,(NumericVector const&)end);
  else res = window_impl((NumericVector const&)x,as<double>(tseps),(NumericVector const&)start,(NumericVector const&)end);
  return res;
}