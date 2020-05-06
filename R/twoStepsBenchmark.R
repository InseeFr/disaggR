#' Bends a time-serie with a lower frequency one
#' 
#' Bends a time-serie with a time serie of a lower frequency.
#' The process followed is an additive Denton, after a regression whose coefficients are
#' specified in regcoeffs. If one is not specified they are calculated.
#' 
#' @param hfserie the bended time-serie.
#' @param lfserie a time-serie whose frequency divide the higher one.
#' @param include.differenciation a boolean of length 1. TRUE if the regression includes a differenciation
#' @param include.rho a boolean of length 1. TRUE if the regression includes a rho
#' @param set.coeff an optional double of length 1. The coefficient of x in the regression
#' @param set.const an optional constant of length 1. The constant in the regression
#' @param start.coeff.calc the start for the coefficients estimation
#' @param start.coeff.calc the end for the coefficients estimation
#' 
#' @return A time serie. Its domain is the same as the high frequency serie.
#' 
#' @export
twoStepsBenchmark <- function(hfserie,lfserie,include.differenciation=FALSE,include.rho=FALSE,set.coeff=numeric(),set.const=numeric(),
                              start.coeff.calc=NA_real_,end.coeff.calc=NA_real_,...) {
  cl <- match.call()
  if ("cl" %in% names(cl)) cl <- list(...)$cl
  n <- if (is.matrix(hfserie)) nrow(hfserie) else length(hfserie)
  
  if (!include.differenciation) constant <- ts(rep(frequency(lfserie)/frequency(hfserie),n),frequency=frequency(hfserie),start=start(hfserie))
  else constant <- ts(1:n*(frequency(lfserie)/frequency(hfserie))^2,frequency=frequency(hfserie),start=start(hfserie))

  if (length(set.const) > 1) stop("set.const must be of a single value")
  if (length(set.const) == 1) names(set.const) <- "constant"
  if (length(set.coeff) ==1) names(set.coeff) <- "hfserie"
  
  return(Cpp_twoStepsBenchmark(cbind(constant,hfserie),lfserie,
                               include.differenciation,include.rho,
                               c(set.const,set.coeff),
                               start.coeff.calc,end.coeff.calc,
                               getOption("ts.eps"),cl))
}

#' Bends a time-serie with an annual frequency one
#' 
#' Bends a time-serie with a time serie of a frequency of 1. It is a wrapper of twoStepsBenchmark.
#' 
#' @param hfserie the bended time-serie.
#' @param annualserie a time-serie with a frequency of 1.
#' @param include.differenciation a boolean of length 1. TRUE if the regression includes a differenciation
#' @param include.rho a boolean of length 1. TRUE if the regression includes a rho
#' @param set.coeff an optional double of length 1. The coefficient of x in the regression
#' @param set.const an optional constant of length 1. The constant in the regression
#' @param start.coeff.calc the starting year for the coefficients estimation.
#' By default set to the starting year of the annual serie.
#' @param start.coeff.calc the ending year for the coefficients estimation
#' By default set to the ending year of the annual serie.
#' @param start.benchmark the starting year for the annual values to be taken into account
#' By default the starting year of the annual serie.
#' @param end.benchmark the starting year for the annual values to be taken into account
#' By default one year more than the end for the coefficients.
#' @param start.domain the start of the output serie. If it is before start.benchmark, the residuals are smoothed until
#' this value. 
#' By default the start of the high frequency serie
#' @param end.domain the end of the output. If it is after end.benchmark, the residuals are smoothed until this value.
#' By default 2 years after the end of the benchmark
#' 
#' @return A time serie.
#' 
#' @export
annualBenchmark <- function(hfserie,annualserie,include.differenciation=FALSE,include.rho=FALSE,set.coeff=numeric(),set.const=numeric(),
                            start.coeff.calc=start(annualserie)[1],end.coeff.calc=end(annualserie)[1],
                            start.benchmark=start(annualserie)[1],end.benchmark=end.coeff.calc+1,
                            start.domain=start(hfserie),end.domain=c(end.benchmark+2,frequency(hfserie))) {
  if (frequency(annualserie) !=1) stop("Not an annual time-serie")
  if (length(start.benchmark) !=1 || length(end.benchmark) != 1) stop("The start and end of the benchmark must be a single value year")
  twoStepsBenchmark(Cpp_window(hfserie,tseps=getOption("ts.eps"),start=start.domain,end=end.domain),
                    Cpp_window(annualserie,tseps=getOption("ts.eps"),start=start.benchmark,end=end.benchmark),
                    include.differenciation,include.rho,
                    set.coeff,set.const,
                    start.coeff.calc,end.coeff.calc,cl=match.call())
}