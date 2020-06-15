#' Bends a time-serie with a lower frequency one
#' 
#' twoStepsBenchmark bends a time-serie with a time serie of a lower frequency.
#' The procedure is a Prais-Winsten regression, then an additive Denton.
#' annualBenchmark is a wrapper of the main function, that applies more specifically
#' to annual series, and changes the default window parameters.
#' 
#' @aliases annualBenchmark
#' @usage
#' twoStepsBenchmark(hfserie,lfserie,include.differenciation=FALSE,include.rho=FALSE,set.coeff=numeric(),set.const=numeric(),
#'                   start.coeff.calc=NULL,end.coeff.calc=NULL,
#'                   start.benchmark=NULL,end.benchmark=NULL,
#'                   start.domain=NULL,end.domain=NULL,...)
#'
#' annualBenchmark(hfserie,annualserie,include.differenciation=FALSE,include.rho=FALSE,set.coeff=numeric(),set.const=numeric(),
#'                 start.coeff.calc=start(annualserie)[1],end.coeff.calc=end(annualserie)[1],
#'                 start.benchmark=start(annualserie)[1],end.benchmark=end.coeff.calc+1,
#'                 start.domain=start(hfserie),end.domain=c(end.benchmark+2,frequency(hfserie)))
#' 
#' @param hfserie the bended time-serie.
#' @param lfserie a time-serie whose frequency divides the frequency of `hfserie`.
#' @param include.differenciation a boolean of length 1. If `TRUE`, lfserie and the aggregated hfserie
#' before the estimation of the regression.
#' @param include.rho a boolean of length 1. If `TRUE`, the regression includes an autocorrelation
#' parameter for the residuals. The procedure applied is the Prais-Winsten estimation.
#' @param set.coeff an optional double of length 1, that sets the regression coefficient.
#' @param set.const an optional double of length 1, that sets the regression constant.
#' @param start.coeff.calc an optional start for the coefficients estimation of the regression.
#' Should be a double or a numeric of length 2, like a window for `lfserie`. If NULL, the start is defined by lfserie's window.
#' @param end.coeff.calc an optional end for the coefficients estimation of the regression.
#' Should be a double or a numeric of length 2, like a window for `lfserie`. If NULL, the end is defined by lfserie's window.
#' @param start.benchmark an optional start for `lfserie` to bend `hfserie`.
#' Should be a double or a numeric of length 2, like a window for `lfserie`. If NULL, the start is defined by lfserie's window.
#' @param end.benchmark an optional start for `lfserie` to bend `hfserie`.
#' Should be a double or a numeric of length 2, like a window for `lfserie`. If NULL, the start is defined by lfserie's window.
#' @param start.domain the start of the output high-frequency serie. It also defines the window for the smoothing :
#' The low-frequency residuals will be extrapolated until the smallest low-frequency window that contains the high-frequency
#' domain window.
#' Should be a double or a numeric of length 2, like a window for `hfserie`. If NULL, the start is defined by hfserie's window.
#' @param end.domain the end of the output high-frequency serie. It also defines the window for the smoothing :
#' The low-frequency residuals will be extrapolated until the smallest low-frequency window that contains the high-frequency
#' domain window.
#' Should be a double or a numeric of length 2, like a window for `hfserie`. If NULL, the start is defined by hfserie's window.

#' @return
#' twoStepsBenchark returns an object of class "`twoStepsBenchmark`".
#' 
#' The functions `summary` can be used to obtain and print a summary of the regression used by the benchmark.
#' The functions `plot` and `autoplot` (the latter requires to load \pkg{ggplot2}) produces graphics of the benchmarked
#' serie and the bending serie.
#' The function \link{insample} produces in-sample predictions with the inner regression.
#' The generic accessor functions `as.ts`, `prais`, `coefficients`, `residuals`, `fitted.values`, `model.list`, `se`, `rho`
#' extract various useful features of the returned value.
#' 
#' An object of class "`twoStepsBenchmark`" is a list containing the following components :
#'   \item{benchmarked.serie}{a time-serie, that is the result of the benchmark.}
#'   \item{fitted.values}{a time-serie, that is the high-frequency serie as it is
#'   after having applied the regression coefficients.
#'   The difference `benchmarked.serie` - `fitted.values` is then a smoothed residual, eventually integrated
#'   if `include.differenciation=TRUE`.}
#'   \item{regression}{an object of class praislm, it is the regression on which relies the
#'   benchmark. It can be extracted with the function \link{prais}}
#'   \item{model.list}{a list containing all the arguments submitted to the function.}
#'   \item{call}{the matched call (either of twoStepsBenchmark or annualBenchmark)}
#'
#' @export
twoStepsBenchmark <- function(hfserie,lfserie,include.differenciation=FALSE,include.rho=FALSE,set.coeff=numeric(),set.const=numeric(),
                              start.coeff.calc=NULL,end.coeff.calc=NULL,
                              start.benchmark=NULL,end.benchmark=NULL,
                              start.domain=NULL,end.domain=NULL,...) {
  cl <- match.call()
  
  if ("cl" %in% names(cl)) cl <- list(...)$cl
  n <- NROW(hfserie)
  
  if (!include.differenciation) constant <- ts(rep(frequency(lfserie)/frequency(hfserie),n),frequency=frequency(hfserie),start=start(hfserie))
  else constant <- ts(1:n*(frequency(lfserie)/frequency(hfserie))^2,frequency=frequency(hfserie),start=start(hfserie))

  if (length(set.const) > 1) stop("set.const must be of a single value")
  if (length(set.const) == 1) names(set.const) <- "constant"
  if (length(set.coeff) == 1) names(set.coeff) <- "hfserie"
  
  return(Cpp_twoStepsBenchmark(window(cbind(constant,hfserie),start.domain,end.domain,extend=TRUE),
                               window(lfserie,start.benchmark,end.benchmark,extend=TRUE),
                               include.differenciation,include.rho,
                               c(set.const,set.coeff),
                               start.coeff.calc,end.coeff.calc,
                               getOption("ts.eps"),cl))
}

#' @export
annualBenchmark <- function(hfserie,annualserie,include.differenciation=FALSE,include.rho=FALSE,set.coeff=numeric(),set.const=numeric(),
                            start.coeff.calc=start(annualserie)[1],end.coeff.calc=end(annualserie)[1],
                            start.benchmark=start(annualserie)[1],end.benchmark=end.coeff.calc+1,
                            start.domain=start(hfserie),end.domain=c(end.benchmark+2,frequency(hfserie))) {
  if (frequency(annualserie) !=1) stop("Not an annual time-serie")
  if (length(start.benchmark) !=1 || length(end.benchmark) != 1) stop("The start and end of the benchmark must be a single value year")
  twoStepsBenchmark(hfserie,annualserie,
                    include.differenciation,include.rho,
                    set.coeff,set.const,
                    start.coeff.calc,end.coeff.calc,
                    start.benchmark,end.benchmark,
                    start.domain,end.domain,cl=match.call())
}