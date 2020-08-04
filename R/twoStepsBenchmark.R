residuals_extrap_sequence <- function(u0,u1,rho,n,include.differenciation) {
  if (include.differenciation) {
    if (rho == 1) u1 + (u1-u0) * (1:n)
    else (u1-u0)*(1-rho^(2:(n+1)))/(1-rho)+u0
  }
  else u1*(rho^(1:n))
}

#' Extrapolation function for the residuals in a twoStepsBenchmark
#' 
#' This function is the rule to extrapolate the low-frequency residuals
#' If include.differenciation is true, u(n+1)-u(n) = rho*(u(n)-u(n-1))
#' Else u(n+1) = rho * u(n)
#'
#' @param lfresiduals the residuals to extrapolate
#' @param rho the autocorrelation parameter of the regression
#' @param n an integer, how many extrapolations to do.
#' @param include.differenciation a boolean, the same as submitted
#' to twoStepsBenchmark
#'
#' @return a numeric, the extrapolated sequence of residuals, to replace the NA of
#' the residuals
#' @keywords internal
#' @export
residuals_extrap <- function(lfresiduals,rho,include.differenciation) {
  valplaces <- which(!is.na(lfresiduals))
  if (length(valplaces) != 0) {
    firstval <- valplaces[1L]
    lastval <- valplaces[length(valplaces)]
    if (rho==0) rhoinverse <- 0 else rhoinverse <- 1/rho
    if (lastval != length(lfresiduals)) {
      lfresiduals[(lastval+1):length(lfresiduals)] <-
        residuals_extrap_sequence(lfresiduals[lastval-1],
                                  lfresiduals[lastval],
                                  rho,
                                  length(lfresiduals)-lastval,
                                  include.differenciation)
    }
    if (firstval != 1L) {
      lfresiduals[(firstval-1):1] <-
        residuals_extrap_sequence(lfresiduals[firstval+1],
                                  lfresiduals[firstval],
                                  rhoinverse,
                                  firstval-1,
                                  include.differenciation)
    }
  }
  return(lfresiduals)
}

#' @importFrom utils head
twoStepsBenchmark_impl <- function(hfserie,lfserie,
                                   include.differenciation,include.rho,
                                   set_coefficients,start.coeff.calc,end.coeff.calc,
                                   cl) {
  if (! inherits(lfserie,"ts") || ! inherits(hfserie,"ts")) stop("Not a ts object")
  if (!is.null(dim(lfserie)) && dim(lfserie)[2] != 1) stop("The low frequency serie must be one-dimensional")
  
  tsphf <- tsp(hfserie)
  tsplf <- tsp(lfserie)
  ratio <- frequency(hfserie)/frequency(lfserie)
  
  # Estimation of the reg coefficients
  
  y <- window(lfserie,start=start.coeff.calc,end=end.coeff.calc,extend = TRUE)
  tspy <- tsp(y)
  x <- aggregate.ts(
    window(hfserie,tspy[1],tspy[2]+1/tspy[3]-1/tsphf[3],extend = TRUE)
    ,nfrequency = tspy[3]
  )
  
  regresults <- praislm(x,y,include.rho,include.differenciation,set_coefficients,cl)
  
  # Application of the reg coefficients
  
  startdomain_extended <- floor(tsphf[1]*tsplf[3])/tsplf[3]
  enddomain_extended <- ceiling((tsphf[2]+1/tsphf[3])*tsplf[3])/tsplf[3]-1/tsphf[3]
        # This window is the smallest that is all around the domain of the hfserie.
  
  lfserie_wbench <- window(lfserie,start=startdomain_extended,end=enddomain_extended,extend = TRUE)
  hfserie_wbench <- window(hfserie,start=startdomain_extended,end=enddomain_extended,extend = TRUE)
  
  A <- length(lfserie_wbench)
  
  rho <- regresults$rho
  
  hfserie_fitted <- ts(as.numeric(hfserie_wbench %*% regresults$coefficients),
                       start=tsp(hfserie_wbench)[1],
                       frequency=frequency(hfserie_wbench))
  
  hfserie_fitted_aggreg <- aggregate.ts(hfserie_fitted,nfrequency = tsplf[3])
  
  lfresiduals <- lfserie_wbench-hfserie_fitted_aggreg
  lfresiduals <- residuals_extrap(lfresiduals,rho,include.differenciation)
  
  hfresiduals <- bflSmooth(lfresiduals,frequency(hfserie))
  
  rests <- hfserie_fitted+hfresiduals
  
  res <- list(benchmarked.serie = window(rests,start=tsp(hfserie)[1],end=tsp(hfserie)[2],extend = TRUE),
              fitted.values = window(hfserie_fitted,end=tsp(hfserie)[2],extend = TRUE),
              regression = regresults,
              smoothed.part = hfresiduals,
              model.list = list(hfserie = hfserie,
                                lfserie =lfserie,
                                include.rho = include.rho,
                                include.differenciation=include.differenciation,
                                set.coefficients=set_coefficients,
                                start.coeff.calc = start.coeff.calc,
                                end.coeff.calc = end.coeff.calc),
              call = cl)
  class(res) <- c("twoStepsBenchmark","list")
  return(res)
}

#' Bends a time-serie with a lower frequency one
#' 
#' twoStepsBenchmark bends a time-serie with a time-serie of a lower frequency.
#' The procedure involved is a Prais-Winsten regression, then an additive
#' Denton benchmark.
#' annualBenchmark is a wrapper of the main function, that applies more specifically
#' to annual series, and changes the default window parameters to the ones
#' that are commonly used by quarterly national accounts.
#' 
#' @aliases annualBenchmark
#' @usage
#' twoStepsBenchmark(hfserie,lfserie,include.differenciation=FALSE,include.rho=FALSE,
#'                   set.coeff=NULL,set.const=NULL,
#'                   start.coeff.calc=NULL,end.coeff.calc=NULL,
#'                   start.benchmark=NULL,end.benchmark=NULL,
#'                   start.domain=NULL,end.domain=NULL,...)
#'
#' annualBenchmark(hfserie,lfserie,include.differenciation=FALSE,include.rho=FALSE,
#'                 set.coeff=NULL,set.const=NULL,
#'                 start.coeff.calc=start(lfserie)[1],end.coeff.calc=end(lfserie)[1],
#'                 start.benchmark=start(lfserie)[1],end.benchmark=end.coeff.calc+1,
#'                 start.domain=start(hfserie),
#'                 end.domain=c(end.benchmark+2,frequency(hfserie)))
#' 
#' @param hfserie the bended time-serie. It can be a matrix time-serie.
#' @param lfserie a time-serie whose frequency divides the frequency of `hfserie`.
#' @param include.differenciation a boolean of length 1. If `TRUE`, `lfserie` and
#' `hfserie` are differenced before the estimation of the regression.
#' @param include.rho a boolean of length 1. If `TRUE`, the regression includes an autocorrelation
#' parameter for the residuals. The applied procedure is a Prais-Winsten estimation.
#' @param set.coeff an optional double, that allows the user to set the regression coefficients instead
#' of evaluating them.
#' If `hfserie` is a matrix, each column initializes a coefficient with the same name as the column name.
#' Hence, `set.coeff` has to be a named double, which will optionally set some coefficients instead of
#' evaluating them. 
#' @param set.const an optional double of length 1, that sets the regression constant.
#' The constant is actually an automatically added column to `hfserie`. Using `set.constant=3`
#' is equivalent to using `set.coeff=c(constant=3)`.
#' @param start.coeff.calc an optional start for the estimation of the coefficients of the regression.
#' Should be a double or a numeric of length 2, like a window for `lfserie`. If NULL, the start is defined by lfserie's window.
#' @param end.coeff.calc an optional end for the estimation of the coefficients of the regression.
#' Should be a double or a numeric of length 2, like a window for `lfserie`. If NULL, the end is defined by lfserie's window.
#' @param start.benchmark an optional start for `lfserie` to bend `hfserie`.
#' Should be a double or a numeric of length 2, like a window for `lfserie`. If NULL, the start is defined by lfserie's window.
#' @param end.benchmark an optional end for `lfserie` to bend `hfserie`.
#' Should be a double or a numeric of length 2, like a window for `lfserie`. If NULL, the start is defined by lfserie's window.
#' @param start.domain the start of the output high-frequency serie. It also defines the smoothing window :
#' The low-frequency residuals will be extrapolated until they contain the smallest low-frequency window that around the high-frequency
#' domain window.
#' Should be a double or a numeric of length 2, like a window for `hfserie`. If NULL, the start is defined by hfserie's window.
#' @param end.domain the end of the output high-frequency serie. It also defines the smoothing window :
#' The low-frequency residuals will be extrapolated until they contain the smallest low-frequency window that around the high-frequency
#' domain window.
#' Should be a double or a numeric of length 2, like a window for `hfserie`. If NULL, the start is defined by hfserie's window.
#' @param \dots if the dots contain a cl item, its value overwrites the value
#' of the returned call. This feature allows to build wrappers.
#' @return
#' twoStepsBenchark returns an object of class "`twoStepsBenchmark`".
#' 
#' The functions `summary` can be used to obtain and print a summary of the regression used by the benchmark.
#' The functions `plot` and `autoplot` (the latter requires to load \pkg{ggplot2}) produces graphics of the benchmarked
#' serie and the bending serie.
#' The function \link{in_sample} produces in-sample predictions with the inner regression.
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
#'   \item{smoothed.part}{the smoothed part of the two-steps benchmark.}
#'   \item{model.list}{a list containing all the arguments submitted to the function.}
#'   \item{call}{the matched call (either of twoStepsBenchmark or annualBenchmark)}
#' @examples
#' 
#' ## How to use annualBenchmark or twoStepsBenchark
#' 
#' benchmark <- annualBenchmark(hfserie = turnover,
#'                             lfserie = construction,
#'                             include.differenciation = TRUE)
#' as.ts(benchmark)
#' coef(benchmark)
#' summary(benchmark)
#' library(ggplot2)
#' autoplot(in_sample(benchmark))
#' 
#' ## How to manually set the coefficient
#' 
#' benchmark2 <- annualBenchmark(hfserie = turnover,
#'                               lfserie = construction,
#'                               include.differenciation = TRUE,
#'                               set.coeff = 0.1)
#' coef(benchmark2)
#'
#' @export
twoStepsBenchmark <- function(hfserie,lfserie,include.differenciation=FALSE,include.rho=FALSE,set.coeff=NULL,set.const=NULL,
                              start.coeff.calc=NULL,end.coeff.calc=NULL,
                              start.benchmark=NULL,end.benchmark=NULL,
                              start.domain=NULL,end.domain=NULL,...) {
  cl <- match.call()
  
  if (is.null(set.coeff)) set.coeff <- numeric()
  if (is.null(set.const)) set.const <- numeric()
  
  if ("cl" %in% names(cl)) cl <- list(...)$cl
  n <- if (is.matrix(hfserie)) nrow(hfserie) else length(hfserie)
  
  if (!include.differenciation) constant <- ts(rep(frequency(lfserie)/frequency(hfserie),n),frequency=frequency(hfserie),start=start(hfserie))
  else constant <- ts(1:n*(frequency(lfserie)/frequency(hfserie))^2,frequency=frequency(hfserie),start=start(hfserie))
  
  if (length(set.const) > 1) stop("set.const must be of a single value")
  if (length(set.const) == 1) names(set.const) <- "constant"
  if (length(set.coeff) == 1) names(set.coeff) <- "hfserie"
  
  return(twoStepsBenchmark_impl(window(cbind(constant,hfserie),start.domain,end.domain,extend=TRUE),
                                window(lfserie,start.benchmark,end.benchmark,extend=TRUE),
                                include.differenciation,include.rho,
                                c(set.const,set.coeff),
                                start.coeff.calc,end.coeff.calc,cl))
}

#' @export
annualBenchmark <- function(hfserie,lfserie,include.differenciation=FALSE,include.rho=FALSE,set.coeff=NULL,set.const=NULL,
                            start.coeff.calc=start(lfserie)[1],end.coeff.calc=end(lfserie)[1],
                            start.benchmark=start(lfserie)[1],end.benchmark=end.coeff.calc+1,
                            start.domain=start(hfserie),end.domain=c(end.benchmark+2,frequency(hfserie))) {
  if (frequency(lfserie) !=1) stop("Not an annual time-serie")
  if (length(start.benchmark) != 1 || length(end.benchmark) != 1) stop("The start and end of the benchmark must be a single value year")
  twoStepsBenchmark(hfserie,lfserie,
                    include.differenciation,include.rho,
                    set.coeff,set.const,
                    start.coeff.calc,end.coeff.calc,
                    start.benchmark,end.benchmark,
                    start.domain,end.domain,cl=match.call())
}