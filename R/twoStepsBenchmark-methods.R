#' @export
as.ts.twoStepsBenchmark <- function(x) x$benchmarked.serie

#' Extracting the coefficients of a twoStepsBenchmark
#' 
#' Extracts the coefficients of the twoStepsBenchark regression.
#' 
#' @param x a twoStepsBenchmark
#' @return a numeric vector
#' 
#' @importFrom stats coef
#' @export
coef.twoStepsBenchmark <- function(x) coef(x$regression)

#' Extracting the residuals of a twoStepsBenchmark
#' 
#' Extracts the residuals of the twoStepsBenchark regression.
#' 
#' @param x a twoStepsBenchmark
#' @return a numeric vector
#'
#' @importFrom stats residuals
#' @export
residuals.twoStepsBenchmark <- function(x) residuals(x$regression)

#' @importFrom stats vcov
#' @export
vcov.twoStepsBenchmark <- function(x) vcov(x$regression)

#' @importFrom stats fitted
#' @export
fitted.twoStepsBenchmark <- function(x) x$fitted.values

#' Extracting the Model list from a twoStepsBenchmark
#' 
#' The model list of a twoStepsBenchmark is a list which contains the parameters provided to the Benchmark.
#' 
#' @param x a twoStepsBenchmark
#' 
#' @return A list containing hfserie, lfserie, include.rho, include.differenciation, set.coeff, set. const, start.coeff.calc
#' and end.coeff.calc.
#' 
#' @export
model.list <- function(x) UseMethod("model.list")
#' @export
model.list.twoStepsBenchmark <- function(x) x$model

#' @export
se.twoStepsBenchmark <- function(x) se(x$regression)

#' @export
rho.twoStepsBenchmark <- function(x) rho(x$regression)

#' @export
print.twoStepsBenchmark <- function(x,...) {
  print(x$regression,...)
  stats:::print.ts(as.ts(x))
  invisible(x)
}

#' @export
summary.twoStepsBenchmark <- function(x, ...) {
  summary.praislm(x$regression,...)
}

#' @export
insample.twoStepsBenchmark <- function(object,type="changes") {
  insample.praislm(object$regression,type)
}