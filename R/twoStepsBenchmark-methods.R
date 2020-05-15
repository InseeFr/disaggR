#' Extracting the regression of a twoStepsBenchmark
#' 
#' prais is a function which extracts the regression, a praislm object,
#' of a twoStepsBenchmark.
#' 
#' @aliases praislm
#' @param x a twoStepsBenchmark
#' @return
#' prais returns an object of class "`praislm`".
#' 
#' The functions that can be used on that class are almost the same than
#' for the class `twoStepsBenchmark`.
#' `summary`, `coefficients`, `residuals` will return the same values.
#' However, as for `fitted.values`, the accessor returns the fitted values
#' of the regression, not the high-frequency, eventually integrated, time-serie
#' contained in a twoStepsBenchmark.
#' Th
#' 
#' An object of class "`praislm`" is a list containing the following components :
#'   \item{coefficients}{a named vector of coefficients.}
#'   \item{residuals}{the residuals, that is response minus fitted values.}
#'   \item{fitted.values}{a time-serie, the fitted mean values}
#'   \item{se}{a named vector of standard errors.}
#'   \item{df.residuals}{the residual degrees of freedom.}
#'   \item{rho}{the autocorrelation coefficients of the residuals. It
#'   is equal to zero if twoStepsBenchmark was called with `include.rho=FALSE`}
#'   \item{residuals.decorrelated}{the residuals of the model after having been
#'   transformed by rho in a least square model.}
#'   \item{fitted.values.decorrelated}{the fitted values of the model after having been
#'   transformed by rho in a least square model.}
#' @export
prais <- function(x) UseMethod("prais")
#' @export
prais <- function(x) x$regression

#' @export
as.ts.twoStepsBenchmark <- function(x) x$benchmarked.serie

#' @importFrom stats coef
#' @export
coef.twoStepsBenchmark <- function(x) coef(prais(x))

#' @importFrom stats residuals
#' @export
residuals.twoStepsBenchmark <- function(x) residuals(prais(x))

#' @importFrom stats vcov
#' @export
vcov.twoStepsBenchmark <- function(x) vcov(prais(x))

#' @importFrom stats fitted
#' @export
fitted.twoStepsBenchmark <- function(x) x$fitted.values

#' @export
model.list <- function(x) UseMethod("model.list")
#' @export
model.list.twoStepsBenchmark <- function(x) x$model

#' @export
se.twoStepsBenchmark <- function(x) se(prais(x))

#' @export
rho.twoStepsBenchmark <- function(x) rho(prais(x))

#' @export
print.twoStepsBenchmark <- function(x,...) {
  print(prais(x),...)
  stats:::print.ts(as.ts(x))
  invisible(x)
}

#' @export
summary.twoStepsBenchmark <- function(x, ...) {
  summary.praislm(prais(x),...)
}