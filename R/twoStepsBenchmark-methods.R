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
#' @examples
#' benchmark <- twoStepsBenchmark(turnover,construction); prais(benchmark)
#' @export
prais <- function(x) UseMethod("prais")
#' @export
prais <- function(x) x$regression

#' @importFrom stats as.ts
#' @export
as.ts.twoStepsBenchmark <- function(x, ...) x$benchmarked.serie

#' @importFrom stats coef
#' @export
coef.twoStepsBenchmark <- function(object, ...) coef(prais(object))

#' @importFrom stats residuals
#' @export
residuals.twoStepsBenchmark <- function(object, ...) residuals(prais(object))

#' @importFrom stats vcov
#' @export
vcov.twoStepsBenchmark <- function(object, ...) vcov(prais(object))

#' @importFrom stats fitted
#' @export
fitted.twoStepsBenchmark <- function(object, ...) object$fitted.values

#' Extracting all the arguments submitted to generate an object
#' 
#' The function `model.list` returns the arguments submitted
#' to the function \link{praislm} or a \link{twoStepsBenchmark}.
#' 
#' These are returned as they are after evaluation, model.list doesn't
#' return a call.
#'   
#' @usage
#' model.list(object)
#' @param object a praislm or twoStepsBenchmark object.
#' @return
#' a list containing every evaluated arguments
#' @examples
#' benchmark <- twoStepsBenchmark(turnover,construction); model.list(benchmark)
#'
#' @export
model.list <- function(object) UseMethod("model.list")
#' @export
model.list.twoStepsBenchmark <- function(object) object$model.list

#' Extracting the smoothed part of a twoStepsBenchmark
#' 
#' The function `smoothed.part` returns the smoothed part of a
#' \link{twoStepsBenchmark}. It derives from the residuals of the
#' aggregated regression, with some differences :
#'    * it is eventually integrated if `include.differenciation=TRUE`.
#'    * it is extrapolated to match the domain window.
#'    * it is smoothed with an additive Denton benchmark.
#'   
#' @usage
#' smoothed.part(object)
#' @param object a twoStepsBenchmark object.
#' @return
#' a time-serie
#' @examples
#' benchmark <- twoStepsBenchmark(turnover,construction); smoothed.part(benchmark)
#'
#' @export
smoothed.part <- function(object) UseMethod("smoothed.part")
#' @export
smoothed.part.twoStepsBenchmark <- function(object) object$smoothed.part

#' @export
se.twoStepsBenchmark <- function(object) se(prais(object))

#' @export
rho.twoStepsBenchmark <- function(object) rho(prais(object))

#' @export
print.twoStepsBenchmark <- function(x,...) {
  print(prais(x),...)
  print(as.ts(x))
  invisible(x)
}

#' @export
summary.twoStepsBenchmark <- function(object, ...) {
  summary.praislm(prais(object),...)
}