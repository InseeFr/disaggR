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

tsExpand <- function(x,nfrequency){
  ratio <- nfrequency/frequency(x)
  if (ratio!=as.integer(ratio)) stop("The new frequency must be a multiple of the lower one")
  ts(rep(x/ratio, each = ratio), start = tsp(x)[1], freq = nfrequency)
}

#' @importFrom graphics plot
#' @export
plot.twoStepsBenchmark <- function (x) {
  model <- model.list(x)
  x <- as.ts(x)
  rangex <- range(time(x)[!is.na(x)])
  rangex <- c(floor(rangex)[1],ceiling(rangex)[2])
  rangey <- range(x, na.rm = TRUE)
  plot(x = rangex, y = rangey, type = "n",xaxt="n",xlab = "",ylab="")
  axis(side = 1, at = rangex[1]:rangex[2] + 0.5, labels = rangex[1]:rangex[2],tick = FALSE)
  points(tsExpand(model$lfserie,nfrequency = frequency(model$hfserie)),cex=0.25,pch=20)
  lines(x)
  return(invisible(NULL))
}

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