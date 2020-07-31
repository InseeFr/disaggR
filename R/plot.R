#' @importFrom graphics plot points
#' @export
plot.twoStepsBenchmark <- function(x, ...) {
  model <- model.list(x)
  ts.plot(as.ts(x))
  points(tsExpand(model$lfserie,nfrequency = frequency(model$hfserie)),cex=0.25,pch=20)
  return(invisible(NULL))
}

#' @export
plot.insample <- function(x, ...) {
  ts.plot(x,gpars=list(lty=c(1L,2L)))
  return(invisible(NULL))
}