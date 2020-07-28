tsExpand <- function(x,nfrequency){
  ratio <- nfrequency/frequency(x)
  if (ratio!=as.integer(ratio)) stop("The new frequency must be a multiple of the lower one")
  ts(rep(x/ratio, each = ratio), start = tsp(x)[1], freq = nfrequency)
}

#' @importFrom graphics plot
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