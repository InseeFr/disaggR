tsExpand <- function(x,nfrequency){
  ratio <- nfrequency/frequency(x)
  if (ratio!=as.integer(ratio)) stop("The new frequency must be a multiple of the lower one")
  ts(rep(x/ratio, each = ratio), start = tsp(x)[1], freq = nfrequency)
}

#' @importFrom graphics plot
#' @export
plot.twoStepsBenchmark <- function (object) {
  model <- model.list(object)
  x <- as.ts(object)
  ts.plot(x)
  points(tsExpand(model$lfserie,nfrequency = frequency(model$hfserie)),cex=0.25,pch=20)
  return(invisible(NULL))
}

#' @export
plot.insamplepraislm <- function (object) {
  ts.plot(insample(bn),gpars=list(lty=c(1L,2L)))
  return(invisible(NULL))
}