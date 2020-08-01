#' @importFrom graphics plot points
#' @export
plot.twoStepsBenchmark <- function(x, xlab = "", ylab = "", ...) {
  model <- model.list(x)
  plot(na.omit(as.ts(x)), xlab = xlab, ylab = ylab, ...)
  points(tsExpand(model$lfserie,nfrequency = frequency(model$hfserie)),cex=0.25,pch=20)
  return(invisible(NULL))
}

#' @export
plot.insample <- function(x, xlab="", ylab="", ...) {
  class(x) <- class(x)[-1]
  plot(x, plot.type="single", lty=c(1L,2L), xlab = xlab, ylab = ylab,
       main = paste0("In-sample predictions (", attr(x,"type"),")"), ...)
  return(invisible(NULL))
}