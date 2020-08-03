tsExpand <- function(x,nfrequency){
  ratio <- nfrequency/frequency(x)
  if (ratio!=as.integer(ratio)) stop("The new frequency must be a multiple of the lower one")
  ts(rep(x/ratio, each = ratio), start = tsp(x)[1], frequency = nfrequency)
}

#' @importFrom graphics plot points
#' @export
plot.twoStepsBenchmark <- function(x, xlab = "", ylab = "", ...) {
  model <- model.list(x)
  tsbench <- as.ts(x)
  lims <- c(floor(min(time(tsbench)[!is.na(tsbench)])),
            ceiling(max(time(tsbench)[!is.na(tsbench)])))
  plot(window(tsbench,start=lims[1],end=lims[2],extend=TRUE)
       , xlab = xlab, ylab = ylab, ...)
  points(tsExpand(model$lfserie,nfrequency = frequency(model$hfserie)),cex=0.25,pch=20)
  return(invisible(NULL))
}

#' @export
plot.insample <- function(x, xlab="", ylab="", ...) {
  class(x) <- class(x)[-1]
  lims <- c(floor(min(time(x)[!is.na(x[,1])|!is.na(x[,2])])),
            ceiling(max(time(x)[!is.na(x[,1])|!is.na(x[,2])])))
  plot(window(x,start=lims[1],end=lims[2],extend=TRUE), plot.type="single", lty=c(1L,2L), xlab = xlab, ylab = ylab,
       main = paste0("In-sample predictions (", attr(x,"type"),")"), ...)
  return(invisible(NULL))
}