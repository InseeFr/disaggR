tsExpand <- function(x,nfrequency){
  ratio <- nfrequency/frequency(x)
  if (ratio!=as.integer(ratio)) stop("The new frequency must be a multiple of the lower one")
  ts(rep(x/ratio, each = ratio), start = tsp(x)[1], freq = nfrequency)
}
