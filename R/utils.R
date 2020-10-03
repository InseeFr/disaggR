tsfromtsp <- function(x,tspx) {
  tsp(x) <- tspx
  class(x) <- "ts"
  x
}

ts_fastop <- function(x,y,FUN) {
  FUN <- match.fun(FUN)
  if (any(tsp(x) != tsp(y))) stop("This method is only made for similar tsp", call. = FALSE)
  tsfromtsp(FUN(as.numeric(x),as.numeric(y)),tsp(x))
}

tsExpand <- function(x,nfrequency,divide.by.ratio=TRUE){
  ratio <- nfrequency/frequency(x)
  res <- if (divide.by.ratio) x/ratio else x
  ts(rep(res, each = ratio), start = tsp(x)[1], frequency = nfrequency)
}