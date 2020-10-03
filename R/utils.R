ts_from_tsp <- function(x,tspx) {
  structure(as.numeric(x), tsp=tspx, class="ts")
}

fast_op <- function(e1,e2,FUN) {
  FUN <- match.fun(FUN)
  if (any(abs(tsp(e1) - tsp(e2)) > getOption("ts.eps"))) stop("This method is only made for similar tsp", call. = FALSE)
  structure(FUN(as.numeric(e1),as.numeric(e2)), tsp=tsp(e1), class="ts")
}

`%+%` <- function(e1,e2) fast_op(e1,e2,`+`)
`%-%` <- function(e1,e2) fast_op(e1,e2,`-`)

ts_expand <- function(x,nfrequency,divide.by.ratio=TRUE){
  ratio <- nfrequency/frequency(x)
  res <- if (divide.by.ratio) x/ratio else x
  ts(rep(res, each = ratio), start = tsp(x)[1], frequency = nfrequency)
}