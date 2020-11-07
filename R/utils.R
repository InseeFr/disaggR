ts_from_tsp <- function(x,tspx) {
  ts(x, start=tspx[1L], frequency=tspx[3L])
}

drop_tsp <- function(x) {
  attr(x,"tsp") <- NULL
  class(x) <- setdiff(class(x),c("mts","ts"))
  x
}

ts_expand <- function(x,nfrequency,divide.by.ratio=TRUE){
  ratio <- nfrequency/frequency(x)
  res <- if (divide.by.ratio) x/ratio else x
  ts(rep(res, each = ratio), start = tsp(x)[1], frequency = nfrequency)
}

## Behind these are speedup routines that overrides base/stats methods and work well for the package
## They can be removed if the base implementation become faster

window <- function(x,start=NA_real_,end=NA_real_,...) {
  if (is.null(start)) start <- NA_real_
  if (is.null(end)) end <- NA_real_
  .Call("Cpp_window", PACKAGE = "disaggR", x, getOption("ts.eps"), start, end)
}

fast_op <- function(e1,e2,FUN) {
  FUN <- match.fun(FUN)
  tsp1 <- tsp(e1)
  tsp2 <- tsp(e2)
  if (is.null(tsp1)) {
    if (length(e1) != 1L) stop("This method only deals with two time-series", call. = FALSE)
    e1 <- ts_from_tsp(rep(e1,length(e2)),tsp2)
    tsp1 <- tsp2
  }
  if (is.null(tsp2)) {
    if (length(e2) != 1L) stop("This method only deals with two time-series", call. = FALSE)
    e2 <- ts_from_tsp(rep(e2,length(e1)),tsp1)
    tsp2 <- tsp1
  }
  if (tsp1[3L] != tsp2[3L]) stop("not all series have the same frequency")
  if (any(abs(tsp1[-3L] - tsp2[-3L]) > getOption("ts.eps"))) {
    start <- max(tsp1[1L],tsp2[1L])
    end <- min(tsp1[2L],tsp2[2L])
    e1 <- window(e1,start=start,end=end,extend=TRUE)
    e2 <- window(e2,start=start,end=end,extend=TRUE)
  }
  structure(FUN(drop_tsp(e1),drop_tsp(e2)), tsp=tsp(e1), class="ts")
}

`Ops.ts` <- function(e1,e2) fast_op(e1,e2,match.fun(.Generic))