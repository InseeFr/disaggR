ts_from_tsp <- function(x,tspx) {
  ts(x, start=tspx[1L], frequency=tspx[3L])
}

tsp_equal <- function(tspx,tspy) {
  
  ts.eps <- getOption("ts.eps")
  
  tspx[c(1L,2L)] <- tspx[c(1L,2L)] * tspx[3L]
  tspy[c(1L,2L)] <- tspy[c(1L,2L)] * tspy[3L]
  
  all(abs(tspx - tspy) < ts.eps)
  
}

clean_tsp <- function(x) {
  s <- start(x)
  if (length(s) == 2L) ts(x,start = s, frequency = frequency(x))
  else if (as.integer(frequency(x)) != frequency(x)) stop("The frequencies must be integers", call. = FALSE)
  else stop("Incorrect time-serie phase", call. = FALSE)
}

drop_tsp <- function(x) {
  attr(x,"tsp") <- NULL
  class(x) <- setdiff(class(x),c("mts","ts"))
  x
}

fast_op_on_x <- function(x,y,FUN) {
  FUN <- match.fun(FUN)
  tsp <- tsp(x)
  ts_from_tsp(FUN(drop_tsp(x),
                  drop_tsp(window(y,start=tsp[1L],end=tsp[2L],extend=TRUE))),
              tspx = tsp)
}

fast_aggregate <- function(x,nfrequency) {
  dimx <- dim(x)
  tspx <- tsp(x)
  ratio <- tspx[3L]/nfrequency
  
  res <- .colSums(as.numeric(x),n = length(x)/ratio,m = ratio)
  
  if (is.null(dimx)) {
    ts(res,
       start = tspx[1L],
       frequency = nfrequency)
  } else {
    ts(matrix(res,ncol = dimx[2L]),
       start = tspx[1L],
       frequency = nfrequency,
       names = colnames(x))
  }
}

neither_outlier_nor_constant_impl <- function(hfserie, object) {
  hfserie[,!(colnames(hfserie) %in% c("constant",
                                      names(outliers(object)))),
          drop = FALSE]
}

neither_outlier_nor_constant <- function(object) UseMethod("neither_outlier_nor_constant")

neither_outlier_nor_constant.twoStepsBenchmark <- function(object) {
  neither_outlier_nor_constant_impl(model.list(object)$hfserie, object)
}

neither_outlier_nor_constant.threeRuleSmooth <- function(object) {
  model.list(object)$hfserie
}

neither_outlier_nor_constant.praislm <- function(object) {
  neither_outlier_nor_constant_impl(model.list(object)$X, object)
}

#' Extend tsp with lf
#'
#' This window is the smallest that is all around tsphf
#' that is compatible with the low frequency.
#' 
#' @param tsphf a numeric of length 3, a tsp of high-frequency
#' @param lffreq a numeric of length 1, the low frequency
#' @return
#' a numeric of length 3, a tsp of high-frequency.
#' @keywords internal
extend_tsp <- function(tsphf,lffreq) {
  
  ts.eps <- getOption("ts.eps")
  
  c(floor(tsphf[1L]*lffreq+ts.eps)/lffreq,
    ceiling((tsphf[2L]+1/tsphf[3L])*lffreq-ts.eps)/lffreq-1/tsphf[3L],
    tsphf[3L])
}

aggregate_and_crop_hf_to_lf <- function(hfserie,lfserie) {
  tsplf <- tsp(lfserie)
  fast_aggregate(
    window(hfserie,tsplf[1L],tsplf[2L]+1/tsplf[3L]-1/frequency(hfserie),extend = TRUE),
    nfrequency = tsplf[3L]
  )
}

ts_expand <- function(x,nfrequency,divide.by.ratio=TRUE) {
  ratio <- nfrequency/frequency(x)
  res <- if (divide.by.ratio) x/ratio else x
  ts(rep(res, each = ratio), start = tsp(x)[1L], frequency = nfrequency)
}

switch_window <- function(start,end,init_tsp) {
  c(switch(length(start),
           start,
           start[1L] + (start[2L] - 1)/init_tsp[3L]) %||%
      init_tsp[1L],
    switch(length(end),
           end,
           end[1L] + (end[2L] - 1)/init_tsp[3L]) %||%
      init_tsp[2L]
  )
}

`%||%` <- function(x,y) if (is.null(x)) y else x
