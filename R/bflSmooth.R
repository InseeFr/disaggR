stairs_diagonal <- function(A,ratio,weights=1) {
  res <- matrix(0,A,ratio*A)
  res[matrix(c(rep(1L:A,each=ratio),
               seq_len(ncol(res))),
             ncol = 2L)] <- weights
  res
}

weights_control <- function(weights,start,hf_length,hf_freq) {
  if (is.null(weights)) return()
  if (!inherits(weights,"ts")) stop("The weights must be either NULL or a one-dimensional ts with the same window than the expected high-frequency serie", call. = FALSE)
  if (!is.null(dim(weights)) && dim(weights)[2L] != 1L) stop("The weights serie must be one-dimensional", call. = FALSE)
  tspw <- tsp(weights)
  if (tspw[3L] != hf_freq) stop("The frequency of the weights must be the same than the new frequency", call. = FALSE)
  if (abs(tspw[1L] - start) > getOption("ts.eps")) stop("The weights serie must have the same start than the expected high-frequency serie", call. = FALSE)
  if (length(weights) != hf_length) stop("The weights serie must have the same end than the expected high-frequency serie", call. = FALSE)
  return()
}

bflSmooth_matrices_impl <- function(lf_length,ratio,weights,lfserie.is.rate) {
  if (is.null(weights)) weights <- 1
  else {
    aggregated_weights <- aggregate.ts(weights,frequency(weights)/ratio)
    weights <- weights/ts_expand(aggregated_weights,
                                 frequency(weights),divide.by.ratio = FALSE)
  }
  
  MT <- t(apply(stairs_diagonal(lf_length,ratio,weights),1,function(x) rev(cumsum(rev(x)))))
  m1 <- MT[,1L]
  tildem <- MT[,-1L,drop=FALSE]
  inversemm <- solve((tcrossprod(tildem)))
  cprod1 <- crossprod(m1,inversemm)
  cprod2 <- crossprod(tildem,inversemm)
  
  # A * as.numeric(x) stands for diag(x) %*% A
  # t(t(A) * as.numeric(x)) stands for A %*% diag(x)
  
  if (lfserie.is.rate||identical(weights,1)) {
    list(m1=m1,
         cprod1=cprod1,
         cprod2=cprod2)
  } else {
    list(m1=m1*as.numeric(aggregated_weights),
         cprod1=t(t(cprod1)/as.numeric(aggregated_weights)),
         cprod2=t(t(cprod2)/as.numeric(aggregated_weights)))
  }
}

#' Generating a clone for bflSmooth_matrices_impl
#' 
#' This *function factory* returns a clone of bflSmooth_matrices_impl that gives
#' the same results than the original function but uses cache, which is useful
#' considering people would often use a lot of similar calls.
#' 
#' bflSmooth_matrices_factory is only run at build time.
#' 
#' @keywords internal
bflSmooth_matrices_factory <- function(cache_size=100L) {
  cache <- vector("list",cache_size)
  cache_next <- 1L
  function(lf_length,ratio,weights,lfserie.is.rate) {
    args <- list(lf_length,ratio,weights,lfserie.is.rate)
    cached_index <- Position(function(x) identical(x$args,args),cache)
    if (is.na(cached_index)) {
      value <- bflSmooth_matrices_impl(lf_length,ratio,weights,lfserie.is.rate)
      cache[[cache_next]] <<- list(args=args,
                                   value=value)
      cache_next <<- cache_next %% cache_size + 1L
      value
    }
    else cache[[cached_index]]$value
  }
}

bflSmooth_matrices <- bflSmooth_matrices_factory()

#' Smooth a time serie
#' 
#' bflSmooth smoothes a time-serie into a time serie of a higher frequency that
#' exactly aggregates into the higher one. The process followed is Boot, Feibes
#' and Lisman, which minimizes the squares of the variations.
#' 
#' If `weights` isn't `NULL` the results depends of `lfserie.is.rate` :
#' 
#' * if `FALSE` the rate output/weights is smoothed with the constraint that the
#' aggregated output is equal to the input lfserie.
#' * if `TRUE` the input lfserie is the rate to be smoothed, with the constraint
#' that the low-frequency weighted means of the output are equal to
#' lfserie.
#' 
#' @param lfserie a time-serie to be smoothed
#' @param nfrequency the new high frequency. It must be a multiple of the low
#' frequency.
#' @param weights NULL or a time-serie of the same size than the expected
#' high-frequency serie.
#' @param lfserie.is.rate TRUE or FALSE. only means a thing if weights isn't
#' NULL.
#' 
#' @return A time serie of frequency nfrequency
#' 
#' @export
bflSmooth <- function(lfserie,nfrequency,weights=NULL,lfserie.is.rate=FALSE) {
  if (!inherits(lfserie,"ts")) stop("Not a ts object", call. = FALSE)
  tsplf <- tsp(lfserie)
  if (as.integer(tsplf[3L]) != tsplf[3L]) stop("The frequency of the smoothed serie must be an integer", call. = FALSE)
  if (nfrequency == 0) stop("The new frequency must be strictly positive", call. = FALSE)
  if (nfrequency %% tsplf[3L] != 0L) stop("The new frequency must be a multiple of the lower one", call. = FALSE)
  if (!is.null(dim(lfserie)) && dim(lfserie)[2L] != 1) stop("The low frequency serie must be one-dimensional", call. = FALSE)
  if (is.null(weights) && lfserie.is.rate) {
    warning("weights is NULL. Ignoring lfserie.is.rate",call. = FALSE)
    lfserie.is.rate <- FALSE
  }
  
  ratio <- nfrequency/tsplf[3L]
  
  weights_control(weights,tsplf[1L],ratio*length(lfserie),nfrequency)
  
  if (nfrequency == tsplf[3L]) return(lfserie)
  
  matrices <- bflSmooth_matrices(lf_length = length(lfserie),
                                 ratio = nfrequency/tsplf[3L],
                                 weights = weights,
                                 lfserie.is.rate)
  
  x11 <- as.numeric(matrices$cprod1 %*% lfserie/(matrices$cprod1 %*% matrices$m1))
  res <- cumsum(c(x11,matrices$cprod2 %*% (as.numeric(lfserie)-matrices$m1*x11)))
  if (!lfserie.is.rate && !is.null(weights)) res <- res * as.numeric(weights)
  ts(res,start=tsplf[1L],frequency = nfrequency)
}
