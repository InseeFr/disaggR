tsExpand <- function(x,nfrequency,divide.by.ratio=TRUE){
  ratio <- nfrequency/frequency(x)
  res <- if (divide.by.ratio) x/ratio else x
  ts(rep(res, each = ratio), start = tsp(x)[1], frequency = nfrequency)
}

stairs_diagonal <- function(A,ratio,weights=1) {
  res <- matrix(0,A,ratio*A)
  res[matrix(c(rep(1:A,each=ratio),
               seq_len(ncol(res))),
             ncol = 2)] <- weights
  res
}

weights_control <- function(weights,start,hf_length,hf_freq) {
  if (is.null(weights)) return()
  if (!inherits(weights,"ts")) stop("The weights must be either NULL or a one-dimensional ts with the same window than the expected high-frequency serie", call. = FALSE)
  if (!is.null(dim(weights)) && dim(weights)[2] != 1) stop("The weights serie must be one-dimensional", call. = FALSE)
  tspw <- tsp(weights)
  if (tspw[3] != hf_freq) stop("The frequency of the weights must be the same than the new frequency", call. = FALSE)
  if (tspw[1] != start) stop("The weights serie must have the same start than the expected high-frequency serie", call. = FALSE)
  if (length(weights) != hf_length) stop("The weights serie must have the same end than the expected high-frequency serie", call. = FALSE)
  return()
}

bflSmooth_matrices_impl <- function(lf_length,ratio,weights) {
  weights <- {
    if (is.null(weights)) 1
    else weights/tsExpand(aggregate.ts(weights,frequency(weights)/ratio),
                          frequency(weights),divide.by.ratio = FALSE)
  }
  MT <- t(apply(stairs_diagonal(lf_length,ratio,weights),1,function(x) rev(cumsum(rev(x)))))
  m1 <- MT[,1]
  tildem <- MT[,-1,drop=FALSE]
  inversemm <- solve((tcrossprod(tildem)))
  cprod1 <- crossprod(m1,inversemm)
  cprod2 <- crossprod(tildem,inversemm)
  
  list(m1=m1,
       cprod1=cprod1,
       cprod2=cprod2)
}

# This function generates a wrapper of bflSmooth_matrices_impl that gives the
# same results but uses cache, which is useful considering a lot of similar
bflSmooth_matrices_generator <- function(cache_size=100L) {
  cache <- vector("list",cache_size)
  cache_next <- 1L
  function(lf_length,ratio,weights) {
    args <- list(lf_length,ratio,weights)
    cached_index <- Position(function(x) identical(x$args,args),cache)
    if (is.na(cached_index)) {
      value <- bflSmooth_matrices_impl(lf_length,ratio,weights)
      cache[[cache_next]] <<- list(args=args,
                                   value=value)
      cache_next <<- cache_next %% cache_size + 1L
      value
    }
    else cache[[cached_index]]$value
  }
}

bflSmooth_matrices <- bflSmooth_matrices_generator()

#' Smooth a time serie
#' 
#' bflSmooth smoothes a time-serie into a time serie of a higher frequency that exactly
#' aggregate into the higher one. The process followed is Boot, Feibes and Lisman
#' 
#' @author
#' Arnaud Feldmann
#' 
#' @param lfserie a time-serie to be smoothed
#' @param nfrequency the new high frequency. It must be a multiple of the low frequency.
#' @param weights NULL or a time-serie of the same size than the expected high-frequency serie.
#' The weights permits, by example, to smooth prices or rates relatively to a high-frequency
#' account. The results minimizes the squares of the variations, with the constraint that,
#' if multiplied by the weights then aggregated, they are equal to the low-frequency serie,
#' multiplied by the aggregated weigths.
#' @return A time serie of frequency nfrequency
#' 
#' @export
bflSmooth <- function(lfserie,nfrequency,weights=NULL) {
  if (!inherits(lfserie,"ts")) stop("Not a ts object", call. = FALSE)
  tsplf <- tsp(lfserie)
  if (as.integer(tsplf[3]) != tsplf[3]) stop("The frequency of the smoothed serie must be an integer", call. = FALSE)
  if (nfrequency==0) stop("The new frequency must be strictly positive", call. = FALSE)
  if (nfrequency==tsplf[3]) return(lfserie)
  if (nfrequency%%tsplf[3]!=0) stop("The new frequency must be a multiple of the lower one", call. = FALSE)
  if (!is.null(dim(lfserie)) && dim(lfserie)[2] != 1) stop("The low frequency serie must be one-dimensional", call. = FALSE)

  ratio <- nfrequency/tsplf[3]
  
  weights_control(weights,tsplf[1],ratio*length(lfserie),nfrequency)
  
  matrices <- bflSmooth_matrices(lf_length = length(lfserie),
                                 ratio = nfrequency/tsplf[3],
                                 weights = weights)
  
  x11 <- as.numeric(matrices$cprod1 %*% lfserie/(matrices$cprod1 %*% matrices$m1))
  res <- cumsum(c(x11,matrices$cprod2 %*% (lfserie-matrices$m1*x11)))
  ts(res,start=tsplf[1],frequency = nfrequency)
}