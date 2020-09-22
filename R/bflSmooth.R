stairs_diagonal <- function(A,ratio,weights=1) {
  res <- matrix(0,A,ratio*A)
  res[matrix(c(rep(1:A,each=ratio),
               seq_len(ncol(res))),
             ncol = 2)] <- weights
  return(res)
}

weights_control_shaping <- function(weights,start,n,nfrequency) {
  if (is.null(weights)) return(1)
  if (inherits(weights,"ts")) {
    if (!is.null(dim(weights)) && dim(weights)[2] != 1) stop("The weights serie must be one-dimensional", call. = FALSE)
    tspw <- tsp(weights)
    if (tspw[3] != nfrequency) stop("The frequency of the weights must be the same than the new frequency", call. = FALSE)
    if (tspw[1] != start) stop("The weights serie must have the same start than the expected high-frequency serie", call. = FALSE)
    if (length(weights) != n) stop("The weights serie must have the same end than the expected high-frequency serie", call. = FALSE)
    return(weights)
  }
  if !is.nu
}

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
#' 
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
  
  n <- ratio*length(lfserie)
  
  weights <- weights_control_shaping(weights)
  
  Tmat <- lower.tri(matrix(1,n,n),diag=TRUE)
  MT <- stairs_diagonal(length(lfserie),ratio,weights) %*% Tmat
  m1 <- MT[,1]
  tildem <- MT[,-1,drop=FALSE]
  inversemm <- solve((tcrossprod(tildem)))
  x11 <- as.numeric((crossprod(m1,inversemm) %*% lfserie)/(crossprod(m1,inversemm)%*%m1))
  primey <- c(x11,crossprod(tildem,inversemm %*% (lfserie-m1*x11)))
  return(ts(as.numeric(Tmat %*% primey),start=tsplf[1],frequency = nfrequency))
  return(bflSmooth_impl(lfserie,nfrequency,weights=1))
}