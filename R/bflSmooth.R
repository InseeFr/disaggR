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
bflSmooth <- function(lfserie,nfrequency) {
  if (!inherits(lfserie,"ts")) stop("Not a ts object", call. = FALSE)
  tspold <- tsp(lfserie)
  if (as.integer(tspold[3]) != tspold[3]) stop("The frequency of the smoothed serie must be an integer", call. = FALSE)
  if (nfrequency==0) stop("The new frequency must be strictly positive", call. = FALSE)
  if (nfrequency==tspold[3]) return(lfserie)
  if (nfrequency%%tspold[3]!=0) stop("The new frequency must be a multiple of the lower one", call. = FALSE)
  if (!is.null(dim(lfserie)) && dim(lfserie)[2] != 1) stop("The low frequency serie must be one-dimensional", call. = FALSE)
  
  ratio <- nfrequency/tspold[3]
  
  A <- length(lfserie)
  n <- ratio*A
  M <- matrix(rep(diag(A),each=ratio),A,n,byrow=TRUE)
  Tmat <- lower.tri(matrix(1,n,n),diag=TRUE)
  MT <- M %*% Tmat
  m1 <- MT[,1]
  tildem <- MT[,-1,drop=FALSE]
  inversemm <- solve((tcrossprod(tildem)))
  x11 <- as.numeric((crossprod(m1,inversemm)%*%lfserie)/(crossprod(m1,inversemm)%*%m1))
  lambda <- inversemm %*% (lfserie-m1*x11)
  primey <- c(x11,crossprod(tildem,lambda))
  x <- as.numeric(Tmat %*% primey)
  return(ts(x,start=tspold[1],frequency = nfrequency))
}