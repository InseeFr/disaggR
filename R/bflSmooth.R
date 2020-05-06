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
  Cpp_bflSmooth(lfserie,nfrequency)
}