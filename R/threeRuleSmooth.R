# threeRuleSmooth is registered as a minimal S4 class in order to allow
# Ops group generic double dispatch with ts. Through, most of the package uses
# S3 methods for simplicity
#' @import methods
#' @export
setClass("threeRuleSmooth",contains = "list")

#' Extrapolation function for the hfserie in a threeRuleSmooth
#' 
#' This function replaces the incomplete low frequency cycles, at the start and the end of the hfserie,
#' with respectively the first and the last complete cycles.
#' It may seem very raw, but it's only used for the weights in `bflSmooth`, in order to get a high
#' frequency rate.
#'
#' @param hfserie a time-serie, the high frequency serie to extrapolate
#' @param lffreq a integer of length 1. The low frequency
#'
#' @return a time-serie, the extrapolated hfserie
#' @export
hfserie_extrap <- function(hfserie,lffreq) {
  ratio <- frequency(hfserie)/lffreq
  valplaces <- which(!is.na(hfserie))
  if (length(valplaces) != 0) {
    firstval <- valplaces[1L]
    lastval <- valplaces[length(valplaces)]
    if (lastval != length(hfserie)) {
      incomplete_cycle_start <- lastval %/% ratio * ratio + 1L
      hfserie[incomplete_cycle_start:length(hfserie)] <- hfserie[(incomplete_cycle_start-ratio):(incomplete_cycle_start-1L)]
    }
    if (firstval != 1L) {
      incomplete_cycle_end <- (firstval - 2L) %/% ratio * ratio + ratio
      hfserie[1L:incomplete_cycle_end] <- hfserie[(incomplete_cycle_end+1L):(incomplete_cycle_end+ratio)]
    }
  }
  hfserie
}

calc_hfserie_win <- function(hfserie,start.domain,end.domain,lffreq) {
  
  hfserie <- window(hfserie,start=start.domain,end=end.domain,extend=TRUE)
  
  tsphf <- tsp(hfserie)
  
  startdomain_extended <- floor(tsphf[1L]*lffreq)/lffreq
  enddomain_extended <- ceiling((tsphf[2L]+1/tsphf[3L])*lffreq)/lffreq-1/tsphf[3L]
  
  # This window is the smallest that is all around the domain of the hfserie
  # that is compatible with the low frequency.
  
  hfserie_extrap(window(hfserie,
                        start = startdomain_extended,
                        end = enddomain_extended,extend = TRUE),
                 lffreq)
}

mean_delta <- function(serie,start,end) {
  mean(diff(as.numeric(window(serie,start=start,end=end,extend=TRUE))),na.rm = TRUE)
}

rate_extrap <- function(lfrate,mean.delta) {
  valplaces <- which(!is.na(lfrate))
  if (length(valplaces) != 0) {
    firstval <- valplaces[1L]
    lastval <- valplaces[length(valplaces)]
    if (lastval != length(lfrate)) {
      lfrate[(lastval+1L):length(lfrate)] <- lfrate[lastval] + 1:(length(lfrate)-lastval) * mean.delta
    }
    if (firstval != 1L) {
      lfrate[(firstval-1L):1L] <- lfrate[firstval] - 1:(firstval-1L) * mean.delta
    }
  }
  lfrate
}

calc_lfrate_win <- function(hfserie,lfserie,
                            start.benchmark,end.benchmark,
                            start.delta.rate,end.delta.rate,
                            set.delta.rate,
                            start.domain.extended,end.domain.extended) {
  
  lfrate <- lfserie / aggregate_and_crop_hf_to_lf(hfserie,lfserie)
  
  delta_rate <- {
    if (is.null(set.delta.rate)) mean_delta(lfrate,start.delta.rate,end.delta.rate)
    else set.delta.rate
  }
  
  list(
    lfrate = rate_extrap(
      window(
        window(lfrate,
               start  = start.benchmark,
               end    = end.benchmark,
               extend = TRUE),
        start  = start.domain.extended,
        end    = end.domain.extended,
        extend = TRUE),
      delta_rate),
    delta_rate = delta_rate)
}

threeRuleSmooth_impl <- function(hfserie,lfserie,
                                 start.benchmark,end.benchmark,
                                 start.domain,end.domain,
                                 start.delta.rate,end.delta.rate,
                                 set.delta.rate,
                                 maincl,cl=NULL) {
  
  if (is.null(cl)) cl <- maincl
  
  hfserie_win <- calc_hfserie_win(hfserie,
                                  start.domain,end.domain,
                                  frequency(lfserie))
  
  lfrate_win <- calc_lfrate_win(hfserie,lfserie,
                                start.benchmark,end.benchmark,
                                start.delta.rate,end.delta.rate,
                                set.delta.rate,
                                tsp(hfserie_win)[1L],tsp(hfserie_win)[2L])
  
  hfrate <- bflSmooth(lfserie = lfrate_win$lfrate,
                      nfrequency = frequency(hfserie),
                      weights = hfserie_win,
                      lfserie.is.rate = TRUE)
  
  rests <- hfrate * hfserie
  
  res <- list(benchmarked.serie = window(rests,start=start.domain,end=end.domain,extend = TRUE),
              lfrate = lfrate_win$lfrate,
              smoothed.rate = hfrate,
              hfserie.as.weights = hfserie_win,
              delta.rate = lfrate_win$delta_rate,
              model.list = list(hfserie = hfserie,
                                lfserie = lfserie,
                                start.benchmark = start.benchmark,
                                end.benchmark = end.benchmark,
                                start.domain = start.domain,
                                end.domain = end.domain,
                                start.delta.rate = start.delta.rate,
                                end.delta.rate = end.delta.rate,
                                set.delta.rate = set.delta.rate),
              call = cl)
  
  new("threeRuleSmooth",res)
}

#' @title Bends a time-serie with a lower frequency one by smoothing their rate
#' 
#' @description threeRuleSmooth bends a time-serie with a time-serie of a lower
#' frequency. The procedure involved is a proportional Denton benchmark.
#' 
#' Therefore, the resulting time-serie is the product of the high-frequency input
#' with a smoothed rate. This latter is extrapolated using an arithmetic sequence.
#' 
#' The resulting time-serie is equal to the low-frequency serie after aggregation
#' within the benchmark window.
#' 
#' @details In order to smooth the rate, threeRuleSmooth calls \link{bflSmooth}
#' and uses a modified and extrapolated version of hfserie as weights :
#' 
#' * only the full cycles are kept
#' * the first and last full cycles are replicated respectively backwards and
#' forwards to fill the domain window.
#' 
#' @aliases threeRuleSmooth-class
#' Ops,threeRuleSmooth,ts-method Ops,ts,threeRuleSmooth-method
#' Math2,threeRuleSmooth-method
#' show,threeRuleSmooth-method
#' @param hfserie the bended time-serie. It can be a matrix time-serie.
#' @param lfserie a time-serie whose frequency divides the frequency of `hfserie`.
#' @param start.benchmark an optional start for `lfserie` to bend `hfserie`.
#' Should be a double or a numeric of length 2, like a window for `lfserie`. If NULL, the start is defined by lfserie's window.
#' @param end.benchmark an optional end for `lfserie` to bend `hfserie`.
#' Should be a double or a numeric of length 2, like a window for `lfserie`. If NULL, the start is defined by lfserie's window.
#' @param start.domain an optional start of the output high-frequency serie. It also defines the smoothing window :
#' The low-frequency residuals will be extrapolated until they contain the smallest low-frequency window that is around the high-frequency
#' domain window.
#' Should be a double or a numeric of length 2, like a window for `hfserie`. If NULL, the start is defined by hfserie's window.
#' @param end.domain an optional end of the output high-frequency serie. It also defines the smoothing window :
#' The low-frequency residuals will be extrapolated until they contain the smallest low-frequency window that is around the high-frequency
#' domain window.
#' @param start.delta.rate an optional start for the mean of the rate difference
#' required for the arithmetical extrapolation of the rate.
#' Should be a double or a numeric of length 2, like a window for `lfserie`. If NULL, the start is defined by lfserie's window.
#' @param end.delta.rate an optional end for the mean of the rate difference
#' required for the arithmetical extrapolation of the rate.
#' Should be a double or a numeric of length 2, like a window for `lfserie`. If NULL, the end is defined by lfserie's window.
#' @param set.delta.rate  an optional double, that allows the user to set the
#' delta mean instead of using a mean.
#' @param \dots if the dots contain a cl item, its value overwrites the value
#' of the returned call. This feature allows to build wrappers.
#' @return
#' threeRuleSmooth returns an object of class "`threeRuleSmooth`".
#' 
#' The functions `plot` and `autoplot` (the generic from \pkg{ggplot2}) produce graphics of the benchmarked
#' serie and the bending serie.
#' The functions \link{in_disaggr}, \link{in_revisions},
#' \link{in_scatter} produces various comparisons on which plot and autoplot can also
#' be used.
#' 
#' The generic accessor functions `as.ts`, `model.list`, `smoothed.rate` extract
#' various useful features of the returned value.
#' 
#' An object of class "`threeRuleSmooth`" is a list containing the following components :
#'   \item{benchmarked.serie}{a time-serie, that is the result of the benchmark.}
#'   \item{lfrate}{a time-serie, that is the low-frequency rate of the threeRuleSmooth.}
#'   \item{smoothed.rate}{the smoothed rate of the threeRuleSmooth.}
#'   \item{hfserie.as.weights}{the modified and extrapolated hfserie (see details)}
#'   \item{delta.rate}{the low-frequency delta of the rate, used to extrapolate
#'   the rate time-serie. It is estimated as the mean value in the specified window.}
#'   \item{model.list}{a list containing all the arguments submitted to the function.}
#'   \item{call}{the matched call (either of twoStepsBenchmark or annualBenchmark)}
#' @examples
#' 
#' ## How to use threeRuleSmooth
#' 
#' smooth <- threeRuleSmooth(hfserie = turnover,
#'                           lfserie = construction)
#' as.ts(smooth)
#' coef(smooth)
#' summary(smooth)
#' library(ggplot2)
#' autoplot(in_disaggr(smooth))
#' 
#' @export
threeRuleSmooth <- function(hfserie,lfserie,
                            start.benchmark=NULL,end.benchmark=NULL,
                            start.domain=NULL,end.domain=NULL,
                            start.delta.rate=start.benchmark,end.delta.rate=end.benchmark,
                            set.delta.rate=NULL,
                            ...) {
  
  if ( !is.ts(lfserie) || !is.ts(hfserie) ) stop("Not a ts object", call. = FALSE)
  tsplf <- tsp(lfserie)
  if (as.integer(tsplf[3L]) != tsplf[3L]) stop("The frequency of the smoothed serie must be an integer", call. = FALSE)
  if  (!(frequency(hfserie) %% frequency(lfserie) == 0L)) stop("The low frequency should divide the higher one", call. = FALSE)
  if (!is.null(dim(lfserie)) && dim(lfserie)[2L] != 1) stop("The low frequency serie must be one-dimensional", call. = FALSE)
  if (!is.null(dim(hfserie)) && dim(hfserie)[2L] != 1) stop("The high frequency serie must be one-dimensional", call. = FALSE)
  
  maincl <- match.call()
  
  threeRuleSmooth_impl(hfserie,lfserie,
                       start.benchmark,end.benchmark,
                       start.domain,end.domain,
                       start.delta.rate,end.delta.rate,
                       set.delta.rate,
                       maincl,...)
}
