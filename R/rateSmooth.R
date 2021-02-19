hfserie_extrap_function <- function(incomplete_cycle,complete_cycle) {
  cumchange <- mean(incomplete_cycle/complete_cycle,na.rm=TRUE)
  if (is.na(cumchange)) complete_cycle
  else complete_cycle * cumchange
}

hfserie_extrap <- function(hfserie,lffreq) {
  ratio <- frequency(hfserie)/lffreq
  valplaces <- which(!is.na(hfserie))
  if (length(valplaces) != 0) {
    firstval <- valplaces[1L]
    lastval <- valplaces[length(valplaces)]
    if (lastval != length(hfserie)) {
      incomplete_cycle_start <- lastval %/% ratio * ratio + 1L
      hfserie[incomplete_cycle_start:length(hfserie)] <- 
        hfserie_extrap_function(hfserie[incomplete_cycle_start:(incomplete_cycle_start+ratio-1L)],
                                hfserie[(incomplete_cycle_start-ratio):(incomplete_cycle_start-1L)])
    }
    if (firstval != 1L) {
      incomplete_cycle_end <- (firstval - 1L) %/% ratio * ratio + ratio
      hfserie[1L:incomplete_cycle_end] <-
        hfserie_extrap_function(hfserie[(incomplete_cycle_end-ratio + 1L):incomplete_cycle_end],
                                hfserie[(incomplete_cycle_end+1L):(incomplete_cycle_end+ratio)])
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
  mean(diff(as.numeric(window(serie,start,end,extend=TRUE))),na.rm = TRUE)
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
                            start.mean.delta.rate,end.mean.delta.rate,
                            start.domain.extended,end.domain.extended) {
  
  lfrate <- lfserie / aggregate_and_crop_hf_to_lf(hfserie,lfserie)
  
  rate_extrap(
    window(
      window(lfrate,
             start = start.benchmark,
             end = end.benchmark,
             extend=TRUE),
      start = start.domain.extended,
      end = end.domain.extended,
      extend = TRUE),
    mean_delta(lfrate,start.mean.delta.rate,end.mean.delta.rate))
}

calc_hfrate <- function(hfserie,lfserie,
                        start.benchmark,end.benchmark,
                        start.domain,end.domain,
                        start.mean.delta.rate,end.mean.delta.rate) {
  
  hfserie_win <- calc_hfserie_win(hfserie,
                                  start.domain,end.domain,
                                  frequency(lfserie))
  
  lfrate_win <- calc_lfrate_win(hfserie,lfserie,
                                start.benchmark,end.benchmark,
                                start.mean.delta.rate,end.mean.delta.rate,
                                tsp(hfserie_win)[1L],tsp(hfserie_win)[2L])
  
  bflSmooth(lfserie = lfrate_win,
            nfrequency = frequency(hfserie),
            weights = hfserie_win,
            lfserie.is.rate = TRUE)
}

rateSmooth_impl <- function(hfserie,lfserie,
                            start.benchmark,end.benchmark,
                            start.domain,end.domain,
                            start.mean.delta.rate,end.mean.delta.rate,
                            maincl,cl=NULL) {
  
  if (is.null(cl)) cl <- maincl
  
  hfrate <- calc_hfrate(hfserie,lfserie,
                        start.benchmark,end.benchmark,
                        start.domain,end.domain,
                        start.mean.delta.rate,end.mean.delta.rate)
  
  rests <- hfrate * hfserie
  
  res <- list(benchmarked.serie = window(rests,start=start.domain,end=end.domain,extend = TRUE),
              rate = hfrate,
              model.list = list(hfserie = hfserie,
                                lfserie = lfserie,
                                start.benchmark = start.benchmark,
                                end.benchmark = end.benchmark,
                                start.domain = start.domain,
                                end.domain = end.domain,
                                start.mean.delta.rate = start.mean.delta.rate,
                                end.mean.delta.rate = end.mean.delta.rate),
              call = cl)
  
  class(res) <- c("rateSmooth","list")
  
  res
}

#' @export
rateSmooth <- function(hfserie,lfserie,
                       start.benchmark=NULL,end.benchmark=NULL,
                       start.domain=NULL,end.domain=NULL,
                       start.mean.delta.rate=start.benchmark,end.mean.delta.rate=end.benchmark,
                       ...) {
  
  if ( !is.ts(lfserie) || !is.ts(hfserie) ) stop("Not a ts object", call. = FALSE)
  tsplf <- tsp(lfserie)
  if (as.integer(tsplf[3L]) != tsplf[3L]) stop("The frequency of the smoothed serie must be an integer", call. = FALSE)
  if  (!(frequency(hfserie) %% frequency(lfserie) == 0L)) stop("The low frequency should divide the higher one", call. = FALSE)
  if (!is.null(dim(lfserie)) && dim(lfserie)[2L] != 1) stop("The low frequency serie must be one-dimensional", call. = FALSE)
  if (!is.null(dim(hfserie)) && dim(hfserie)[2L] != 1) stop("The high frequency serie must be one-dimensional", call. = FALSE)
  
  maincl <- match.call()
  
  rateSmooth_impl(hfserie,lfserie,
                  start.benchmark,end.benchmark,
                  start.domain,end.domain,
                  start.mean.delta.rate,end.mean.delta.rate,
                  maincl,...)
}
