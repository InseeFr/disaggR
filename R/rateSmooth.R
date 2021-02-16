hfserie_extrap_function <- function(incomplete_cycle,complete_cycle) {
  complete_cycle * sum(incomplete_cycle/complete_cycle,na.rm=TRUE)
}

hfserie_extrap <- function(hfserie,lffreq) {
  ratio <- frequency(hfserie)/lffreq
  valplaces <- which(!is.na(hfserie))
  if (length(valplaces) != 0) {
    firstval <- valplaces[1L]
    lastval <- valplaces[length(valplaces)]
    if (lastval != length(hfserie)) {
      incomplete_cycle_start <- (lastval + 1L) %% ratio * ratio + 1L
      hfserie[incomplete_cycle_start:length(hfserie)] <- 
        hfserie_extrap_function(hfserie[(incomplete_cycle_start-ratio):(incomplete_cycle_start-1L)],
                                hfserie[incomplete_cycle_start:(incomplete_cycle_start+ratio-1L)])
    }
    if (firstval != 1L) {
      incomplete_cycle_end <- (firstval + 1L) %% ratio * ratio + 1L
      hfserie[(firstval-1):1] <-
        hfserie[1L:incomplete_cycle_end] <- 
        hfserie_extrap_function(hfserie[(incomplete_cycle_end-ratio + 1L):incomplete_cycle_end],
                                hfserie[(incomplete_cycle_end+1L):(incomplete_cycle_end+ratio)])
    }
  }
  hfserie
}

rateSmooth <- function(hfserie,lfserie,start.domain,end.domain) {
  tsphf <- tsp(hfserie)
  tsplf <- tsp(lfserie)
  
  hfserie <- window(hfserie,start=start.domain,end=end.domain)
  
  startdomain_extended <- floor(tsphf[1]*tsplf[3])/tsplf[3]
  enddomain_extended <- ceiling((tsphf[2]+1/tsphf[3])*tsplf[3])/tsplf[3]-1/tsphf[3]
  
  lfserie <- window(lfserie,start=startdomain_extended,end=enddomain_extended)
  hfserie <- window(hfserie,start=startdomain_extended,end=enddomain_extended)
  
  hfserie <- hfserie_extrap(hfserie,tsplf[3L])
}

