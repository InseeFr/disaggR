ts_from_tsp <- function(x,tspx) {
  ts(x, start=tspx[1L], frequency=tspx[3L])
}

purify_ts <- function(x) {
  s <- start(x)
  if (length(s) == 2L) ts(x,start = s, frequency = frequency(x))
  else stop("Incorrect time-serie phase", call. = FALSE)
}

tsp_equal <- function(tspx,tspy) {
  
  ts.eps <- getOption("ts.eps")
  
  tspx[c(1L,2L)] <- tspx[c(1L,2L)] * tspx[3L]
  tspy[c(1L,2L)] <- tspy[c(1L,2L)] * tspy[3L]
  
  all(abs(tspx - tspy) < ts.eps)
  
}

# This window is the smallest that is all around the domain of the hfserie
# that is compatible with the low frequency.
extend_tsp <- function(tsphf,lffreq) {
  
  ts.eps <- getOption("ts.eps")
  
  if (is.null(tsphf) || is.null(lffreq)) return(NULL)
  
  c(floor(tsphf[1L]*lffreq+ts.eps)/lffreq,
    ceiling((tsphf[2L]+1/tsphf[3L])*lffreq-ts.eps)/lffreq-1/tsphf[3L],
    tsphf[3L])
}

aggregate_and_crop_hf_to_lf <- function(hfserie,lfserie) {
  tsplf <- tsp(lfserie)
  aggregate.ts(
    window(hfserie,tsplf[1L],tsplf[2L]+1/tsplf[3L]-1/frequency(hfserie),extend = TRUE),
    nfrequency = tsplf[3L]
  )
}

ts_expand <- function(x,nfrequency,divide.by.ratio=TRUE) {
  ratio <- nfrequency/frequency(x)
  res <- if (divide.by.ratio) x/ratio else x
  ts(rep(res, each = ratio), start = tsp(x)[1], frequency = nfrequency)
}

switch_window <- function(start,end,init_tsp) {
  start <- {
    if (is.null(start)) init_tsp[1L]
    else switch(length(start),
                start,
                start[1L] + (start[2L] - 1)/init_tsp[3L])
  }
  end <- {
    if (is.null(end)) init_tsp[2L]
    else switch(length(end),
                end,
                end[1L] + (end[2L] - 1)/init_tsp[3L])
  }
  c(start,end)
}

lfserie <- function(benchmark) model.list(benchmark)$lfserie
hfserie <- function(benchmark) {
  res <- model.list(benchmark)$hfserie
  if (is.mts(res)) res[,colnames(res) != "constant"] else res
}

