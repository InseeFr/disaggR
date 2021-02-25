ts_from_tsp <- function(x,tspx) {
  ts(x, start=tspx[1L], frequency=tspx[3L])
}

drop_tsp <- function(x) {
  attr(x,"tsp") <- NULL
  class(x) <- setdiff(class(x),c("mts","ts"))
  x
}

aggregate_and_crop_hf_to_lf <- function(hfserie,lfserie) {
  tsplf <- tsp(lfserie)
  aggregate.ts(
    window(hfserie,tsplf[1L],tsplf[2L]+1/tsplf[3L]-1/frequency(hfserie),extend = TRUE),
    nfrequency = tsplf[3L]
  )
}

ts_expand <- function(x,nfrequency,divide.by.ratio=TRUE){
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

