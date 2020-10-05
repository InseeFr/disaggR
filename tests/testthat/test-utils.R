test_that("window", {
  set.seed(1)
  frequencyx <- c(sample(1:40,1000,replace = TRUE),rnorm(1000,10,10))
  datax <- lapply(sample(1:400,2000,replace = TRUE),identity)
  startx <- rnorm(2000,0,sd = 100)
  series <- Map(ts,datax,start=startx,frequency=abs(frequencyx))
  start <- rnorm(2000,0,sd = 100)
  end <- rnorm(2000,0,sd = 100)
  win1 <- function(x,start,end) {
    list(tryCatch(stats::window(x,start,end,extend=TRUE), error = function(e) FALSE),
         tryCatch(stats::window(x,start=start,extend=TRUE), error = function(e) FALSE),
         tryCatch(stats::window(x,end=end,extend=TRUE), error = function(e) FALSE))
  }
  win2 <- function(x,start,end) {
    list(tryCatch(disaggR:::window(x,start,end), error = function(e) FALSE),
         tryCatch(disaggR:::window(x,start=start), error = function(e) FALSE),
         tryCatch(disaggR:::window(x,end=end), error = function(e) FALSE))
  }
  statsw <- Map(win1,series,start,end)
  statsd <- Map(win2,series,start,end)
  do.call(all,Map(all.equal,statsw,statsd))
})
