test_that("window", {
  set.seed(1)
  frequencyx <- c(sample(1:40,100,replace = TRUE),rnorm(100,10,10))
  datax <- lapply(vapply(sample(1:50,200,replace = TRUE),identity,1),rnorm)
  startx <- rnorm(200,0,sd = 100)
  series <- Map(ts,datax,start=startx,frequency=abs(frequencyx))
  start <- rnorm(200,0,sd = 100)
  end <- rnorm(200,0,sd = 100)

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
  expect_true(do.call(all,Map(all.equal,statsw,statsd)))
  
  start <- Map(c,floor(rnorm(200,0,sd = 100)),sample.int(40))
  end <- Map(c,floor(rnorm(200,0,sd = 100)),sample.int(40))
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
  expect_true(do.call(all,Map(all.equal,statsw,statsd)))
  
  frequencyx <- c(sample(1:40,100,replace = TRUE),rnorm(100,10,10))
  datax <- lapply(vapply(sample(1:50,200,replace = TRUE),identity,1),
                  function(n) matrix(rnorm(n),rnorm(n),nrow=n,ncol=2))
  startx <- rnorm(200,0,sd = 100)
  series <- Map(ts,datax,start=startx,frequency=abs(frequencyx))
  
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
  expect_true(do.call(all,Map(all.equal,statsw,statsd)))
  
  start <- Map(c,floor(rnorm(200,0,sd = 100)),sample.int(40))
  end <- Map(c,floor(rnorm(200,0,sd = 100)),sample.int(40))
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
  expect_true(do.call(all,Map(all.equal,statsw,statsd)))
  
  expect_error(disaggR:::window(NULL))
  expect_error(disaggR:::window(NA))
  expect_error(disaggR:::window("a"))
  expect_error(disaggR:::window(ts(1:10),start="a"))
  expect_error(disaggR:::window(ts(1:10),end="a"))
})
