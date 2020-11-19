if (R.version$major>=4) { # before that no support for the fractional ts
  test_that("window", {
    set.seed(10)
    frequencyx <- c(sample(1:40,500,replace = TRUE))
    datax <- lapply(vapply(sample(1:50,500,replace = TRUE),identity,1),rnorm)
    startx <- rnorm(500,0,sd = 100)
    series <- Map(ts,datax,start=startx,frequency=abs(frequencyx))
    start <- rnorm(500,0,sd = 100)
    end <- rnorm(500,0,sd = 100)
    
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
    expect_true(isTRUE(all.equal(statsw,statsd)))
    
    start <- Map(c,floor(rnorm(500,0,sd = 100)),sample.int(40,size = 500,replace=TRUE))
    end <- Map(c,floor(rnorm(500,0,sd = 100)),sample.int(40,size = 500,replace=TRUE))
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
    
    expect_true(isTRUE(all.equal(statsw,statsd)))
    
    frequencyx <- c(sample(1:40,500,replace = TRUE))
    datax <- lapply(vapply(sample(1:50,500,replace = TRUE),identity,1),
                    function(n) matrix(rnorm(n),rnorm(n),nrow=n,ncol=2))
    startx <- rnorm(500,0,sd = 100)
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
    expect_true(isTRUE(all.equal(statsw,statsd)))
    
    start <- Map(c,floor(rnorm(500,0,sd = 100)),sample.int(40,size = 500,replace=TRUE))
    end <- Map(c,floor(rnorm(500,0,sd = 100)),sample.int(40,size = 500,replace=TRUE))
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
    expect_true(isTRUE(all.equal(statsw,statsd)))
    
    expect_error(disaggR:::window(NULL))
    expect_error(disaggR:::window(NA))
    expect_error(disaggR:::window("a"))
    expect_error(disaggR:::window(ts(1:10),start="a"))
    expect_error(disaggR:::window(ts(1:10),end="a"))
    expect_error(disaggR:::window(ts(1:10),start=c(1,1,1)))
    expect_error(disaggR:::window(ts(1:10),start=numeric()))
    expect_error(disaggR:::window(ts(1:10),end=c(1,1,1)))
    expect_error(disaggR:::window(ts(1:10),end=numeric()))
  })
}

test_that("window degenerates" {
  verysmall <- getOption("ts.eps")/24
  data <- rnorm(1:13)
  expect_identical(disaggR::window(ts(data,start=2010,freq=12),end=2010+verysmall),
                   stats::window(ts(data,start=2010,freq=12),end=2010+verysmall))
  expect_error(disaggR::window(ts(data,start=2010,freq=12),end=2010-verysmall))
  expect_identical(disaggR::window(ts(data,start=2010,freq=12),start=2011-verysmall),
                   stats::window(ts(data,start=2010,freq=12),start=2011-verysmall))
  expect_error(disaggR::window(ts(data,start=2010,freq=12),start=2011+verysmall))
  expect_error(disaggR::window(ts(data,start=2010,freq=12),start=2010+verysmall,end=2010-verysmall))
  expect_identical(disaggR::window(ts(data,start=2010,freq=12),start=2010-verysmall,end=2010+verysmall),
                   stats::window(ts(data,start=2010,freq=12),start=2010-verysmall,end=2010+verysmall))
  
  small <- getOption("ts.eps")/2
  
  expect_identical(disaggR::window(ts(data,start=2010,freq=12+small),start=2010,end=2010),
                   stats::window(ts(data,start=2010,freq=12+small),start=2010,end=2010))
  expect_identical(disaggR::window(ts(data,start=2010,freq=12-small),start=2010,end=2010),
                   stats::window(ts(data,start=2010,freq=12-small),start=2010,end=2010))
  
})

test_that("fastop on matrix",{
  mts1 <- ts(matrix(1:60,ncol=3),start=2009,freq=12)
  mts2 <-ts(matrix(2:13,ncol = 3),start=2010,freq=12)
  summts <- fast_op(mts1,mts2,`+`)
  expect_identical(summts,
                   ts(matrix(c(15, 17, 19,21,
                               39, 41, 43, 45,
                               63, 65, 67, 69),ncol=3),
                      start=2010,frequency=12))
})

test_that("fastop errors",{
  expect_error(disaggR:::fast_op(ts(1:10),1:10,`+`),"two time-series")
  expect_error(disaggR:::fast_op(1:10,ts(1:10),`+`),"two time-series")
  expect_error(disaggR:::fast_op(ts(1:10),ts(1:10,freq=4),`+`),"same frequency")
})