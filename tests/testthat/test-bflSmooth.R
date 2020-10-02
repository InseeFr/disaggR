test_that("Only accepts ts", {
  expect_error(bflSmooth(1:10, 4))
})
test_that("Only accepts integer frequencies", {
  expect_error(bflSmooth(ts(1:10,freq=0.5), 1))
})
test_that("Only accepts strictly positive high frequencies", {
  expect_error(bflSmooth(ts(1:10,freq=1), 0))
})
test_that("Only accepts frequencies that are multiples of the lower one", {
  expect_error(bflSmooth(ts(1:10,freq=4), 5))
})
test_that("Only accepts one dimensional time-series", {
  expect_error(bflSmooth(ts(matrix(1:20,10,2),freq=4), 12))
})

test_that("Smoothing works", {
  expect_identical(bflSmooth(ts(1:10,start=c(2010,2),freq=4), 4),
                   ts(1:10,start=c(2010,2),freq=4))
  expect_identical(bflSmooth(ts(rep(3,12),start=c(2010,2),freq=4), 12),
                   ts(as.double(rep(1,36)),start=c(2010,4),freq=12))
  expect_equal(bflSmooth(ts((1:4)^2,freq=1,start=1990), 4),
               ts(c(0.1188207,0.1712924,0.2762359,0.4336510,
                    0.6435379,0.8700775,1.1132698,1.3731148,
                    1.6496125,2.0003666,2.4253770,2.9246439,
                    3.4981672,3.9283096,4.2150712,4.3584520),start=1990,freq=4))
  expect_equal(bflSmooth(ts(sin(4:7),start=c(2010,3),freq=4),12),
               ts(c(-0.22957412,-0.24659415,-0.28063423,
                    -0.33169433,-0.33541968,-0.29181026,
                    -0.20086608,-0.09733435,0.01878493,
                    0.14749174,0.23329629,0.27619856),start=c(2010,7),freq=12))
})

test_that("cache works for smoothing", {
  set.seed(10)
  randomarg <- function(n) {
    lfserie <- ts(arima.sim(n,model = list(order=c(1,1,0),ar=0.7)),freq=sample(1:4,1,T),start=2010)
    hf_freq <- sample(1:4,1,T)*frequency(lfserie)
    list(lfserie,hf_freq)
  }
  randomargs <- lapply(rep(30,150),randomarg)
  randomres <- function(notused) lapply(randomargs,function(x) bflSmooth(x[[1]],x[[2]]))
  reslist <- lapply(rep(1,100),randomres)
  expect_true(all(sapply(reslist, FUN = identical, randomres(1))))
  
  set.seed(3)
  randomarg <- function(n) {
    lfserie <- ts(arima.sim(n,model = list(order=c(1,1,0),ar=0.7)),freq=sample(1:4,1,T),start=2010)
    hf_freq <- sample(1:4,1,T)*frequency(lfserie)
    weights <- ts(arima.sim(hf_freq/frequency(lfserie)*length(lfserie),model = list(order=c(1,1,0),ar=0.7))[-1],freq=hf_freq,start=2010)
    list(lfserie,hf_freq,weights)
  }
  randomargs <- lapply(rep(30,150),randomarg)
  randomres <- function(notused) lapply(randomargs,function(x) bflSmooth(x[[1]],x[[2]],x[[3]]))
  reslist <- lapply(rep(1,100),randomres)
  expect_true(all(sapply(reslist, FUN = identical, randomres(1))))
})
