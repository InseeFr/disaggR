test_that("window", {
  set.seed(1)
  freqseries <- c(rnorm(100,10,10),sample.int(1:40,size=100,replace = TRUE))
  series <- lapply(sample(1:400))
  identical(disaggR::window(a,start=c(2004,3)),stats::window(a,start=c(2004,3),extend=TRUE))
  identical(disaggR::window(a,start=c(2004,3),end=c(2005,7)),stats::window(a,start=c(2004,3),end=c(2005,7),extend=TRUE))
  identical(disaggR::window(a,start=c(2004,3),end=c(2005,7)),stats::window(a,start=c(2004,3),end=c(2005,7),extend=TRUE))
})
