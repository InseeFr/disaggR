test_that("window", {
  set.seed(1)
  a <- ts(rnorm(40),freq=12,start = c(2002,4))
  identical(disaggR::window(a,start=c(2004,3)),stats::window(a,start=c(2004,3),extend=TRUE))
  identical(disaggR::window(a,start=c(2004,3),end=c(2005,7)),stats::window(a,start=c(2004,3),end=c(2005,7),extend=TRUE))
  identical(disaggR::window(a,start=c(2004,3),end=c(2005,7)),stats::window(a,start=c(2004,3),end=c(2005,7),extend=TRUE))
})
