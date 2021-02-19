test_that("hfserie extrap works", {
  hfserie <- ts(c(rep(NA,12),1:12,rep(NA,36)),freq=12,start=c(2010,1))
  expect_equal(hfserie_extrap(hfserie,1L),ts(rep(1:12,5),start=2010,freq=12))
  
  hfserie <- ts(c(rep(NA,12),1:12,1:11,rep(NA,37)),freq=12,start=c(2010,1))
  expect_equal(hfserie_extrap(hfserie,1L),ts(rep(1:12,6),start=2010,freq=12))
  
  hfserie <- ts(c(rep(NA,13),2:12,1:12,rep(NA,36)),freq=12,start=c(2010,1))
  expect_equal(hfserie_extrap(hfserie,1L),ts(rep(1:12,6),start=2010,freq=12))
  
  hfserie <- ts(c(rep(NA,13),2:12,1:12,2,rep(NA,35)),freq=12,start=c(2010,1))
  expect_equal(hfserie_extrap(hfserie,1L),ts(c(rep(1:12,3),rep((1:12)*2,3)),start=2010,freq=12))
  
  hfserie <- ts(c(rep(NA,13),2:12,1:12,2,4,rep(NA,34)),freq=12,start=c(2010,1))
  expect_equal(hfserie_extrap(hfserie,1L),ts(c(rep(1:12,3),rep((1:12)*2,3)),start=2010,freq=12))
  
  hfserie <- ts(c(rep(NA,11),6,1:12,1:12,rep(NA,36)),freq=12,start=c(2010,1))
  expect_equal(hfserie_extrap(hfserie,1L),ts(c((1:12)/2,rep((1:12),5)),start=2010,freq=12))
})
