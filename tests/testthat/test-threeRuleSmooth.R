test_that("hfserie extrap works", {
  hfserie <- ts(c(rep(NA,12),1:12,rep(NA,36)),freq=12,start=c(2010,1))
  expect_equal(hfserie_extrap(hfserie,1L),ts(rep(1:12,5),start=2010,freq=12))
  expect_equal(hfserie_extrap(hfserie,4L),ts(c(rep(1:3,4),1:12,rep(10:12,12)),start=2010,freq=12))
  
  hfserie <- ts(c(rep(NA,12),1:12,1:11,rep(NA,37)),freq=12,start=c(2010,1))
  expect_equal(hfserie_extrap(hfserie,1L),ts(rep(1:12,6),start=2010,freq=12))
  expect_equal(hfserie_extrap(hfserie,4L),ts(c(rep(1:3,4),1:12,1:9,rep(7:9,13)),start=2010,freq=12))
  
  hfserie <- ts(c(rep(NA,13),2:12,1:12,rep(NA,36)),freq=12,start=c(2010,1))
  expect_equal(hfserie_extrap(hfserie,1L),ts(rep(1:12,6),start=2010,freq=12))
  expect_equal(hfserie_extrap(hfserie,4L),ts(c(rep(4:6,5),4:12,1:12,rep(10:12,12)),start=2010,freq=12))
  
  hfserie <- ts(c(rep(NA,13),2:12,1:12,2,rep(NA,35)),freq=12,start=c(2010,1))
  expect_equal(hfserie_extrap(hfserie,1L),ts(rep(1:12,6),start=2010,freq=12))
  expect_equal(hfserie_extrap(hfserie,4L),ts(c(rep(4:6,5),4:12,1:12,rep(10:12,12)),start=2010,freq=12))
  
  hfserie <- ts(c(rep(NA,13),2:12,1:12,2,4,rep(NA,34)),freq=12,start=c(2010,1))
  expect_equal(hfserie_extrap(hfserie,1L),ts(rep(1:12,6),start=2010,freq=12))
  expect_equal(hfserie_extrap(hfserie,4L),ts(c(rep(4:6,5),4:12,1:12,rep(10:12,12)),start=2010,freq=12))
  
  hfserie <- ts(c(rep(NA,11),6,1:12,1:12,rep(NA,36)),freq=12,start=c(2010,1))
  expect_equal(hfserie_extrap(hfserie,1L),ts(rep((1:12),6),start=2010,freq=12))
  expect_equal(hfserie_extrap(hfserie,4L),ts(c(rep(1:3,5),4:12,1:12,rep(10:12,12)),start=2010,freq=12))
})

test_that("rate extrap works", {
  lfserie <- ts(c(NA,NA,3:7,NA,NA),start=2010,freq=12)
  expect_equal(rate_extrap(lfserie,mean.delta = 0.2),
               ts(c(2.6,2.8,3:7,7.2,7.4),start=2010,freq=12))
  
  lfserie <- ts(c(NA,NA,3:7,NA,NA),start=2010,freq=4)
  expect_equal(rate_extrap(lfserie,mean.delta = 0.2),
               ts(c(2.6,2.8,3:7,7.2,7.4),start=2010,freq=4))
  
  lfserie <- ts(c(3:7,NA,NA),start=2010,freq=4)
  expect_equal(rate_extrap(lfserie,mean.delta = 0.5),
               ts(c(3:7,7.5,8),start=2010,freq=4))
  
  lfserie <- ts(c(NA,NA,3:7),start=2010,freq=4)
  expect_equal(rate_extrap(lfserie,mean.delta = 0.5),
               ts(c(2,2.5,3:7),start=2010,freq=4))
})

test_that("mean delta", {
  lfserie <- ts(c(NA,4,4:10,NA),start=2008)
  expect_equal(mean_delta(lfserie,NULL,NULL),6/7)
  expect_equal(mean_delta(lfserie,2009,NULL),6/7)
  expect_equal(mean_delta(lfserie,2010,NULL),1)
})

test_that("threeRuleSmooth works",{
  expect_snapshot(threeRuleSmooth(turnover,construction))
})