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
  expect_equal(rate_extrap(lfserie,delta_rate = 0.2),
               ts(c(2.6,2.8,3:7,7.2,7.4),start=2010,freq=12))
  
  lfserie <- ts(c(NA,NA,3:7,NA,NA),start=2010,freq=4)
  expect_equal(rate_extrap(lfserie,delta_rate = 0.2),
               ts(c(2.6,2.8,3:7,7.2,7.4),start=2010,freq=4))
  
  lfserie <- ts(c(3:7,NA,NA),start=2010,freq=4)
  expect_equal(rate_extrap(lfserie,delta_rate = 0.5),
               ts(c(3:7,7.5,8),start=2010,freq=4))
  
  lfserie <- ts(c(NA,NA,3:7),start=2010,freq=4)
  expect_equal(rate_extrap(lfserie,delta_rate = 0.5),
               ts(c(2,2.5,3:7),start=2010,freq=4))
})

test_that("mean delta", {
  lfserie <- ts(c(NA,4,4:10,NA),start=2008)
  expect_equal(mean_delta(lfserie,NULL,NULL),6/7)
  expect_equal(mean_delta(lfserie,2009,NULL),6/7)
  expect_equal(mean_delta(lfserie,2010,NULL),1)
})

test_that("threeRuleSmooth works",{
  expect_snapshot(as.ts(threeRuleSmooth(turnover,construction)),
                  cran = TRUE)
  expect_snapshot(as.ts(threeRuleSmooth(turnover,construction,
                                        start.benchmark = 2004,
                                        end.benchmark = 2017,
                                        start.domain = c(2004,1),
                                        end.domain = c(2030,12))),
                  cran = TRUE)
  set.seed(10L)
  indic <- ts(arima.sim(list(order = c(1,1,0), ar = 0.7), n = 200),
              start=c(2000,2),
              frequency = 12)
  account <- aggregate(window(indic,start=c(2001,4),end=c(2016,3)),nfrequency=4) * rnorm(n=60,mean = 3L,sd = 0.5)
  smooth1 <- threeRuleSmooth(indic,account)
  expect_snapshot(smooth1,cran=TRUE)
  expect_true(all(abs(aggregate(smooth1$smoothed.rate*smooth1$hfserie.as.weights,
                                nfrequency = 4)/
                        aggregate(smooth1$hfserie.as.weights,nfrequency=4)-
                        smooth1$lfrate)<10^-8))
  expect_true(all(abs(aggregate(window(as.ts(smooth1),
                                       start=c(2001,4),
                                       end=c(2016,3),
                                       extend=TRUE),
                                nfrequency=4)-
                        account)<10^-8))
  expect_true(all(abs(diff(window(smooth1$lfrate,end=c(2001,2)))-
                        smooth1$delta.rate)<10^-8))
  expect_true(all(abs(diff(window(smooth1$lfrate,start=c(2016,2)))-
                        smooth1$delta.rate)<10^-8))
  expect_true(abs(mean(diff(window(smooth1$lfrate,c(2001,2),c(2016,2))))-
                    smooth1$delta.rate)<10^-8)
  
  smooth2 <- threeRuleSmooth(indic,account,
                             start.benchmark = c(2003,3),
                             end.benchmark = c(2007,4),
                             start.domain = c(2004,1),
                             end.domain = c(2017,12),
                             start.delta.rate = c(2007,1),
                             end.delta.rate = c(2008,4))
  expect_snapshot(smooth2,cran=TRUE)
  expect_true(all(abs(aggregate(smooth2$smoothed.rate*smooth2$hfserie.as.weights,
                                nfrequency = 4)/
                        aggregate(smooth2$hfserie.as.weights,nfrequency=4)-
                        smooth2$lfrate)<10^-8))
  expect_true(all(abs(aggregate(window(as.ts(smooth2),
                                       start=c(2004,1),
                                       end=c(2007,4),
                                       extend=TRUE),
                                nfrequency=4)-
                        account)<10^-8))
  expect_equal(start(smooth2$lfrate),c(2004,1))
  expect_true(all(abs(diff(window(smooth2$lfrate,start=c(2008,1)))-
                        smooth2$delta.rate)<10^-8))
  lfrate_before_bench <- account/aggregate(window(indic,start=c(2000,4)),nfrequency = 4)
  expect_true(abs(mean(diff(window(lfrate_before_bench,c(2007,1),c(2008,4))))-
                    smooth2$delta.rate)<10^-8)
  
})

test_that("threeRuleSmooth works with set delta",{
  smooth <- threeRuleSmooth(turnover,construction,
                            start.benchmark = 2004,
                            end.benchmark = 2017,
                            start.domain = c(1990,1),
                            end.domain = c(2030,12),set.delta.rate = 2)
  expect_snapshot(smooth)
  expect_true(
    all(
      abs(window(diff(smooth$lfrate),start=1991,end=2004,extend=TRUE)-2) < 10^-8
    )
  )
  
  expect_true(
    all(
      abs(
        aggregate(smooth$hfserie.as.weights * smooth$smoothed.rate) /
          aggregate(smooth$hfserie.as.weights)-
          smooth$lfrate) < 10^-8
    )
  )
  expect_true(
    all(
      abs(
        window(aggregate(as.ts(smooth))-construction,
               start=2004,
               end=2017)
      ) < 10^-8
    )
  )
})

test_that("errors",{
  set.seed(10L)
  indic <- ts(arima.sim(list(order = c(1,1,0), ar = 0.7), n = 200),
              start=c(2000,2),
              frequency = 12)
  account <- aggregate(window(indic,start=c(2001,4),end=c(2016,3)),nfrequency=4) * rnorm(n=60,mean = 3L,sd = 0.5)
  expect_error(threeRuleSmooth(indic,account,
                               start.domain = c(2012,3),
                               end.domain = c(2014,1),
                               start.benchmark = c(2004,3),
                               end.benchmark = c(2007,2),
                               start.delta.rate = c(2010,3)),
               "should have an intersection")
  indic <- ts(arima.sim(list(order = c(1,1,0), ar = 0.7), n = 200),
              start=c(2000,2),
              frequency = 12)
  account <- ts(arima.sim(list(order = c(1,1,0), ar = 0.7), n = 80),
                start=c(2000,2),
                frequency = 4)
  
  expect_error(threeRuleSmooth(indic,account,
                               start.domain = c(2017,1),end.domain = c(2017,12)),
               "does not have any value")
  
  expect_error(threeRuleSmooth(1:10,construction),
               regexp = "Not a ts object")
  expect_error(threeRuleSmooth(matrix(1:9,3,3),construction),
               regexp = "Not a ts object")
  expect_error(threeRuleSmooth(turnover,1:10),
               regexp = "Not a ts object")
  
  expect_error(threeRuleSmooth(cbind(turnover,turnover),construction),
               regexp = "one-dimensional")  
  expect_error(threeRuleSmooth(turnover,cbind(construction,construction)),
               regexp = "one-dimensional")
  expect_error(threeRuleSmooth(turnover,ts(1:700,start=2000,frequency=7)),
               regexp = "should divide")
  expect_error(threeRuleSmooth(ts(1:10,frequency=0.5),ts(1:10,frequency=0.25)),
               regexp = "integer")
  expect_error(threeRuleSmooth(ts(rep(0,120),frequency=12,start=2000),construction),
               "Every hfserie aggregation value is equal to zero")
  hfserie <- turnover
  hfserie[1L:12L] <- 0
  expect_error(threeRuleSmooth(hfserie,construction),
               "There is a zero")
  
  set.seed(20)
  expect_error(threeRuleSmooth(hfserie = ts(diffinv(rnorm(240,1,1)),start=2010.1,freq=12),
                                 lfserie = ts(diffinv(rnorm(18,12,1)),start=2010,freq=1)),
               "time-serie phase")
  expect_error(threeRuleSmooth(hfserie = ts(diffinv(rnorm(240,1,1)),start=2010,freq=12),
                                 lfserie = ts(diffinv(rnorm(18,12,1)),start=2010.1,freq=1)),
               "time-serie phase")
})

test_that("ts eps",{
  turnover_tspmodif <- turnover
  tsp(turnover_tspmodif)[2L] <- tsp(turnover)[2L]+getOption("ts.eps")/24
  tsp(turnover_tspmodif)[1L] <- tsp(turnover)[1L]-getOption("ts.eps")/24
  construction_tspmodif <- disaggR::construction
  tsp(construction_tspmodif)[2L] <- tsp(construction)[2L]+getOption("ts.eps")/24
  tsp(construction_tspmodif)[1L] <- tsp(construction)[1L]-getOption("ts.eps")/24
  expect_identical(as.ts(threeRuleSmooth(turnover_tspmodif,construction_tspmodif)),
                   as.ts(threeRuleSmooth(turnover,construction)))
  expect_identical(as.ts(threeRuleSmooth(turnover_tspmodif,construction)),
                   as.ts(threeRuleSmooth(disaggR::turnover,disaggR::construction)))
  expect_identical(as.ts(threeRuleSmooth(turnover,construction_tspmodif)),
                   as.ts(threeRuleSmooth(disaggR::turnover,disaggR::construction)))
  
  turnover_tspmodif <- turnover
  tsp(turnover_tspmodif)[2L] <- tsp(turnover)[2L]-getOption("ts.eps")/24
  tsp(turnover_tspmodif)[1L] <- tsp(turnover)[1L]+getOption("ts.eps")/24
  construction_tspmodif <- construction
  tsp(construction_tspmodif)[2L] <- tsp(construction)[2L]-getOption("ts.eps")/24
  tsp(construction_tspmodif)[1L] <- tsp(construction)[1L]+getOption("ts.eps")/24
  expect_identical(as.ts(twoStepsBenchmark(turnover_tspmodif,construction_tspmodif)),
                   as.ts(twoStepsBenchmark(disaggR::turnover,disaggR::construction)))
  expect_identical(as.ts(twoStepsBenchmark(turnover_tspmodif,construction)),
                   as.ts(twoStepsBenchmark(disaggR::turnover,disaggR::construction)))
  expect_identical(as.ts(twoStepsBenchmark(turnover,construction_tspmodif)),
                   as.ts(twoStepsBenchmark(disaggR::turnover,disaggR::construction)))
})
