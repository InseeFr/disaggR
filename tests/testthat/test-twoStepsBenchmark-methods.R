test_that("methods tests", {
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = TRUE,
                                 include.rho = TRUE,
                                 set.const = pi^2)
  expect_s3_class(prais(benchmark),"praislm")
  expect_s3_class(as.ts(benchmark),"ts")
  expect_equal(frequency(as.ts(benchmark)),frequency(turnover))
  expect_s3_class(residuals(benchmark),"ts")
  expect_equal(frequency(residuals(benchmark)),frequency(construction))
  expect_output(print(summary(benchmark)),"^\nCall:\ntwoStepsBenchmark\\(hfserie = turnover, lfserie = construction")
  expect_known_output(print(benchmark),"outputs/benchmark.txt",update = FALSE)
  
  a <- diff(aggregate(smoothed.part(benchmark)))
  b <- residuals(benchmark)
  a <- window(a,end=end(b))
  expect_equal(a,b)
  
  a <- smoothed.part(benchmark)
  b <- as.ts(benchmark)-fitted(benchmark)
  a <- window(a,end=end(b))
  expect_equal(a,b)
  
  bn<- twoStepsBenchmark(turnover,construction,start.coeff.calc=2001,end.coeff.calc=2015,start.benchmark=2002,end.benchmark=2018,start.domain = 2000,end.domain=c(2020,12))
  m <- model.list(bn)
  expect_equal(m$start.coeff.calc,2001)
  expect_equal(m$end.coeff.calc,2015)
  expect_equal(m$start.benchmark,2002)
  expect_equal(m$end.benchmark,2018)
  expect_equal(m$start.domain,2000)
  expect_equal(m$end.domain,c(2020,12))
})
