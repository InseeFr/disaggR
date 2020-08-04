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
})
