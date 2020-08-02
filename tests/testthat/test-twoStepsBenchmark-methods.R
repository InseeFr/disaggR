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
  expect_output(print(summary(benchmark)))
})
