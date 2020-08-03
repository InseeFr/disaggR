test_that("print praislm", {
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = TRUE,
                                 include.rho = TRUE,
                                 set.const = pi^2)
  expect_known_output(print(prais(benchmark)),"outputs/prais.txt")
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = TRUE,
                                 include.rho = FALSE,
                                 set.coeff = exp(15))
  expect_output(print(summary(prais(benchmark))),"^\nCall:\ntwoStepsBenchmark\\(hfserie = turnover, lfserie = construction")
})
