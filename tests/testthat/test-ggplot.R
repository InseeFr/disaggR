test_that("ggplot works", {
  library(ggplot2)
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = TRUE)
  ggbenchmark <- autoplot(benchmark)
  gginsample <- autoplot(in_sample(benchmark))
  context("ggplot")
  vdiffr::expect_doppelganger("ggplot of benchmark",ggbenchmark)
  vdiffr::expect_doppelganger("ggplot of insample",gginsample)
})
