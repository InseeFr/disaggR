test_that("plot works", {
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = TRUE)
  plotbenchmark <- function() plot(benchmark)
  plotinsample <- function() plot(in_sample(benchmark))
  context("plot")
  vdiffr::expect_doppelganger("plot of benchmark",plotbenchmark)
  vdiffr::expect_doppelganger("plot of insample",plotinsample)
})

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
