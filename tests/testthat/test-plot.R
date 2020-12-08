context(if (R.version$major>=4 &&R.version$minor>=1.0) "plots-R-4-1" else "plots-R-4-0")

test_that("plot works", {
  testthat::skip_if_not_installed("vdiffr")
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = TRUE)
  plotbenchmark <- function() plot(benchmark)
  plotinsample <- function() plot(in_sample(benchmark))
  vdiffr::expect_doppelganger("plot of benchmark",plotbenchmark)
  vdiffr::expect_doppelganger("plot of insample",plotinsample)
})

test_that("ggplot works", {
  testthat::skip_if_not_installed("vdiffr")
  library(ggplot2)
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = TRUE)
  ggbenchmark <- autoplot(benchmark)
  gginsample <- autoplot(in_sample(benchmark))
  vdiffr::expect_doppelganger("ggplot of benchmark",ggbenchmark)
  vdiffr::expect_doppelganger("ggplot of insample",gginsample)
})
