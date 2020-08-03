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