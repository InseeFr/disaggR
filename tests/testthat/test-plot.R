skips <- function() {
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  skip_if(
    any(
      grepl("openblas",
            as.character(sessionInfo()[c("BLAS","LAPACK")]))
    )
  )
}

expect_doppelganger <- function(title, fig) vdiffr::expect_doppelganger(
  paste0(title, "-", if (getRversion() < "4.4.0") "lt43" else "gt44"),
  fig
)

test_that("function_if_it_isnt_one works", {
  expect_identical(function_if_it_isnt_one(seq_len),seq_len)
  expect_identical(lapply(1:20,function_if_it_isnt_one(3L)),
                   lapply(1:20,function(n) rep(3L,n)))
  expect_identical(lapply(1:20,function_if_it_isnt_one(1L:10L)),
                   lapply(1:20,function(n) (1L:10L)[1L:n]))
  expect_identical(lapply(1:20,function_if_it_isnt_one("Hey")),
                   lapply(1:20,function(n) rep("Hey",n)))
})

test_that("plot works with twoStepsBenchmark", {
  skips()
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = TRUE,
                                 end.coeff.calc = 2018,
                                 end.domain = c(2021,12))
  benchmark2 <- twoStepsBenchmark(hfserie = turnover,
                                  lfserie = construction,
                                  include.differenciation = FALSE,
                                  set.const = TRUE,
                                  end.coeff.calc = 2018,
                                  end.domain = c(2021,12))
  
  expect_doppelganger("plot-benchmark-nowin",
                      function() plot(benchmark))
  
  expect_doppelganger("plot-insample-levels-nowin",
                      function() plot(in_sample(benchmark,type="levels")))
  expect_doppelganger("plot-insample-changes-nowin",
                      function() plot(in_sample(benchmark,type="changes")))
  
  expect_doppelganger("plot-indicator-levels-nowin",
                      function() plot(in_disaggr(benchmark,type="levels")))
  expect_doppelganger("plot-indicator-levels-rebased-nowin",
                      function() plot(in_disaggr(benchmark,type="levels-rebased")))
  expect_doppelganger("plot-indicator-changes-nowin",
                      function() plot(in_disaggr(benchmark,type="changes")))
  expect_doppelganger("plot-indicator-contributions-nowin",
                      function() plot(in_disaggr(benchmark,type="contributions")))
  
  expect_doppelganger("plot-inrevisions-levels-nowin",
                      function() plot(in_revisions(benchmark,
                                                   benchmark2,
                                                   type="levels")))
  expect_doppelganger("plot-inrevisions-levels-rebased-nowin",
                      function() plot(in_revisions(benchmark,
                                                   benchmark2,
                                                   type="levels-rebased")))
  expect_doppelganger("plot-inrevisions-changes-nowin",
                      function() plot(in_revisions(benchmark,
                                                   benchmark2,
                                                   type="changes")))
  expect_doppelganger("plot-inrevisions-contributions-nowin",
                      function() plot(in_revisions(benchmark,
                                                   benchmark2,
                                                   type="contributions")))
  
  expect_doppelganger("plot-benchmark-2008-4-2012-7",
                      function() plot(benchmark,
                                      start=c(2008,4),
                                      end=c(2012,7)))
  
  expect_doppelganger("plot-insample-levels-2008-2012",
                      function() plot(in_sample(benchmark,type="levels"),
                                      start=2008,
                                      end=2012))
  expect_doppelganger("plot-insample-changes-2008-2012",
                      function() plot(in_sample(benchmark,type="changes"),
                                      start=2008,
                                      end=2012))
  
  expect_doppelganger("plot-indicator-levels-2008-4-2012-7",
                      function() plot(in_disaggr(benchmark,type="levels"),
                                      start=c(2008,4),
                                      end=c(2012,7)))
  expect_doppelganger("plot-indicator-levels-rebased-2008-4-2012-7",
                      function() plot(in_disaggr(benchmark,type="levels-rebased"),
                                      start=c(2008,4),
                                      end=c(2012,7)))
  expect_doppelganger("plot-indicator-changes-2008-4-2012-7",
                      function() plot(in_disaggr(benchmark,type="changes"),
                                      start=c(2008,4),
                                      end=c(2012,7)))
  expect_doppelganger("plot-indicator-contributions-2008-4-2012-7",
                      function() plot(in_disaggr(benchmark,type="contributions"),
                                      start=c(2008,4),
                                      end=c(2012,7)))
  
  expect_doppelganger("plot-inrevisions-levels-2008-4-2012-7",
                      function() plot(in_revisions(benchmark,
                                                   benchmark2,
                                                   type="levels"),
                                      start=c(2008,4),
                                      end=c(2012,7)))
  expect_doppelganger("plot-inrevisions-levels-rebased-2008-4-2012-7",
                      function() plot(in_revisions(benchmark,
                                                   benchmark2,
                                                   type="levels-rebased"),
                                      start=c(2008,4),
                                      end=c(2012,7)))
  expect_doppelganger("plot-inrevisions-changes-2008-4-2012-7",
                      function() plot(in_revisions(benchmark,
                                                   benchmark2,
                                                   type="changes"),
                                      start=c(2008,4),
                                      end=c(2012,7)))
  expect_doppelganger("plot-inrevisions-contributions-2008-4-2012-7",
                      function() plot(in_revisions(benchmark,
                                                   benchmark2,
                                                   type="contributions"),
                                      start=c(2008,4),
                                      end=c(2012,7)))
  expect_doppelganger("plot-scatter-2008-2012",
                      function() plot(in_scatter(benchmark),
                                      start=2008,
                                      end=2012))
  expect_doppelganger("plot-scatter-coeff-2008-2012",
                      function() plot(in_scatter(twoStepsBenchmark(turnover,
                                                                   construction,
                                                                   start.coeff.calc = 2008,
                                                                   end.coeff.calc = 2012))))
  
  
  expect_doppelganger("plot-main-insample",
                      function() plot(in_sample(benchmark),
                                      main="title in sample"))
  expect_doppelganger("plot-main-inrev",
                      function() plot(in_revisions(benchmark,
                                                   benchmark2),
                                      main="title in rev"))
  expect_doppelganger("plot-main-ctb",
                      function() plot(in_disaggr(benchmark,
                                                 type = "contributions"),
                                      main="title ctb"))
  expect_doppelganger("plot-main-scatter",
                      function() plot(in_scatter(benchmark),
                                      main="title scatter"))
  set.seed(1)
  series <- 10+replicate(3,arima.sim(list(order = c(1,1,0), ar = 0.8), n = 300))
  mts <- ts(series,start=c(2000,1),freq=12) %>%
    `colnames<-`(c("a","b","c")) %>%
    twoStepsBenchmark(construction)
  
  expect_doppelganger("plot-mts-ctb",
                      function() plot(in_disaggr(mts,
                                                 type = "contributions")))
  expect_doppelganger("plot-mts-ins",
                      function() plot(in_sample(mts,type="levels")))
  
  expect_doppelganger("plot-mts-ind",
                      function() plot(in_disaggr(mts,
                                                 type="levels-rebased")))
  series <- 10+replicate(3,arima.sim(list(order = c(1,1,0), ar = 0.8), n = 300))
  mts2 <- ts(series,start=c(2000,1),freq=12) %>%
    `colnames<-`(c("a","b","c")) %>%
    twoStepsBenchmark(construction)
  expect_doppelganger("plot-mts-rev",
                      function() suppressWarnings(plot(in_revisions(mts,
                                                                    mts2,
                                                                    type="levels"))))
})

test_that("ggplot works", {
  skips()
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = TRUE)
  benchmark2 <- twoStepsBenchmark(hfserie = turnover,
                                  lfserie = construction,
                                  include.differenciation = FALSE,
                                  set.const = TRUE)
  
  expect_doppelganger("gg-benchmark-nowin",ggplot2::autoplot(benchmark))
  
  expect_doppelganger("gg-insample-levels-nowin",
                      ggplot2::autoplot(in_sample(benchmark,type="levels")))
  expect_doppelganger("gg-insample-changes-nowin",
                      ggplot2::autoplot(in_sample(benchmark,type="changes")))
  
  expect_doppelganger("gg-indicator-levels-nowin",
                      ggplot2::autoplot(in_disaggr(benchmark,type="levels")))
  expect_doppelganger("gg-indicator-levels-rebased-nowin",
                      ggplot2::autoplot(in_disaggr(benchmark,type="levels-rebased")))
  expect_doppelganger("gg-indicator-changes-nowin",
                      ggplot2::autoplot(in_disaggr(benchmark,type="changes")))
  expect_doppelganger("gg-indicator-contributions-nowin",
                      ggplot2::autoplot(in_disaggr(benchmark,type="contributions")))
  
  expect_doppelganger("gg-inrevisions-levels-nowin",
                      ggplot2::autoplot(in_revisions(benchmark,
                                                     benchmark2,
                                                     type="levels")))
  expect_doppelganger("gg-inrevisions-levels-rebased-nowin",
                      ggplot2::autoplot(in_revisions(benchmark,
                                                     benchmark2,
                                                     type="levels-rebased")))
  expect_doppelganger("gg-inrevisions-changes-nowin",
                      ggplot2::autoplot(in_revisions(benchmark,
                                                     benchmark2,
                                                     type="changes")))
  expect_doppelganger("gg-inrevisions-contributions-nowin",
                      ggplot2::autoplot(in_revisions(benchmark,
                                                     benchmark2,
                                                     type="contributions")))
  
  expect_doppelganger("gg-benchmark-2008-4-2012-7",
                      ggplot2::autoplot(benchmark,
                                        start=c(2008,4),
                                        end=c(2012,7)))
  
  expect_doppelganger("gg-insample-levels-2008-2012",
                      ggplot2::autoplot(in_sample(benchmark,type="levels"),
                                        start=2008,
                                        end=2012))
  expect_doppelganger("gg-insample-changes-2008-2012",
                      ggplot2::autoplot(in_sample(benchmark,type="changes"),
                                        start=2008,
                                        end=2012))
  
  expect_doppelganger("gg-indicator-levels-2008-4-2012-7",
                      ggplot2::autoplot(in_disaggr(benchmark,type="levels"),
                                        start=c(2008,4),
                                        end=c(2012,7)))
  expect_doppelganger("gg-indicator-levels-rebased-2008-4-2012-7",
                      ggplot2::autoplot(in_disaggr(benchmark,type="levels-rebased"),
                                        start=c(2008,4),
                                        end=c(2012,7)))
  expect_doppelganger("gg-indicator-changes-2008-4-2012-7",
                      ggplot2::autoplot(in_disaggr(benchmark,type="changes"),
                                        start=c(2008,4),
                                        end=c(2012,7)))
  expect_doppelganger("gg-indicator-contributions-2008-4-2012-7",
                      ggplot2::autoplot(in_disaggr(benchmark,type="contributions"),
                                        start=c(2008,4),
                                        end=c(2012,7)))
  
  expect_doppelganger("gg-inrevisions-levels-2008-4-2012-7",
                      ggplot2::autoplot(in_revisions(benchmark,
                                                     benchmark2,
                                                     type="levels"),
                                        start=c(2008,4),
                                        end=c(2012,7)))
  expect_doppelganger("gg-inrevisions-levels-rebased-2008-4-2012-7",
                      ggplot2::autoplot(in_revisions(benchmark,
                                                     benchmark2,
                                                     type="levels"),
                                        start=c(2008,4),
                                        end=c(2012,7)))
  expect_doppelganger("gg-inrevisions-changes-2008-4-2012-7",
                      ggplot2::autoplot(in_revisions(benchmark,
                                                     benchmark2,
                                                     type="changes"),
                                        start=c(2008,4),
                                        end=c(2012,7)))
  expect_doppelganger("gg-inrevisions-contributions-2008-4-2012-7",
                      ggplot2::autoplot(in_revisions(benchmark,
                                                     benchmark2,
                                                     type="contributions"),
                                        start=c(2008,4),
                                        end=c(2012,7)))
  expect_doppelganger("gg-plot-scatter-2008-2012",
                      ggplot2::autoplot(in_scatter(benchmark),
                                        start=2008,
                                        end=2012))
  expect_doppelganger("gg-plot-scatter-coeff-2008-2012",
                      ggplot2::autoplot(in_scatter(twoStepsBenchmark(turnover,
                                                                     construction,
                                                                     start.coeff.calc = 2008,
                                                                     end.coeff.calc = 2012))))
  
  expect_doppelganger("gg-main-insample",
                      ggplot2::autoplot(in_sample(benchmark),
                                        main="title in sample"))
  expect_doppelganger("gg-plot-main-inrev",
                      ggplot2::autoplot(in_revisions(benchmark,
                                                     benchmark2),
                                        main="title in rev"))
  expect_doppelganger("gg-plot-main-ctb",
                      ggplot2::autoplot(in_disaggr(benchmark,
                                                   type = "contributions"),
                                        main="title ctb"))
  expect_doppelganger("gg-plot-main-scatter",
                      ggplot2::autoplot(in_scatter(benchmark),
                                        main="title scatter"))
})

test_that("show.legend=FALSE works with plot", {
  skips()
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = TRUE)
  
  benchmark2 <- twoStepsBenchmark(hfserie = turnover,
                                  lfserie = construction,
                                  include.differenciation = FALSE,
                                  set.const = TRUE)
  
  expect_doppelganger("plot-benchmark-showlegendF",
                      function() plot(benchmark,show.legend = FALSE))
  expect_doppelganger("plot-insample-showlegendF",
                      function() plot(in_sample(benchmark,type="levels"),
                                      show.legend = FALSE))
  expect_doppelganger("plot-indicator-showlegendF",
                      function() plot(in_disaggr(benchmark,type="levels-rebased"),
                                      show.legend = FALSE))
  expect_doppelganger("plot-ctb-showlegendF",
                      function() plot(in_disaggr(benchmark,type="contributions"),
                                      show.legend = FALSE))
  expect_doppelganger("plot-inrevisions-showlegendF",
                      function() plot(in_revisions(benchmark,
                                                   benchmark2,
                                                   type="changes"),
                                      show.legend = FALSE))
  expect_doppelganger("plot-scatter-showlegendF",
                      function() plot(in_scatter(benchmark),
                                      show.legend = FALSE))
})

test_that("show.legend=FALSE works with ggplot", {
  skips()
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = TRUE)
  
  benchmark2 <- twoStepsBenchmark(hfserie = turnover,
                                  lfserie = construction,
                                  include.differenciation = FALSE,
                                  set.const = TRUE)
  
  expect_doppelganger("gg-benchmark-showlegendF",
                      ggplot2::autoplot(benchmark,show.legend = FALSE))
  expect_doppelganger("gg-insample-showlegendF",
                      ggplot2::autoplot(in_sample(benchmark,type="levels"),
                                        show.legend = FALSE))
  expect_doppelganger("gg-indicator-showlegendF",
                      ggplot2::autoplot(in_disaggr(benchmark,type="levels-rebased"),
                                        show.legend = FALSE))
  expect_doppelganger("gg-ctb-showlegendF",
                      ggplot2::autoplot(in_disaggr(benchmark,type="contributions"),
                                        show.legend = FALSE))
  expect_doppelganger("gg-inrevisions-showlegendF",
                      ggplot2::autoplot(in_revisions(benchmark,
                                                     benchmark2,
                                                     type="changes"),
                                        show.legend = FALSE))
  expect_doppelganger("gg-scatter-showlegendF",
                      ggplot2::autoplot(in_scatter(benchmark),
                                        show.legend = FALSE))
})

test_that("mts ggplot", {
  skips()
  set.seed(1)
  series <- 10+replicate(3,arima.sim(list(order = c(1,1,0), ar = 0.8), n = 300))
  mts <- ts(series,start=c(2000,1),freq=12) %>%
    `colnames<-`(c("a","b","c")) %>%
    twoStepsBenchmark(construction)
  
  expect_doppelganger("gg-mts-ctb",
                      ggplot2::autoplot(in_disaggr(mts,
                                                   type = "contributions")))
  expect_doppelganger("gg-mts-ins",
                      ggplot2::autoplot(in_sample(mts,type="levels")))
  
  expect_doppelganger("gg-mts-indic",
                      ggplot2::autoplot(in_disaggr(mts,
                                                   type="levels-rebased")))
  series <- 10+replicate(3,arima.sim(list(order = c(1,1,0), ar = 0.8), n = 300))
  mts2 <- ts(series,start=c(2000,1),freq=12) %>%
    `colnames<-`(c("a","b","c")) %>%
    twoStepsBenchmark(construction)
  expect_doppelganger("gg-mts-rev",
                      ggplot2::autoplot(suppressWarnings(in_revisions(mts,
                                                                      mts2,
                                                                      type="levels"))))
})

test_that("xlab and ylab works", {
  skips()
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = TRUE)
  expect_doppelganger("plot-benchmark-lab-foo-bar",
                      function() plot(benchmark,xlab = "foo",ylab="bar"))
  expect_doppelganger("plot-benchmark-lab-foo",
                      function() plot(benchmark,xlab = "foo"))
  expect_doppelganger("plot-benchmark-lab-bar",
                      function() plot(benchmark,ylab = "bar"))
  expect_doppelganger("gg-benchmark-lab-foo-bar",
                      ggplot2::autoplot(benchmark,xlab = "foo",ylab="bar"))
  expect_doppelganger("gg-benchmark-lab-foo",
                      ggplot2::autoplot(benchmark,xlab = "foo"))
  expect_doppelganger("gg-benchmark-lab-bar",
                      ggplot2::autoplot(benchmark,ylab="bar"))
})

test_that("plot works with threeRuleSmooth", {
  skips()
  smooth <- threeRuleSmooth(hfserie = turnover,
                            lfserie = construction,
                            end.domain = c(2021,12))
  smooth2 <- threeRuleSmooth(hfserie = turnover,
                             lfserie = construction,
                             end.benchmark = 2017,
                             end.domain = c(2021,12))
  
  expect_doppelganger("plot-nowin-smooth",
                      function() plot(smooth))
  
  expect_doppelganger("plot-indicator-levels-nowin-smooth",
                      function() plot(in_disaggr(smooth,type="levels")))
  expect_doppelganger("plot-indicator-levels-rebased-nowin-smooth",
                      function() plot(in_disaggr(smooth,type="levels-rebased")))
  expect_doppelganger("plot-indicator-changes-nowin-smooth",
                      function() plot(in_disaggr(smooth,type="changes")))
  expect_doppelganger("plot-indicator-contributions-nowin-smooth",
                      function() plot(in_disaggr(smooth,type="contributions")))
  
  expect_doppelganger("plot-inrevisions-levels-nowin-smooth",
                      function() plot(in_revisions(smooth,
                                                   smooth2,
                                                   type="levels")))
  expect_doppelganger("plot-inrevisions-levels-rebased-nowin-smooth",
                      function() plot(in_revisions(smooth,
                                                   smooth2,
                                                   type="levels-rebased")))
  expect_doppelganger("plot-inrevisions-changes-nowin-smooth",
                      function() plot(in_revisions(smooth,
                                                   smooth2,
                                                   type="changes")))
  expect_doppelganger("plot-inrevisions-contributions-nowin-smooth",
                      function() plot(in_revisions(smooth,
                                                   smooth2,
                                                   type="contributions")))
  
  expect_doppelganger("plot-smooth-2008-4-2012-7-smooth",
                      function() plot(smooth,
                                      start=c(2008,4),
                                      end=c(2012,7)))
  
  expect_doppelganger("plot-indicator-levels-2008-4-2012-7-smooth",
                      function() plot(in_disaggr(smooth,type="levels"),
                                      start=c(2008,4),
                                      end=c(2012,7)))
  expect_doppelganger("plot-indicator-levels-rebased-2008-4-2012-7-smooth",
                      function() plot(in_disaggr(smooth,type="levels-rebased"),
                                      start=c(2008,4),
                                      end=c(2012,7)))
  expect_doppelganger("plot-indicator-changes-2008-4-2012-7-smooth",
                      function() plot(in_disaggr(smooth,type="changes"),
                                      start=c(2008,4),
                                      end=c(2012,7)))
  expect_doppelganger("plot-indicator-contributions-2008-4-2012-7-smooth",
                      function() plot(in_disaggr(smooth,type="contributions"),
                                      start=c(2008,4),
                                      end=c(2012,7)))
  
  expect_doppelganger("plot-inrevisions-levels-2008-4-2012-7-smooth",
                      function() plot(in_revisions(smooth,
                                                   smooth2,
                                                   type="levels"),
                                      start=c(2008,4),
                                      end=c(2012,7)))
  expect_doppelganger("plot-inrevisions-levels-rebased-2008-4-2012-7-smooth",
                      function() plot(in_revisions(smooth,
                                                   smooth2,
                                                   type="levels"),
                                      start=c(2008,4),
                                      end=c(2012,7)))
  expect_doppelganger("plot-inrevisions-changes-2008-4-2012-7-smooth",
                      function() plot(in_revisions(smooth,
                                                   smooth2,
                                                   type="changes"),
                                      start=c(2008,4),
                                      end=c(2012,7)))
  expect_doppelganger("plot-inrevisions-contributions-2008-4-2012-7-smooth",
                      function() plot(in_revisions(smooth,
                                                   smooth2,
                                                   type="contributions"),
                                      start=c(2008,4),
                                      end=c(2012,7)))
  expect_doppelganger("plot-scatter-2008-2012-smooth",
                      function() plot(in_scatter(smooth),
                                      start=2008,
                                      end=2012))
  
  expect_doppelganger("plot-main-inrev-smooth",
                      function() plot(in_revisions(smooth,
                                                   smooth2),
                                      main="title in rev"))
  expect_doppelganger("plot-main-ctb-smooth",
                      function() plot(in_disaggr(smooth,
                                                 type = "contributions"),
                                      main="title ctb"))
  expect_doppelganger("plot-main-scatter-smooth",
                      function() plot(in_scatter(smooth),
                                      main="title scatter"))
})

test_that("ggplot works with threeRuleSmooth", {
  skips()
  smooth <- threeRuleSmooth(hfserie = turnover,
                            lfserie = construction,
                            end.domain = c(2021,12))
  smooth2 <- threeRuleSmooth(hfserie = turnover,
                             lfserie = construction,
                             end.benchmark = 2017,
                             end.domain = c(2021,12))
  
  expect_doppelganger("gg-nowin-smooth",
                      ggplot2::autoplot(smooth))
  
  expect_doppelganger("gg-indicator-levels-nowin-smooth",
                      ggplot2::autoplot(in_disaggr(smooth,type="levels")))
  expect_doppelganger("gg-indicator-levels-rebased-nowin-smooth",
                      ggplot2::autoplot(in_disaggr(smooth,type="levels-rebased")))
  expect_doppelganger("gg-indicator-changes-nowin-smooth",
                      ggplot2::autoplot(in_disaggr(smooth,type="changes")))
  expect_doppelganger("gg-indicator-contributions-nowin-smooth",
                      ggplot2::autoplot(in_disaggr(smooth,type="contributions")))
  
  expect_doppelganger("gg-inrevisions-levels-nowin-smooth",
                      ggplot2::autoplot(in_revisions(smooth,
                                                     smooth2,
                                                     type="levels")))
  expect_doppelganger("gg-inrevisions-levels-rebased-nowin-smooth",
                      ggplot2::autoplot(in_revisions(smooth,
                                                     smooth2,
                                                     type="levels-rebased")))
  expect_doppelganger("gg-inrevisions-changes-nowin-smooth",
                      ggplot2::autoplot(in_revisions(smooth,
                                                     smooth2,
                                                     type="changes")))
  expect_doppelganger("gg-inrevisions-contributions-nowin-smooth",
                      ggplot2::autoplot(in_revisions(smooth,
                                                     smooth2,
                                                     type="contributions")))
  
  expect_doppelganger("gg-smooth-2008-4-2012-7-smooth",
                      ggplot2::autoplot(smooth,
                                        start=c(2008,4),
                                        end=c(2012,7)))
  
  expect_doppelganger("gg-indicator-levels-2008-4-2012-7-smooth",
                      ggplot2::autoplot(in_disaggr(smooth,type="levels"),
                                        start=c(2008,4),
                                        end=c(2012,7)))
  expect_doppelganger("gg-indicator-levels-rebased-2008-4-2012-7-smooth",
                      ggplot2::autoplot(in_disaggr(smooth,type="levels-rebased"),
                                        start=c(2008,4),
                                        end=c(2012,7)))
  expect_doppelganger("gg-indicator-changes-2008-4-2012-7-smooth",
                      ggplot2::autoplot(in_disaggr(smooth,type="changes"),
                                        start=c(2008,4),
                                        end=c(2012,7)))
  expect_doppelganger("gg-indicator-contributions-2008-4-2012-7-smooth",
                      ggplot2::autoplot(in_disaggr(smooth,type="contributions"),
                                        start=c(2008,4),
                                        end=c(2012,7)))
  
  expect_doppelganger("gg-inrevisions-levels-2008-4-2012-7-smooth",
                      ggplot2::autoplot(in_revisions(smooth,
                                                     smooth2,
                                                     type="levels"),
                                        start=c(2008,4),
                                        end=c(2012,7)))
  expect_doppelganger("gg-inrevisions-levels-rebased-2008-4-2012-7-smooth",
                      ggplot2::autoplot(in_revisions(smooth,
                                                     smooth2,
                                                     type="levels"),
                                        start=c(2008,4),
                                        end=c(2012,7)))
  expect_doppelganger("gg-inrevisions-changes-2008-4-2012-7-smooth",
                      ggplot2::autoplot(in_revisions(smooth,
                                                     smooth2,
                                                     type="changes"),
                                        start=c(2008,4),
                                        end=c(2012,7)))
  expect_doppelganger("gg-inrevisions-contributions-2008-4-2012-7-smooth",
                      ggplot2::autoplot(in_revisions(smooth,
                                                     smooth2,
                                                     type="contributions"),
                                        start=c(2008,4),
                                        end=c(2012,7)))
  expect_doppelganger("gg-scatter-2008-2012-smooth",
                      ggplot2::autoplot(in_scatter(smooth),
                                        start=2008,
                                        end=2012))
  
  expect_doppelganger("gg-main-inrev-smooth",
                      ggplot2::autoplot(in_revisions(smooth,
                                                     smooth2),
                                        main="title in rev"))
  expect_doppelganger("gg-main-ctb-smooth",
                      ggplot2::autoplot(in_disaggr(smooth,
                                                   type = "contributions"),
                                        main="title ctb"))
  expect_doppelganger("gg-main-scatter-smooth",
                      ggplot2::autoplot(in_scatter(smooth),
                                        main="title scatter"))
})

test_that("plot outliers in_disaggr",{
  skips()
  benchmark <- twoStepsBenchmark(turnover,construction,
                                 outliers=list(LS2005=rep(0.1,12L)))
  
  expect_doppelganger("plot-indicator-levels-outlier",
                      function() plot(in_disaggr(benchmark,type="levels")))
  expect_doppelganger("plot-indicator-levels-rebased-outlier",
                      function() plot(in_disaggr(benchmark,type="levels-rebased")))
  expect_doppelganger("plot-indicator-changes-outlier",
                      function() plot(in_disaggr(benchmark,type="changes")))
  expect_doppelganger("plot-indicator-contributions-outlier",
                      function() plot(in_disaggr(benchmark,type="contributions")))
})

test_that("ggplot outliers in_disaggr",{
  skips()
  benchmark <- twoStepsBenchmark(turnover,construction,
                                 outliers=list(LS2005=rep(0.1,12L)))
  
  expect_doppelganger("ggplot-indicator-levels-outlier",
                      ggplot2::autoplot(in_disaggr(benchmark,type="levels")))
  expect_doppelganger("ggplot-indicator-levels-rebased-outlier",
                      ggplot2::autoplot(in_disaggr(benchmark,type="levels-rebased")))
  expect_doppelganger("ggplot-indicator-changes-outlier",
                      ggplot2::autoplot(in_disaggr(benchmark,type="changes")))
  expect_doppelganger("ggplot-indicator-contributions-outlier",
                      ggplot2::autoplot(in_disaggr(benchmark,type="contributions")))
})

test_that("plot method accessibility of graphical parameters",{
  skips()
  benchmark <- twoStepsBenchmark(turnover,construction)
  expect_doppelganger("plot-par-benchmark",
                      function() plot(benchmark,
                                      main="title",
                                      cex.main=0.5,
                                      xlab="foo",ylab="bar",cex.lab=0.3,
                                      cex.axis = 1.1,
                                      xlim=c(2000,2014),
                                      ylim=c(14,18)
                      ))
  
  expect_doppelganger("plot-par-benchmark-outside",
                      function() plot(benchmark,
                                      xlim=c(1995,2025.1)
                      ))
  expect_doppelganger("plot-inscatter-par-benchmark",
                      function() plot(in_scatter(benchmark),
                                      main="title",
                                      cex.main=0.5,
                                      xlab="foo",ylab="bar",cex.lab=0.3,
                                      cex.axis = 1.1,
                                      xlim=c(600,1700),
                                      ylim=c(100,270)
                      ))
  expect_doppelganger("plot-indisaggr-par-benchmark-ctb",
                      function() plot(in_disaggr(benchmark,type = "contributions"),
                                      main="title",
                                      cex.main=0.5,
                                      xlab="foo",ylab="bar",cex.lab=0.3,
                                      cex.axis = 1.1,
                                      xlim=c(2008,2015.5),
                                      ylim=c(-10,10)
                      ))
  expect_doppelganger("plot-indisaggr-par-benchmark",
                      function() plot(in_disaggr(benchmark),
                                      main="title",
                                      cex.main=0.5,
                                      xlab="foo",ylab="bar",cex.lab=0.3,
                                      cex.axis = 1.1,
                                      xlim=c(2008,2015.5),
                                      ylim=c(-5,5)
                      ))
  
  expect_doppelganger("plot-insample-par-benchmark",
                      function() plot(in_sample(benchmark),
                                      main="title",
                                      cex.main=0.5,
                                      xlab="foo",ylab="bar",cex.lab=0.3,
                                      cex.axis = 1.1,
                                      xlim=c(2008,2015.5),
                                      ylim=c(-5,5)
                      ))
})

test_that("eval_function_if_it_is_one works", {
  expect_identical(eval_function_if_it_is_one(seq_len,2L),c(1L,2L))
  expect_identical(eval_function_if_it_is_one(3L,2L),3L)
  expect_identical(eval_function_if_it_is_one(1L:10L,2L),1L:10L)
  expect_identical(eval_function_if_it_is_one("Hey",2L),"Hey")
})

test_that("no labels outside margins", {
  skips()
  expect_doppelganger("no-labels-outside-plot-margins",
                      function() plot(in_sample(twoStepsBenchmark(turnover,construction)),mar = c(5,5,5,5)))
})
