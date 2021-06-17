test_that("function_if_it_isnt_one works", {
  expect_identical(function_if_it_isnt_one(seq_len),seq_len)
  expect_identical(lapply(1:20,function_if_it_isnt_one(3L)),
                   lapply(1:20,function(n) rep(3L,n)))
  expect_identical(lapply(1:20,function_if_it_isnt_one(1L:10L)),
                   lapply(1:20,function(n) (1L:10L)[1L:n]))
  expect_identical(lapply(1:20,function_if_it_isnt_one("Hey")),
                   lapply(1:20,function(n) rep("Hey",n)))
})

expect_doppelganger <- vdiffr::expect_doppelganger

test_that("plot works with twoStepsBenchmark", {
  skip_if_not_installed("vdiffr")
  skip_on_cran()
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
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = TRUE)
  benchmark2 <- twoStepsBenchmark(hfserie = turnover,
                                  lfserie = construction,
                                  include.differenciation = FALSE,
                                  set.const = TRUE)
  
  expect_doppelganger("gg-benchmark-nowin",autoplot(benchmark))
  
  expect_doppelganger("gg-insample-levels-nowin",
                      autoplot(in_sample(benchmark,type="levels")))
  expect_doppelganger("gg-insample-changes-nowin",
                      autoplot(in_sample(benchmark,type="changes")))
  
  expect_doppelganger("gg-indicator-levels-nowin",
                      autoplot(in_disaggr(benchmark,type="levels")))
  expect_doppelganger("gg-indicator-levels-rebased-nowin",
                      autoplot(in_disaggr(benchmark,type="levels-rebased")))
  expect_doppelganger("gg-indicator-changes-nowin",
                      autoplot(in_disaggr(benchmark,type="changes")))
  expect_doppelganger("gg-indicator-contributions-nowin",
                      autoplot(in_disaggr(benchmark,type="contributions")))
  
  expect_doppelganger("gg-inrevisions-levels-nowin",
                      autoplot(in_revisions(benchmark,
                                            benchmark2,
                                            type="levels")))
  expect_doppelganger("gg-inrevisions-levels-rebased-nowin",
                      autoplot(in_revisions(benchmark,
                                            benchmark2,
                                            type="levels-rebased")))
  expect_doppelganger("gg-inrevisions-changes-nowin",
                      autoplot(in_revisions(benchmark,
                                            benchmark2,
                                            type="changes")))
  expect_doppelganger("gg-inrevisions-contributions-nowin",
                      autoplot(in_revisions(benchmark,
                                            benchmark2,
                                            type="contributions")))
  
  expect_doppelganger("gg-benchmark-2008-4-2012-7",
                      autoplot(benchmark,
                               start=c(2008,4),
                               end=c(2012,7)))
  
  expect_doppelganger("gg-insample-levels-2008-2012",
                      autoplot(in_sample(benchmark,type="levels"),
                               start=2008,
                               end=2012))
  expect_doppelganger("gg-insample-changes-2008-2012",
                      autoplot(in_sample(benchmark,type="changes"),
                               start=2008,
                               end=2012))
  
  expect_doppelganger("gg-indicator-levels-2008-4-2012-7",
                      autoplot(in_disaggr(benchmark,type="levels"),
                               start=c(2008,4),
                               end=c(2012,7)))
  expect_doppelganger("gg-indicator-levels-rebased-2008-4-2012-7",
                      autoplot(in_disaggr(benchmark,type="levels-rebased"),
                               start=c(2008,4),
                               end=c(2012,7)))
  expect_doppelganger("gg-indicator-changes-2008-4-2012-7",
                      autoplot(in_disaggr(benchmark,type="changes"),
                               start=c(2008,4),
                               end=c(2012,7)))
  expect_doppelganger("gg-indicator-contributions-2008-4-2012-7",
                      autoplot(in_disaggr(benchmark,type="contributions"),
                               start=c(2008,4),
                               end=c(2012,7)))
  
  expect_doppelganger("gg-inrevisions-levels-2008-4-2012-7",
                      autoplot(in_revisions(benchmark,
                                            benchmark2,
                                            type="levels"),
                               start=c(2008,4),
                               end=c(2012,7)))
  expect_doppelganger("gg-inrevisions-levels-rebased-2008-4-2012-7",
                      autoplot(in_revisions(benchmark,
                                            benchmark2,
                                            type="levels"),
                               start=c(2008,4),
                               end=c(2012,7)))
  expect_doppelganger("gg-inrevisions-changes-2008-4-2012-7",
                      autoplot(in_revisions(benchmark,
                                            benchmark2,
                                            type="changes"),
                               start=c(2008,4),
                               end=c(2012,7)))
  expect_doppelganger("gg-inrevisions-contributions-2008-4-2012-7",
                      autoplot(in_revisions(benchmark,
                                            benchmark2,
                                            type="contributions"),
                               start=c(2008,4),
                               end=c(2012,7)))
  announce_snapshot_file(name = "gg-plot-scatter-2008-2012.svg")
  # expect_doppelganger("gg-plot-scatter-2008-2012",
  #                     autoplot(in_scatter(benchmark),
  #                              start=2008,
  #                              end=2012))
  announce_snapshot_file(name = "gg-plot-scatter-coeff-2008-2012.svg")
  # expect_doppelganger("gg-plot-scatter-coeff-2008-2012",
  #                     autoplot(in_scatter(twoStepsBenchmark(turnover,
  #                                                           construction,
  #                                                           start.coeff.calc = 2008,
  #                                                           end.coeff.calc = 2012))))
  # 
  expect_doppelganger("gg-main-insample",
                      autoplot(in_sample(benchmark),
                               main="title in sample"))
  expect_doppelganger("gg-plot-main-inrev",
                      autoplot(in_revisions(benchmark,
                                            benchmark2),
                               main="title in rev"))
  expect_doppelganger("gg-plot-main-ctb",
                      autoplot(in_disaggr(benchmark,
                                          type = "contributions"),
                               main="title ctb"))
  announce_snapshot_file(name = "gg-plot-main-scatter.svg")
  # expect_doppelganger("gg-plot-main-scatter",
  #                     autoplot(in_scatter(benchmark),
  #                              main="title scatter"))
})

test_that("show.legend=FALSE works", {
  skip_if_not_installed("vdiffr")
  skip_on_cran()
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
  announce_snapshot_file(name = "plot-scatter-showlegendf.svg")
  # expect_doppelganger("plot-scatter-showlegendF",
  #                     function() plot(in_scatter(benchmark),
  #                                     show.legend = FALSE))
  
  expect_doppelganger("gg-benchmark-showlegendF",
                      autoplot(benchmark,show.legend = FALSE))
  expect_doppelganger("gg-insample-showlegendF",
                      autoplot(in_sample(benchmark,type="levels"),
                               show.legend = FALSE))
  expect_doppelganger("gg-indicator-showlegendF",
                      autoplot(in_disaggr(benchmark,type="levels-rebased"),
                               show.legend = FALSE))
  expect_doppelganger("gg-ctb-showlegendF",
                      autoplot(in_disaggr(benchmark,type="contributions"),
                               show.legend = FALSE))
  expect_doppelganger("gg-inrevisions-showlegendF",
                      autoplot(in_revisions(benchmark,
                                            benchmark2,
                                            type="changes"),
                               show.legend = FALSE))
  announce_snapshot_file("gg-scatter-showlegendf.svg")
  # expect_doppelganger("gg-scatter-showlegendF",
  #                     autoplot(in_scatter(benchmark),
  #                              show.legend = FALSE))
  
  set.seed(1)
  series <- 10+replicate(3,arima.sim(list(order = c(1,1,0), ar = 0.8), n = 300))
  mts <- ts(series,start=c(2000,1),freq=12) %>%
    `colnames<-`(c("a","b","c")) %>%
    twoStepsBenchmark(construction)
  
  expect_doppelganger("gg-mts-ctb",
                      autoplot(in_disaggr(mts,
                                          type = "contributions")))
  expect_doppelganger("gg-mts-ins",
                      autoplot(in_sample(mts,type="levels")))
  
  expect_doppelganger("gg-mts-indic",
                      autoplot(in_disaggr(mts,
                                          type="levels-rebased")))
  series <- 10+replicate(3,arima.sim(list(order = c(1,1,0), ar = 0.8), n = 300))
  mts2 <- ts(series,start=c(2000,1),freq=12) %>%
    `colnames<-`(c("a","b","c")) %>%
    twoStepsBenchmark(construction)
  expect_doppelganger("gg-mts-rev",
                      autoplot(suppressWarnings(in_revisions(mts,
                                                             mts2,
                                                             type="levels"))))
})

test_that("xlab and ylab works", {
  skip_if_not_installed("vdiffr")
  skip_on_cran()
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
                      autoplot(benchmark,xlab = "foo",ylab="bar"))
  expect_doppelganger("gg-benchmark-lab-foo",
                      autoplot(benchmark,xlab = "foo"))
  expect_doppelganger("gg-benchmark-lab-bar",
                      autoplot(benchmark,ylab="bar"))
})

test_that("plot works with threeRuleSmooth", {
  skip_if_not_installed("vdiffr")
  skip_on_cran()
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
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  smooth <- threeRuleSmooth(hfserie = turnover,
                            lfserie = construction,
                            end.domain = c(2021,12))
  smooth2 <- threeRuleSmooth(hfserie = turnover,
                             lfserie = construction,
                             end.benchmark = 2017,
                             end.domain = c(2021,12))
  
  expect_doppelganger("gg-nowin-smooth",
                      autoplot(smooth))
  
  expect_doppelganger("gg-indicator-levels-nowin-smooth",
                      autoplot(in_disaggr(smooth,type="levels")))
  expect_doppelganger("gg-indicator-levels-rebased-nowin-smooth",
                      autoplot(in_disaggr(smooth,type="levels-rebased")))
  expect_doppelganger("gg-indicator-changes-nowin-smooth",
                      autoplot(in_disaggr(smooth,type="changes")))
  expect_doppelganger("gg-indicator-contributions-nowin-smooth",
                      autoplot(in_disaggr(smooth,type="contributions")))
  
  expect_doppelganger("gg-inrevisions-levels-nowin-smooth",
                      autoplot(in_revisions(smooth,
                                            smooth2,
                                            type="levels")))
  expect_doppelganger("gg-inrevisions-levels-rebased-nowin-smooth",
                      autoplot(in_revisions(smooth,
                                            smooth2,
                                            type="levels-rebased")))
  expect_doppelganger("gg-inrevisions-changes-nowin-smooth",
                      autoplot(in_revisions(smooth,
                                            smooth2,
                                            type="changes")))
  expect_doppelganger("gg-inrevisions-contributions-nowin-smooth",
                      autoplot(in_revisions(smooth,
                                            smooth2,
                                            type="contributions")))
  
  expect_doppelganger("gg-smooth-2008-4-2012-7-smooth",
                      autoplot(smooth,
                               start=c(2008,4),
                               end=c(2012,7)))
  
  expect_doppelganger("gg-indicator-levels-2008-4-2012-7-smooth",
                      autoplot(in_disaggr(smooth,type="levels"),
                               start=c(2008,4),
                               end=c(2012,7)))
  expect_doppelganger("gg-indicator-levels-rebased-2008-4-2012-7-smooth",
                      autoplot(in_disaggr(smooth,type="levels-rebased"),
                               start=c(2008,4),
                               end=c(2012,7)))
  expect_doppelganger("gg-indicator-changes-2008-4-2012-7-smooth",
                      autoplot(in_disaggr(smooth,type="changes"),
                               start=c(2008,4),
                               end=c(2012,7)))
  expect_doppelganger("gg-indicator-contributions-2008-4-2012-7-smooth",
                      autoplot(in_disaggr(smooth,type="contributions"),
                               start=c(2008,4),
                               end=c(2012,7)))
  
  expect_doppelganger("gg-inrevisions-levels-2008-4-2012-7-smooth",
                      autoplot(in_revisions(smooth,
                                            smooth2,
                                            type="levels"),
                               start=c(2008,4),
                               end=c(2012,7)))
  expect_doppelganger("gg-inrevisions-levels-rebased-2008-4-2012-7-smooth",
                      autoplot(in_revisions(smooth,
                                            smooth2,
                                            type="levels"),
                               start=c(2008,4),
                               end=c(2012,7)))
  expect_doppelganger("gg-inrevisions-changes-2008-4-2012-7-smooth",
                      autoplot(in_revisions(smooth,
                                            smooth2,
                                            type="changes"),
                               start=c(2008,4),
                               end=c(2012,7)))
  expect_doppelganger("gg-inrevisions-contributions-2008-4-2012-7-smooth",
                      autoplot(in_revisions(smooth,
                                            smooth2,
                                            type="contributions"),
                               start=c(2008,4),
                               end=c(2012,7)))
  expect_doppelganger("gg-scatter-2008-2012-smooth",
                      autoplot(in_scatter(smooth),
                               start=2008,
                               end=2012))
  
  expect_doppelganger("gg-main-inrev-smooth",
                      autoplot(in_revisions(smooth,
                                            smooth2),
                               main="title in rev"))
  expect_doppelganger("gg-main-ctb-smooth",
                      autoplot(in_disaggr(smooth,
                                          type = "contributions"),
                               main="title ctb"))
  expect_doppelganger("gg-main-scatter-smooth",
                      autoplot(in_scatter(smooth),
                               main="title scatter"))
})

test_that("plot outliers in_disaggr",{
  skip_if_not_installed("vdiffr")
  skip_on_cran()
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
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  benchmark <- twoStepsBenchmark(turnover,construction,
                                 outliers=list(LS2005=rep(0.1,12L)))
  
  expect_doppelganger("ggplot-indicator-levels-outlier",
                      autoplot(in_disaggr(benchmark,type="levels")))
  expect_doppelganger("ggplot-indicator-levels-rebased-outlier",
                      autoplot(in_disaggr(benchmark,type="levels-rebased")))
  expect_doppelganger("ggplot-indicator-changes-outlier",
                      autoplot(in_disaggr(benchmark,type="changes")))
  expect_doppelganger("ggplot-indicator-contributions-outlier",
                      autoplot(in_disaggr(benchmark,type="contributions")))
})

test_that("eval_function_if_it_is_one works", {
  expect_identical(eval_function_if_it_is_one(seq_len,2L),c(1L,2L))
  expect_identical(eval_function_if_it_is_one(3L,2L),3L)
  expect_identical(eval_function_if_it_is_one(1L:10L,2L),1L:10L)
  expect_identical(eval_function_if_it_is_one("Hey",2L),"Hey")
})
