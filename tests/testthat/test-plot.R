test_that("function_if_it_isnt_one works", {
  expect_identical(function_if_it_isnt_one(seq_len),seq_len)
  expect_identical(lapply(1:20,function_if_it_isnt_one(3L)),
                   lapply(1:20,function(n) rep(3L,n)))
  expect_identical(lapply(1:20,function_if_it_isnt_one(1L:10L)),
                   lapply(1:20,function(n) (1L:10L)[1L:n]))
  expect_identical(lapply(1:20,function_if_it_isnt_one("Hey")),
                   lapply(1:20,function(n) rep("Hey",n)))
})

context(if (R.version$major>=4 &&R.version$minor>=1.0) "plots-R-4-1" else "plots-R-4-0")

test_that("plot works", {
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = TRUE)
  benchmark2 <- annualBenchmark(hfserie = turnover,
                                lfserie = construction,
                                include.differenciation = FALSE,
                                set.const = TRUE)
  
  vdiffr::expect_doppelganger("plot-benchmark-nowin",
                              function() plot(benchmark))
  
  vdiffr::expect_doppelganger("plot-insample-levels-nowin",
                              function() plot(in_sample(benchmark,type="levels")))
  vdiffr::expect_doppelganger("plot-insample-changes-nowin",
                              function() plot(in_sample(benchmark,type="changes")))
  
  vdiffr::expect_doppelganger("plot-indicator-levels-nowin",
                              function() plot(in_dicator(benchmark,type="levels")))
  vdiffr::expect_doppelganger("plot-indicator-levels-rebased-nowin",
                              function() plot(in_dicator(benchmark,type="levels-rebased")))
  vdiffr::expect_doppelganger("plot-indicator-changes-nowin",
                              function() plot(in_dicator(benchmark,type="changes")))
  vdiffr::expect_doppelganger("plot-indicator-contributions-nowin",
                              function() plot(in_dicator(benchmark,type="contributions")))
  
  vdiffr::expect_doppelganger("plot-inrevisions-levels-nowin",
                              function() plot(in_revisions(benchmark,
                                                           benchmark2,
                                                           type="levels")))
  vdiffr::expect_doppelganger("plot-inrevisions-levels-rebased-nowin",
                              function() plot(in_revisions(benchmark,
                                                           benchmark2,
                                                           type="levels")))
  vdiffr::expect_doppelganger("plot-inrevisions-changes-nowin",
                              function() plot(in_revisions(benchmark,
                                                           benchmark2,
                                                           type="changes")))
  vdiffr::expect_doppelganger("plot-inrevisions-contributions-nowin",
                              function() plot(in_revisions(benchmark,
                                                           benchmark2,
                                                           type="contributions")))
  
  vdiffr::expect_doppelganger("plot-benchmark-2008-4-2012-7",
                              function() plot(benchmark,
                                              start=c(2008,4),
                                              end=c(2012,7)))
  
  vdiffr::expect_doppelganger("plot-insample-levels-2008-2012",
                              function() plot(in_sample(benchmark,type="levels"),
                                              start=2008,
                                              end=2012))
  vdiffr::expect_doppelganger("plot-insample-changes-2008-2012",
                              function() plot(in_sample(benchmark,type="changes"),
                                              start=2008,
                                              end=2012))
  
  vdiffr::expect_doppelganger("plot-indicator-levels-2008-4-2012-7",
                              function() plot(in_dicator(benchmark,type="levels"),
                                              start=c(2008,4),
                                              end=c(2012,7)))
  vdiffr::expect_doppelganger("plot-indicator-levels-rebased-2008-4-2012-7",
                              function() plot(in_dicator(benchmark,type="levels-rebased"),
                                              start=c(2008,4),
                                              end=c(2012,7)))
  vdiffr::expect_doppelganger("plot-indicator-changes-2008-4-2012-7",
                              function() plot(in_dicator(benchmark,type="changes"),
                                              start=c(2008,4),
                                              end=c(2012,7)))
  vdiffr::expect_doppelganger("plot-indicator-contributions-2008-4-2012-7",
                              function() plot(in_dicator(benchmark,type="contributions"),
                                              start=c(2008,4),
                                              end=c(2012,7)))
  
  vdiffr::expect_doppelganger("plot-inrevisions-levels-2008-4-2012-7",
                              function() plot(in_revisions(benchmark,
                                                           benchmark2,
                                                           type="levels"),
                                              start=c(2008,4),
                                              end=c(2012,7)))
  vdiffr::expect_doppelganger("plot-inrevisions-levels-rebased-2008-4-2012-7",
                              function() plot(in_revisions(benchmark,
                                                           benchmark2,
                                                           type="levels"),
                                              start=c(2008,4),
                                              end=c(2012,7)))
  vdiffr::expect_doppelganger("plot-inrevisions-changes-2008-4-2012-7",
                              function() plot(in_revisions(benchmark,
                                                           benchmark2,
                                                           type="changes"),
                                              start=c(2008,4),
                                              end=c(2012,7)))
  vdiffr::expect_doppelganger("plot-inrevisions-contributions-2008-4-2012-7",
                              function() plot(in_revisions(benchmark,
                                                           benchmark2,
                                                           type="contributions"),
                                              start=c(2008,4),
                                              end=c(2012,7)))
  vdiffr::expect_doppelganger("plot-main-insample",
                              function() plot(in_sample(benchmark),
                                              main="title in sample"))
  vdiffr::expect_doppelganger("plot-main-inrev",
                              function() plot(in_revisions(benchmark,
                                                           benchmark2),
                                              main="title in rev"))
  vdiffr::expect_doppelganger("plot-main-ctb",
                              function() plot(in_dicator(benchmark,
                                                         type = "contributions"),
                                              main="title ctb"))
})

test_that("ggplot works", {
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = TRUE)
  benchmark2 <- annualBenchmark(hfserie = turnover,
                                lfserie = construction,
                                include.differenciation = FALSE,
                                set.const = TRUE)
  
  vdiffr::expect_doppelganger("gg-benchmark-nowin",autoplot(benchmark))
  
  vdiffr::expect_doppelganger("gg-insample-levels-nowin",
                              autoplot(in_sample(benchmark,type="levels")))
  vdiffr::expect_doppelganger("gg-insample-changes-nowin",
                              autoplot(in_sample(benchmark,type="changes")))
  
  vdiffr::expect_doppelganger("gg-indicator-levels-nowin",
                              autoplot(in_dicator(benchmark,type="levels")))
  vdiffr::expect_doppelganger("gg-indicator-levels-rebased-nowin",
                              autoplot(in_dicator(benchmark,type="levels-rebased")))
  vdiffr::expect_doppelganger("gg-indicator-changes-nowin",
                              autoplot(in_dicator(benchmark,type="changes")))
  vdiffr::expect_doppelganger("gg-indicator-contributions-nowin",
                              autoplot(in_dicator(benchmark,type="contributions")))
  
  vdiffr::expect_doppelganger("gg-inrevisions-levels-nowin",
                              autoplot(in_revisions(benchmark,
                                                    benchmark2,
                                                    type="levels")))
  vdiffr::expect_doppelganger("gg-inrevisions-levels-rebased-nowin",
                              autoplot(in_revisions(benchmark,
                                                    benchmark2,
                                                    type="levels")))
  vdiffr::expect_doppelganger("gg-inrevisions-changes-nowin",
                              autoplot(in_revisions(benchmark,
                                                    benchmark2,
                                                    type="changes")))
  vdiffr::expect_doppelganger("gg-inrevisions-contributions-nowin",
                              autoplot(in_revisions(benchmark,
                                                    benchmark2,
                                                    type="contributions")))
  
  vdiffr::expect_doppelganger("gg-benchmark-2008-4-2012-7",
                              autoplot(benchmark,
                                       start=c(2008,4),
                                       end=c(2012,7)))
  
  vdiffr::expect_doppelganger("gg-insample-levels-2008-2012",
                              autoplot(in_sample(benchmark,type="levels"),
                                       start=2008,
                                       end=2012))
  vdiffr::expect_doppelganger("gg-insample-changes-2008-2012",
                              autoplot(in_sample(benchmark,type="changes"),
                                       start=2008,
                                       end=2012))
  
  vdiffr::expect_doppelganger("gg-indicator-levels-2008-4-2012-7",
                              autoplot(in_dicator(benchmark,type="levels"),
                                       start=c(2008,4),
                                       end=c(2012,7)))
  vdiffr::expect_doppelganger("gg-indicator-levels-rebased-2008-4-2012-7",
                              autoplot(in_dicator(benchmark,type="levels-rebased"),
                                       start=c(2008,4),
                                       end=c(2012,7)))
  vdiffr::expect_doppelganger("gg-indicator-changes-2008-4-2012-7",
                              autoplot(in_dicator(benchmark,type="changes"),
                                       start=c(2008,4),
                                       end=c(2012,7)))
  vdiffr::expect_doppelganger("gg-indicator-contributions-2008-4-2012-7",
                              autoplot(in_dicator(benchmark,type="contributions"),
                                       start=c(2008,4),
                                       end=c(2012,7)))
  
  vdiffr::expect_doppelganger("gg-inrevisions-levels-2008-4-2012-7",
                              autoplot(in_revisions(benchmark,
                                                    benchmark2,
                                                    type="levels"),
                                       start=c(2008,4),
                                       end=c(2012,7)))
  vdiffr::expect_doppelganger("gg-inrevisions-levels-rebased-2008-4-2012-7",
                              autoplot(in_revisions(benchmark,
                                                    benchmark2,
                                                    type="levels"),
                                       start=c(2008,4),
                                       end=c(2012,7)))
  vdiffr::expect_doppelganger("gg-inrevisions-changes-2008-4-2012-7",
                              autoplot(in_revisions(benchmark,
                                                    benchmark2,
                                                    type="changes"),
                                       start=c(2008,4),
                                       end=c(2012,7)))
  vdiffr::expect_doppelganger("gg-inrevisions-contributions-2008-4-2012-7",
                              autoplot(in_revisions(benchmark,
                                                    benchmark2,
                                                    type="contributions"),
                                       start=c(2008,4),
                                       end=c(2012,7)))
  
  vdiffr::expect_doppelganger("gg-main-insample",
                              autoplot(in_sample(benchmark),
                                       main="title in sample"))
  vdiffr::expect_doppelganger("gg-plot-main-inrev",
                              autoplot(in_revisions(benchmark,
                                                    benchmark2),
                                       main="title in rev"))
  vdiffr::expect_doppelganger("gg-plot-main-ctb",
                              autoplot(in_dicator(benchmark,
                                                  type = "contributions"),
                                       main="title ctb"))
})

test_that("show.legend=FALSE works", {
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = TRUE)
  
  benchmark2 <- annualBenchmark(hfserie = turnover,
                                lfserie = construction,
                                include.differenciation = FALSE,
                                set.const = TRUE)
  
  vdiffr::expect_doppelganger("plot-benchmark-showlegendF",
                              function() plot(benchmark,show.legend = FALSE))
  vdiffr::expect_doppelganger("plot-insample-showlegendF",
                              function() plot(in_sample(benchmark,type="levels"),
                                              show.legend = FALSE))
  vdiffr::expect_doppelganger("plot-indicator-showlegendF",
                              function() plot(in_dicator(benchmark,type="levels-rebased"),
                                              show.legend = FALSE))
  vdiffr::expect_doppelganger("plot-ctb-showlegendF",
                              function() plot(in_dicator(benchmark,type="contributions"),
                                              show.legend = FALSE))
  vdiffr::expect_doppelganger("plot-inrevisions-showlegendF",
                              function() plot(in_revisions(benchmark,
                                                           benchmark2,
                                                           type="changes"),
                                              show.legend = FALSE))
  
  vdiffr::expect_doppelganger("gg-benchmark-showlegendF",
                              autoplot(benchmark,show.legend = FALSE))
  vdiffr::expect_doppelganger("gg-insample-showlegendF",
                              autoplot(in_sample(benchmark,type="levels"),
                                       show.legend = FALSE))
  vdiffr::expect_doppelganger("gg-indicator-showlegendF",
                              autoplot(in_dicator(benchmark,type="levels-rebased"),
                                       show.legend = FALSE))
  vdiffr::expect_doppelganger("gg-ctb-showlegendF",
                              autoplot(in_dicator(benchmark,type="contributions"),
                                       show.legend = FALSE))
  vdiffr::expect_doppelganger("gg-inrevisions-showlegendF",
                              autoplot(in_revisions(benchmark,
                                                    benchmark2,
                                                    type="changes"),
                                       show.legend = FALSE))
})

test_that("xlab and ylab works", {
  skip_on_os(c("mac","linux","solaris"))
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = TRUE)
  vdiffr::expect_doppelganger("plot-benchmark-lab-foo-bar",
                              function() plot(benchmark,xlab = "foo",ylab="bar"))
  vdiffr::expect_doppelganger("plot-benchmark-lab-foo",
                              function() plot(benchmark,xlab = "foo"))
  vdiffr::expect_doppelganger("plot-benchmark-lab-bar",
                              function() plot(benchmark,ylab = "bar"))
  vdiffr::expect_doppelganger("gg-benchmark-lab-foo-bar",
                              autoplot(benchmark,xlab = "foo",ylab="bar"))
  vdiffr::expect_doppelganger("gg-benchmark-lab-foo",
                              autoplot(benchmark,xlab = "foo"))
  vdiffr::expect_doppelganger("gg-benchmark-lab-bar",
                              autoplot(benchmark,ylab="bar"))
})

context("plots functions")

test_that("eval_function_if_it_is_one works", {
  expect_identical(eval_function_if_it_is_one(seq_len,2L),c(1L,2L))
  expect_identical(eval_function_if_it_is_one(3L,2L),3L)
  expect_identical(eval_function_if_it_is_one(1L:10L,2L),1L:10L)
  expect_identical(eval_function_if_it_is_one("Hey",2L),"Hey")
})
