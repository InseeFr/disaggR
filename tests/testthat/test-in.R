test_that("in_sample works with include.differenciation=TRUE", {
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = TRUE)
  
  simul <- lag(aggregate(construction),-1)*(100+in_sample(benchmark,type="changes")[,1])/100
  obtained <- construction
  obtained <- window(obtained,start=tsp(obtained)[1]+1,extend = TRUE)
  expect_equal(simul,obtained)
  
  simul <- lag(aggregate(construction),-1)*(na.omit(in_sample(benchmark,type="changes")[,2]))/100
  obtained <- fitted(prais(benchmark))
  obtained <- window(obtained,start=tsp(obtained)[1]+1,extend = TRUE)
  expect_equal(simul,obtained)
  
  simul <- na.omit(in_sample(benchmark,type="levels")[,1])
  obtained <- construction
  expect_equal(simul,obtained)
  
  simul <- na.omit(in_sample(benchmark,type="levels")[,2])
  attr(simul, "na.action") <- NULL
  obtained <- fitted(prais(benchmark))+lag(construction,-1)
  obtained <- window(obtained,start=tsp(obtained)[1]+1,extend = TRUE)
  expect_equal(simul,obtained)
})

test_that("in_sample works with include.differenciation=FALSE", {
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = FALSE)
  
  simul <- lag(aggregate(construction),-1)*(100+in_sample(benchmark,type="changes")[,1])/100
  obtained <- construction
  obtained <- window(obtained,start=tsp(obtained)[1]+1,extend = TRUE)
  expect_equal(simul,obtained)
  
  simul <- lag(aggregate(construction),-1)*(na.omit(in_sample(benchmark,type="changes")[,2]))/100
  obtained <- fitted(prais(benchmark))-lag(aggregate(construction),-1)
  expect_equal(simul,obtained)
  
  simul <- na.omit(in_sample(benchmark,type="levels")[,1])
  obtained <- construction
  expect_equal(simul,obtained)
  
  simul <- na.omit(in_sample(benchmark,type="levels")[,2])
  attr(simul, "na.action") <- NULL
  obtained <- fitted(prais(benchmark))
  obtained <- window(obtained,start=tsp(obtained)[1]+1,extend = TRUE)
  expect_equal(simul,obtained)
})

test_that("print in_sample prints",{
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = TRUE)
  expect_known_output(print(in_sample(benchmark)),"outputs/in_sample.txt",update=FALSE)
})



test_that("in_benchmark works", {
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = FALSE)
  
  simul <- in_benchmark(benchmark,type = "levels")
  obtained <- cbind(na.omit(as.ts(benchmark)),turnover)
  class(obtained) <- c("tscomparison","mts","ts","matrix")
  attr(obtained,"type") <- "levels"
  attr(obtained,"func") <- "in_benchmark"
  colnames(obtained) <- c("Benchmark","High-frequency serie")
  
  expect_equal(simul,obtained)
  
  
  simul <- (100+in_benchmark(benchmark,type="changes"))/100
  obtained <- cbind(na.omit(as.ts(benchmark)),turnover)
  simul <- unname(simul*stats::lag(obtained,-1))
  obtained <- unname(window(obtained,start= tsp(obtained)[1L]+ deltat(obtained),extend = TRUE))
  expect_equal(simul,obtained,tolerance = 1e-10)
  
  simul <- in_benchmark(benchmark,type="contributions")
  simul <- unname(na.omit(ts_from_tsp(rowSums(simul),tsp(simul))))
  attr(simul,"na.action") <- NULL
  obtained <- unname(na.omit((as.ts(benchmark)/stats::lag(as.ts(benchmark),-1)-1)*100))
  attr(obtained,"na.action") <- NULL
  expect_equal(simul,obtained)
  
  simul <- in_benchmark(benchmark,type="contributions")/100
  simul <- unname(simul*stats::lag(as.ts(benchmark),-1))
  simul <- ts_from_tsp(simul  %*% diag(1/c(coef(benchmark)["hfserie"],1,1)),tsp(simul))
  expect_equal(unname(simul),
               unname(window(diff(cbind(turnover,smoothed.part(benchmark),0)),end=c(2020,5),extend = TRUE)))
})

test_that("in revisions works",{
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = FALSE)
  expected <- ts(matrix(c(NA,rep(0,244L)),dimnames = list(NULL,"Benchmark")),
                 start=2000,frequency=12)
  class(expected) <- c("tscomparison","ts")
  attr(expected,"type") <- "changes"
  attr(expected,"func") <- "in_revisions"
  expect_identical(in_revisions(benchmark,benchmark),expected)
})


test_that("in scatter works",{
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = FALSE,
                               start.coeff.calc = 2005,
                               end.coeff.calc = 2017,
                               end.benchmark = 2019)
  expected <- ts(matrix(c(construction,
                          window(window(aggregate(turnover),start=2005,end=2017,extend=TRUE),
                                 start=2000,end=2019,extend=TRUE),
                          window(aggregate(turnover),end=2019,extend=TRUE)),
                        ncol=3,dimnames = list(NULL,c("Low-frequency serie",
                                                      "High-frequency serie (regression)",
                                                      "High-frequency serie (benchmark)"))),
                 start=2000,frequency=1)
  
  class(expected) <- c("tscomparison","mts","ts","matrix")
  attr(expected,"type") <- "levels"
  attr(expected,"func") <- "in_scatter"
  attr(expected,"coefficients") <- coefficients(benchmark)
  expect_identical(in_scatter(benchmark),expected)
})

test_that("error in",{
  benchmark <- twoStepsBenchmark(turnover,construction)
  expect_error(in_benchmark(benchmark,type="aaza"),
               "The type argument of in_benchmark")
  expect_error(in_revisions(twoStepsBenchmark(turnover,construction),
                            twoStepsBenchmark(turnover,construction,
                                              include.differenciation = TRUE),
                            type="aaza"),
               "The type argument of in_revisions")
  
  expect_error(in_sample(benchmark,
                         type="aaza"),
               "The type argument of in_sample")
  expect_error(in_revisions(benchmark,"nothing important"),
               "old_object must be a twoStepsBenchmark")
})

test_that("warning revisions",
          {
            expect_warning(in_revisions(twoStepsBenchmark(turnover,construction),
                                        twoStepsBenchmark(turnover+10,construction)),
                           "The high-frequency inputs contain revisions")
          })

