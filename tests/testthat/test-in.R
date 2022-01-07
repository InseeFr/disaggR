test_that("in_disaggR levels rebased works if NA at first place",
          {
            expect_identical(
              in_disaggr(twoStepsBenchmark(turnover,construction,start.domain=1990),type = "levels-rebased"),
              in_disaggr(twoStepsBenchmark(window(turnover,start=1990,extend=TRUE),construction),type = "levels-rebased")
            )
          })

test_that("in_disaggR levels rebased works if 0 at first place",
          {
            turnover_modif <-
              ts(c(rep(0,12),turnover),start=1999,frequency = 12)
            expect_identical(
              window(in_disaggr(twoStepsBenchmark(turnover,construction,start.domain=1999),type = "levels-rebased"),
                     start=2000),
              window(in_disaggr(twoStepsBenchmark(turnover_modif,construction),type = "levels-rebased"),
                     start=2000)
            )
          })

test_that("in_sample works with include.differenciation=TRUE", {
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = TRUE,
                                 end.coeff.calc = 2019)
  
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
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = FALSE,
                                 end.coeff.calc = 2019)
  
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
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = TRUE)
  expect_snapshot_output(print(in_sample(benchmark)),cran = TRUE)
})



test_that("in_disaggr works", {
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = FALSE)
  
  simul <- in_disaggr(benchmark,type = "levels")
  obtained <- cbind(na.omit(as.ts(benchmark)),turnover)
  class(obtained) <- c("tscomparison","mts","ts","matrix")
  attr(obtained,"type") <- "levels"
  attr(obtained,"func") <- "in_disaggr"
  colnames(obtained) <- c("Benchmark","High-frequency serie")
  
  expect_equal(simul,obtained)
  
  
  simul <- (100+in_disaggr(benchmark,type="changes"))/100
  obtained <- cbind(na.omit(as.ts(benchmark)),turnover)
  simul <- unname(simul*stats::lag(obtained,-1))
  obtained <- unname(window(obtained,start= tsp(obtained)[1L]+ deltat(obtained),extend = TRUE))
  expect_equal(simul,obtained,tolerance = 1e-10)
  
  simul <- in_disaggr(benchmark,type="contributions")
  simul <- unname(na.omit(ts_from_tsp(rowSums(simul),tsp(simul))))
  attr(simul,"na.action") <- NULL
  obtained <- unname(na.omit((as.ts(benchmark)/stats::lag(as.ts(benchmark),-1)-1)*100))
  attr(obtained,"na.action") <- NULL
  expect_equal(simul,obtained)
  
  simul <- in_disaggr(benchmark,type="contributions")/100
  simul <- unname(simul*stats::lag(as.ts(benchmark),-1))
  simul <- ts_from_tsp(simul  %*% diag(1/c(coef(benchmark)["hfserie"],1,coef(benchmark)["constant"])),tsp(simul))
  expect_equal(unname(simul),
               unname(window(diff(cbind(turnover,smoothed.part(benchmark),0)),end=c(2020,5),extend = TRUE)))
  
  
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = TRUE)
  
  simul <- in_disaggr(benchmark,type = "levels")
  obtained <- cbind(na.omit(as.ts(benchmark)),turnover)
  class(obtained) <- c("tscomparison","mts","ts","matrix")
  attr(obtained,"type") <- "levels"
  attr(obtained,"func") <- "in_disaggr"
  colnames(obtained) <- c("Benchmark","High-frequency serie")
  
  expect_equal(simul,obtained)
  
  
  simul <- (100+in_disaggr(benchmark,type="changes"))/100
  obtained <- cbind(na.omit(as.ts(benchmark)),turnover)
  simul <- unname(simul*stats::lag(obtained,-1))
  obtained <- unname(window(obtained,start= tsp(obtained)[1L]+ deltat(obtained),extend = TRUE))
  expect_equal(simul,obtained,tolerance = 1e-10)
  
  simul <- in_disaggr(benchmark,type="contributions")
  simul <- unname(na.omit(ts_from_tsp(rowSums(simul),tsp(simul))))
  attr(simul,"na.action") <- NULL
  obtained <- unname(na.omit((as.ts(benchmark)/stats::lag(as.ts(benchmark),-1)-1)*100))
  attr(obtained,"na.action") <- NULL
  expect_equal(simul,obtained)
  
  simul <- in_disaggr(benchmark,type="contributions")/100
  simul <- unname(simul*stats::lag(as.ts(benchmark),-1))
  simul <- ts_from_tsp(simul  %*% diag(1/c(coef(benchmark)["hfserie"],1,coef(benchmark)["constant"])),tsp(simul))
  expect_equal(unname(simul),
               unname(window(diff(cbind(turnover,smoothed.part(benchmark),model.list(benchmark)$hfserie[,"constant"])),end=c(2020,5),extend = TRUE)))
  
  benchmark <- threeRuleSmooth(hfserie = turnover,
                               lfserie = construction)
  
  simul <- in_disaggr(benchmark,type = "levels")
  obtained <- cbind(na.omit(as.ts(benchmark)),turnover)
  class(obtained) <- c("tscomparison","mts","ts","matrix")
  attr(obtained,"type") <- "levels"
  attr(obtained,"func") <- "in_disaggr"
  colnames(obtained) <- c("Benchmark","High-frequency serie")
  
  expect_equal(simul,obtained)
  
  
  simul <- (100+in_disaggr(benchmark,type="changes"))/100
  obtained <- cbind(na.omit(as.ts(benchmark)),turnover)
  simul <- unname(simul*stats::lag(obtained,-1))
  obtained <- unname(window(obtained,start= tsp(obtained)[1L]+ deltat(obtained),extend = TRUE))
  expect_equal(simul,obtained,tolerance = 1e-10)
  
  simul <- in_disaggr(benchmark,type="contributions")
  expect_equal(simul[,1L],in_disaggr(benchmark,type="changes")[,1L])
  simul <- unname(na.omit(ts_from_tsp(rowSums(simul),tsp(simul))))
  attr(simul,"na.action") <- NULL
  obtained <- unname(na.omit((as.ts(benchmark)/stats::lag(as.ts(benchmark),-1)-1)*100))
  attr(obtained,"na.action") <- NULL
  expect_equal(simul,obtained)
})

test_that("in revisions works",{
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = FALSE)
  expected <- ts(matrix(c(NA,rep(0,244L)),dimnames = list(NULL,"Benchmark")),
                 start=2000,frequency=12)
  class(expected) <- c("tscomparison","ts")
  attr(expected,"type") <- "changes"
  attr(expected,"func") <- "in_revisions"
  expect_identical(in_revisions(benchmark,benchmark),expected)
  
  benchmark <- threeRuleSmooth(hfserie = turnover,
                               lfserie = construction)
  
  expected <- ts(matrix(c(NA,rep(0,244L)),dimnames = list(NULL,"Benchmark")),
                 start=2000,frequency=12)
  class(expected) <- c("tscomparison","ts")
  attr(expected,"type") <- "changes"
  attr(expected,"func") <- "in_revisions"
  expect_identical(in_revisions(benchmark,benchmark),expected)
})


test_that("in scatter works",{
  benchmark <- twoStepsBenchmark(hfserie = turnover,
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
  attr(expected,"abline") <- c(constant=as.numeric(coefficients(benchmark)["constant"]),
                               slope=as.numeric(coefficients(benchmark)["hfserie"]))
  expect_identical(in_scatter(benchmark),expected)
  
  
  reg <- prais(benchmark)
  expected <- ts(matrix(c(window(reg$model.list$y,start=2005,end=2017,extend=TRUE),
                          window(aggregate(turnover),start=2005,end=2017,extend=TRUE)),
                        ncol=2,dimnames = list(NULL,c("Low-frequency serie",
                                                      "High-frequency serie (regression)"))),
                 start=2005,frequency=1)
  
  class(expected) <- c("tscomparison","mts","ts","matrix")
  attr(expected,"type") <- "levels"
  attr(expected,"func") <- "in_scatter"
  attr(expected,"abline") <- c(constant=as.numeric(coefficients(reg)["constant"]),
                               slope=as.numeric(coefficients(reg)["hfserie"]))
  expect_identical(in_scatter(reg),expected)
  
  
  benchmark <- threeRuleSmooth(hfserie = turnover,
                               lfserie = construction,
                               end.benchmark = 2019)
  expected <- ts(matrix(c(construction,
                          window(aggregate(turnover),end=2019,extend=TRUE)),
                        ncol=2,dimnames = list(NULL,c("Low-frequency serie",
                                                      "High-frequency serie (benchmark)"))),
                 start=2000,frequency=1)
  
  class(expected) <- c("tscomparison","mts","ts","matrix")
  attr(expected,"type") <- "levels"
  attr(expected,"func") <- "in_scatter"
  expect_identical(in_scatter(benchmark),expected)
  
  
  
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = TRUE,
                                 start.coeff.calc = 2005,
                                 end.coeff.calc = 2017,
                                 end.benchmark = 2019)
  expected <- diff(ts(matrix(c(construction,
                               window(window(aggregate(turnover),start=2005,end=2017,extend=TRUE),
                                      start=2000,end=2019,extend=TRUE),
                               window(aggregate(turnover),end=2019,extend=TRUE)),
                             ncol=3,dimnames = list(NULL,c("Low-frequency serie",
                                                           "High-frequency serie (regression)",
                                                           "High-frequency serie (benchmark)"))),
                      start=2000,frequency=1))
  
  class(expected) <- c("tscomparison","mts","ts","matrix")
  attr(expected,"type") <- "changes"
  attr(expected,"func") <- "in_scatter"
  attr(expected,"abline") <- c(constant=as.numeric(coefficients(benchmark)["constant"]),
                               slope=as.numeric(coefficients(benchmark)["hfserie"]))
  expect_identical(in_scatter(benchmark),expected)
  
  
  reg <- prais(benchmark)
  expected <- diff(ts(matrix(c(window(reg$model.list$y,start=2005,end=2017,extend=TRUE),
                               window(aggregate(turnover),start=2005,end=2017,extend=TRUE)),
                             ncol=2,dimnames = list(NULL,c("Low-frequency serie",
                                                           "High-frequency serie (regression)"))),
                      start=2005,frequency=1))
  
  class(expected) <- c("tscomparison","mts","ts","matrix")
  attr(expected,"type") <- "changes"
  attr(expected,"func") <- "in_scatter"
  attr(expected,"abline") <- c(constant=as.numeric(coefficients(reg)["constant"]),
                               slope=as.numeric(coefficients(reg)["hfserie"]))
  expect_identical(in_scatter(reg),expected)
})

test_that("error in",{
  benchmark <- twoStepsBenchmark(turnover,construction)
  expect_error(in_disaggr(benchmark,type="aaza"),
               "The type argument of in_disaggr")
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
  set.seed(1L)
  bn <- twoStepsBenchmark(ts(matrix(rnorm(900,0,100) ,ncol=3),start=c(2000,1),freq=12) %>%
                            `colnames<-`(c("a","b","c")),construction)
  expect_error(in_scatter(bn),"univariate")
  expect_error(in_scatter(prais(bn)),"univariate")
  expect_error(in_sample(threeRuleSmooth(turnover,construction)),
               "The in_sample method needs a regression")
})

test_that("warning revisions",
          {
            expect_warning(in_revisions(twoStepsBenchmark(turnover,construction),
                                        twoStepsBenchmark(turnover+10,construction)),
                           "The high-frequency inputs contain revisions")
          })

test_that("distance",
          {
            benchmark <- twoStepsBenchmark(turnover,construction)
            
            insam <- in_sample(benchmark)
            
            expect_error(distance(insam,p=0),
                         "p should be greater than 1")
            expect_equal(distance(insam,p=2),
                         sqrt(mean((insam[,1L]-insam[,2L])^2)))
            expect_equal(distance(insam,p=Inf),
                         max(insam[,1L]-insam[,2L]))
            
            expect_error(distance(in_scatter(benchmark)))
            
            
            indis <- in_disaggr(benchmark)
            expect_equal(distance(indis,p=2),
                         sqrt(mean((indis[,1L]-indis[,2L])^2,na.rm = TRUE)))
            expect_equal(distance(indis,p=Inf),
                         max(indis[,1L]-indis[,2L],na.rm = TRUE))
            
            inrev <- in_revisions(benchmark,benchmark)
            expect_equal(distance(inrev),
                         0)
            
            expect_error(distance(in_revisions(benchmark,benchmark,type = "contributions")),
                         "support revisions of contributions")
          })

test_that("outliers",{
  benchmark <- twoStepsBenchmark(turnover,construction,include.differenciation = TRUE,
                                 outliers = list(AO2007=rep(0.1,12L)))
  
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
  
  
  simul <- in_disaggr(benchmark,type = "levels")
  obtained <- cbind(na.omit(as.ts(benchmark)),turnover)
  class(obtained) <- c("tscomparison","mts","ts","matrix")
  attr(obtained,"type") <- "levels"
  attr(obtained,"func") <- "in_disaggr"
  colnames(obtained) <- c("Benchmark","High-frequency serie")
  
  expect_equal(simul,obtained)
  
  
  simul <- (100+in_disaggr(benchmark,type="changes"))/100
  obtained <- cbind(na.omit(as.ts(benchmark)),turnover)
  simul <- unname(simul*stats::lag(obtained,-1))
  obtained <- unname(window(obtained,start= tsp(obtained)[1L]+ deltat(obtained),extend = TRUE))
  expect_equal(simul,obtained,tolerance = 1e-10)
  
  simul <- in_disaggr(benchmark,type="contributions")
  simul <- unname(na.omit(ts_from_tsp(rowSums(simul),tsp(simul))))
  attr(simul,"na.action") <- NULL
  obtained <- unname(na.omit((as.ts(benchmark)/stats::lag(as.ts(benchmark),-1)-1)*100))
  attr(obtained,"na.action") <- NULL
  expect_equal(simul,obtained)
  
  simul <- in_disaggr(benchmark,type="contributions")/100
  simul <- unname(simul*stats::lag(as.ts(benchmark),-1))
  simul <- ts_from_tsp(simul  %*% diag(1/c(coef(benchmark)[c("hfserie","AO2007")],1,coef(benchmark)["constant"])),tsp(simul))
  expect_equal(unname(simul),
               unname(window(diff(cbind(turnover,
                                        ts(c(rep(0,84L),rep(0.1,12L),rep(0,149L)),start = 2000,frequency = 12),
                                        smoothed.part(benchmark),
                                        model.list(benchmark)$hfserie[,"constant"])),
                             end=c(2020,5),extend = TRUE)))
  
  
  simul <- in_disaggr(benchmark,type = "levels-rebased")
  obtained <- cbind(na.omit(as.ts(benchmark)/as.ts(benchmark)[1L]),turnover/turnover[1L])*100
  class(obtained) <- c("tscomparison","mts","ts","matrix")
  attr(obtained,"type") <- "levels-rebased"
  attr(obtained,"func") <- "in_disaggr"
  colnames(obtained) <- c("Benchmark","High-frequency serie")
  
  expect_equal(simul,obtained)
  
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = FALSE,
                                 start.coeff.calc = 2005,
                                 end.coeff.calc = 2017,
                                 end.benchmark = 2019,
                                 outliers = list(AO2007=rep(0.1,12L)))
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
  attr(expected,"abline") <- c(constant=as.numeric(coefficients(benchmark)["constant"]),
                               slope=as.numeric(coefficients(benchmark)["hfserie"]))
  expect_identical(in_scatter(benchmark),expected)
  
  
  reg <- prais(benchmark)
  expected <- ts(matrix(c(window(reg$model.list$y,start=2005,end=2017,extend=TRUE),
                          window(aggregate(turnover),start=2005,end=2017,extend=TRUE)),
                        ncol=2,dimnames = list(NULL,c("Low-frequency serie",
                                                      "High-frequency serie (regression)"))),
                 start=2005,frequency=1)
  
  class(expected) <- c("tscomparison","mts","ts","matrix")
  attr(expected,"type") <- "levels"
  attr(expected,"func") <- "in_scatter"
  attr(expected,"abline") <- c(constant=as.numeric(coefficients(reg)["constant"]),
                               slope=as.numeric(coefficients(reg)["hfserie"]))
  expect_identical(in_scatter(reg),expected)
  
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = TRUE,
                                 start.coeff.calc = 2005,
                                 end.coeff.calc = 2017,
                                 end.benchmark = 2019,
                                 outliers = list(AO2007=rep(0.1,12L)))
  expected <- diff(ts(matrix(c(construction,
                               window(window(aggregate(turnover),start=2005,end=2017,extend=TRUE),
                                      start=2000,end=2019,extend=TRUE),
                               window(aggregate(turnover),end=2019,extend=TRUE)),
                             ncol=3,dimnames = list(NULL,c("Low-frequency serie",
                                                           "High-frequency serie (regression)",
                                                           "High-frequency serie (benchmark)"))),
                      start=2000,frequency=1))
  
  class(expected) <- c("tscomparison","mts","ts","matrix")
  attr(expected,"type") <- "changes"
  attr(expected,"func") <- "in_scatter"
  attr(expected,"abline") <- c(constant=as.numeric(coefficients(benchmark)["constant"]),
                               slope=as.numeric(coefficients(benchmark)["hfserie"]))
  expect_identical(in_scatter(benchmark),expected)
})

test_that("in_revisions with different outliers",{
  benchmarka <- twoStepsBenchmark(turnover,construction)
  benchmarkb <- twoStepsBenchmark(turnover,construction,
                                  outliers = list(AO2006=rep(0.1,12)))
  
  eva <- unname(na.omit((as.ts(benchmarka)/
                           stats::lag(as.ts(benchmarka),-1)-1)*100))
  evb <- unname(na.omit((as.ts(benchmarkb)/
                           stats::lag(as.ts(benchmarkb),-1)-1)*100))
  rev <- window(eva-evb,start=2000,extend = TRUE)
  
  res <- in_revisions(benchmarka,
                      benchmarkb,
                      type = "contributions")
  
  expect_equal(ts_from_tsp(rowSums(res),tsp(res)),
               rev)
  expect_snapshot(res,cran = FALSE)
  
  benchmarka <- twoStepsBenchmark(window(turnover,start=2005),
                                  window(construction,start=2005),
                                  outliers = list(LS2010=rep(1,24L)))
  benchmarkb <- twoStepsBenchmark(window(turnover,start=2005),
                                  window(construction,start=2005),
                                  outliers = list(AO2006=rep(0.1,12)))
  
  eva <- unname(na.omit((as.ts(benchmarka)/
                           stats::lag(as.ts(benchmarka),-1)-1)*100))
  evb <- unname(na.omit((as.ts(benchmarkb)/
                           stats::lag(as.ts(benchmarkb),-1)-1)*100))
  rev <- window(eva-evb,start=2005,extend = TRUE)
  
  res <- in_revisions(benchmarka,
                      benchmarkb,
                      type = "contributions")
  
  expect_equal(ts_from_tsp(rowSums(res),tsp(res)),
               rev)
  expect_snapshot(res,cran = FALSE)
})
