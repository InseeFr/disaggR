test_that("regression calculates the right coeffs", {
  set.seed(5)
  mensualts <- ts(diffinv(rnorm(120,1,1)),start=2010,freq=12)
  annualts <- ts(diffinv(rnorm(9,12,1)),start=2010,freq=1)
  
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = FALSE))),
               c(-4.42319837305,0.07996253268))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = FALSE,
                                             set.const=-4.42319837305,set.coeff=0.07996253268))),
               c(-4.42319837305,0.07996253268))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = FALSE,
                                             set.const=-3))),
               c(-3,0.07851836099))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = FALSE,
                                             set.const=10))),
               c(10,0.06532678329))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = FALSE,
                                             set.coeff=-3))),
               c(2264.800948259,-3))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = FALSE,
                                             set.coeff=10))),
               c(-7313.2096909,10))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = FALSE,
                                             set.const=10))),
               c(10,0.06532678329))
  
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = TRUE))),
               c(13.268370447389,-0.008573728055))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = TRUE,
                                             set.const=-3))),
               c(-3,0.0964113832))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = TRUE,
                                             set.const=10))),
               c(10,0.0125181346))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = TRUE,
                                             set.coeff=-3))),
               c(458.8907581,-3))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = TRUE,
                                             set.coeff=10))),
               c(-1477.674115137,10))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = TRUE,
                                             set.const=10))),
               c(10.00000000,0.0125181346))
  
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = TRUE))),
               c(13.268370447389,-0.008573728055))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = TRUE,
                                             set.const=-3))),
               c(-3,0.0964113832))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = TRUE,
                                             set.const=10))),
               c(10,0.0125181346))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = TRUE,
                                             set.coeff=-3))),
               c(458.890758064,-3))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = TRUE,
                                             set.coeff=10))),
               c(-1477.674115137,10))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.differenciation = TRUE,
                                             set.const=10))),
               c(10,0.0125181346))
  
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.rho = TRUE))),
               c(-4.39884838,0.07989426))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.rho = TRUE,
                                             set.const=-3))),
               c(-3,0.07853606978))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.rho = TRUE,
                                             set.const=10))),
               c(10,0.06933153642))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.rho = TRUE,
                                             set.coeff=-3))),
               c(2259.176133,-3))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.rho = TRUE,
                                             set.coeff=10))),
               c(-7296.108579,10))
  expect_equal(unname(coef(twoStepsBenchmark(hfserie = mensualts,
                                             lfserie = annualts,
                                             include.rho = TRUE,
                                             set.const=10))),
               c(10,0.06933153642))
})
test_that("Error if any missing value between the coefficient calc",{
  set.seed(5)
  mensualts <- ts(diffinv(rnorm(30,1,1)),start=c(2010,3),freq=12)
  annualts <- ts(diffinv(rnorm(2,12,1)),start=2011,freq=1)
  expect_error(twoStepsBenchmark(mensualts,annualts,include.differenciation = TRUE))
  mensualts <- ts(diffinv(rnorm(30,1,1)),start=c(2010,3),freq=12)
  annualts <- ts(diffinv(rnorm(2,12,1)),start=2010,freq=1)
  expect_error(twoStepsBenchmark(mensualts,annualts,include.differenciation = TRUE))
  mensualts <- ts(diffinv(rnorm(36,1,1)),start=c(2010,1),freq=12)
  annualts <- ts(diffinv(rnorm(2,12,1)),start=2010,freq=1)
  expect_s4_class(twoStepsBenchmark(mensualts,annualts,include.differenciation = TRUE),"twoStepsBenchmark")
  mensualts[10] <- NA
  expect_error(twoStepsBenchmark(mensualts,annualts,include.differenciation = TRUE))
})
test_that("twoStepsBenchmark works",
          {
            set.seed(5)
            mensualts <- ts(diffinv(rnorm(30,1,1)),start=c(2010,4),freq=12)
            trimts <- ts(diffinv(rnorm(2,12,1)),start=2011,freq=4)
            expect_snapshot(twoStepsBenchmark(mensualts,trimts,include.differenciation = TRUE),
                            cran = TRUE)
            set.seed(32)
            mensualts <- ts(diffinv(rnorm(30,1,1)),start=c(2010,3),freq=12)
            trimts <- ts(diffinv(rnorm(2,12,1)),start=2011,freq=4)
            bn <- twoStepsBenchmark(mensualts,trimts,include.differenciation = TRUE)
            expect_snapshot(bn, cran = TRUE)
            expect_equal(aggregate(window(as.ts(bn),start=c(2010,4)),nf=4)-trimts,ts(c(0,0,0),start=2011,freq=4))
            expect_snapshot(twoStepsBenchmark(turnover,construction,
                                              include.differenciation = TRUE,
                                              include.rho = TRUE),
                            cran = TRUE)
          })
test_that("standard errors are the same that the vcov diag",{
  set.seed(5)
  mensualts <- ts(diffinv(rnorm(120,1,1)),start=2010,freq=12)
  annualts <- ts(diffinv(rnorm(9,12,1)),start=2010,freq=1)
  
  bn <- twoStepsBenchmark(hfserie = mensualts,
                          lfserie = annualts,
                          include.differenciation = FALSE)
  stderror <- se(bn)
  stderror <- stderror[!is.na(stderror)]
  expect_equal(unname(stderror),unname(sqrt(abs(diag(vcov(bn))))))
  
  set.seed(20)
  mensualts <- ts(diffinv(rnorm(240,1,1)),start=2010,freq=12)
  annualts <- ts(diffinv(rnorm(18,12,1)),start=2010,freq=1)
  bn <- twoStepsBenchmark(hfserie = mensualts,
                          lfserie = annualts,
                          include.differenciation = TRUE)
  stderror <- se(bn)
  stderror <- stderror[!is.na(stderror)]
  expect_equal(unname(stderror),unname(sqrt(abs(diag(vcov(bn))))))
  
  bn <- twoStepsBenchmark(hfserie = mensualts,
                          lfserie = annualts,
                          include.differenciation = TRUE,set.coeff = 3)
  stderror <- se(bn)
  stderror <- stderror[!is.na(stderror)]
  expect_equal(unname(stderror),unname(sqrt(abs(diag(vcov(bn))))))
})

test_that("mts works",{
  bn <- twoStepsBenchmark(ts(matrix(rnorm(900,0,100) ,ncol=3),start=c(2000,1),freq=12) %>%
                            `colnames<-`(c("a","b","c")),construction)
  expect_snapshot(bn, cran = TRUE)
  expect_identical(names(coef(bn)),c("constant","a","b","c"))
  
  mat <- cbind(turnover,lag(turnover))
  colnames(mat) <- c("turnover","lag turnover")
  bn <- twoStepsBenchmark(mat,construction,include.differenciation=TRUE,
                          set.coeff = c(turnover=0.1),set.const=0)
  expect_equal(coef(bn)[c("constant","turnover")],c(constant=0, turnover=0.1))
  bn <- twoStepsBenchmark(mat,construction,include.differenciation=TRUE,
                          set.coeff = c(turnover=0.1,`lag turnover`=0.05),set.const=0)
  expect_equal(coef(bn),c(constant=0, turnover=0.1,`lag turnover`=0.05))
})


test_that("The classes in the bn object are the good ones",{
  set.seed(18)
  mensualts <- ts(diffinv(rnorm(120,1,1)),start=2010,freq=12)
  trimts <- ts(diffinv(rnorm(36,12,1)),start=2010,freq=4)
  
  bn <- twoStepsBenchmark(hfserie = mensualts,
                          lfserie = trimts,
                          include.differenciation = TRUE)
  expect_s4_class(bn,"twoStepsBenchmark")
  expect_true(is.ts(bn$benchmarked.serie))
  expect_true(is.ts(bn$fitted.values))
  expect_s3_class(bn$regression,"praislm")
  expect_true(is.list(bn$model.list))
  expect_true(is.call(bn$call))
  
  expect_true(is.ts(bn$model.list$hfserie))
  expect_true(is.ts(bn$model.list$lfserie))
  expect_true(is.logical(bn$model.list$include.rho))
  expect_true(is.logical(bn$model.list$include.differenciation))
  expect_true(is.numeric(bn$model.list$set.coefficients))
  expect_true(is.null(bn$model.list$start.coeff.calc))
  expect_true(is.null(bn$model.list$end.coeff.calc))
  
  expect_true(is.numeric(bn$regression$coefficients))
  expect_true(is.ts(bn$regression$residuals))
  expect_true(is.ts(bn$regression$fitted.values))
  expect_true(is.numeric(bn$regression$se))
  expect_true(is.numeric(bn$regression$df.residual))
  expect_true(is.numeric(bn$regression$rho))
  expect_true(is.ts(bn$regression$residuals.decorrelated))
  expect_true(is.ts(bn$regression$fitted.values.decorrelated))
})

test_that("windows and extraps works",{
  set.seed(27)
  mensualts <- ts(diffinv(rnorm(120,1,1)),start=2010,freq=12)
  trimts <- ts(diffinv(rnorm(36,12,1)),start=2010,freq=4)
  
  bn <- twoStepsBenchmark(hfserie = mensualts,
                          lfserie = trimts,
                          include.rho = TRUE,
                          start.coeff.calc = c(2012,2),
                          start.domain = c(2011,3))
  asp <- aggregate(smoothed.part(bn),nf=4)
  extrap <- asp/lag(asp,-1)
  expect_equal(as.numeric(window(extrap,start=c(2011,2),end=c(2011,2),extend=TRUE)),
               rho(bn))
  expect_equal(as.numeric(window(extrap,start=c(2019,2))),
               rep(rho(bn),4))
  
  bn <- twoStepsBenchmark(hfserie = mensualts,
                          lfserie = trimts,
                          include.rho = TRUE,
                          include.differenciation = TRUE,
                          start.coeff.calc = c(2012,2),
                          start.domain = c(2011,3))
  asp <- aggregate(smoothed.part(bn),nf=4)
  extrap <- diff(asp)/lag(diff(asp),-1)
  expect_equal(as.numeric(window(extrap,start=c(2011,3),end=c(2011,3),extend=TRUE)),
               rho(bn))
  expect_equal(as.numeric(window(extrap,start=c(2019,2))),
               rep(rho(bn),4))
  expect_equal(tsp(as.ts(bn)),tsp(window(mensualts,start=c(2011,3))))
  
  bn <- twoStepsBenchmark(hfserie = mensualts,
                          lfserie = trimts,
                          include.rho = TRUE,
                          end.coeff.calc = c(2014,2),
                          end.domain = c(2015,5))
  asp <- aggregate(smoothed.part(bn),nf=4)
  extrap <- asp/lag(asp,-1)
  expect_equal(as.numeric(window(extrap,start=c(2015,2))),
               rho(bn))
  expect_equal(tsp(as.ts(bn)),tsp(window(mensualts,end=c(2015,5))))
  
  bn <- twoStepsBenchmark(hfserie = mensualts,
                          lfserie = trimts,
                          include.rho = FALSE,
                          include.differenciation = FALSE,
                          start.benchmark = c(2012,1),
                          end.benchmark = c(2014,3))
  asp <- aggregate(smoothed.part(bn),nf=4)
  residextrap <- window(window(residuals(bn),start=c(2012,1),end=c(2014,3),extend=TRUE),
                        start=start(asp),end=end(asp),extend=TRUE)
  residextrap[is.na(residextrap)] <- 0
  expect_equal(asp,residextrap)
})

test_that("errors",{
  expect_error(twoStepsBenchmark(cbind(turnover,turnover),construction),
               regexp = "perfect rank")
  expect_error(twoStepsBenchmark(cbind(turnover,turnover),construction,set.coeff = c(a=1)),
               regexp = "names of the set coefficient")
  expect_error(twoStepsBenchmark(cbind(turnover,turnover),construction,set.coeff = c(a=NA)),
               regexp = "be set to NA")
  expect_error(twoStepsBenchmark(1:10,construction),
               regexp = "Not a ts object")
  expect_error(twoStepsBenchmark(matrix(1:9,3,3),construction),
               regexp = "Not a ts object")
  expect_error(twoStepsBenchmark(turnover,1:10),
               regexp = "Not a ts object")
  expect_error(twoStepsBenchmark(window(turnover,start=2001),construction),
               regexp = "must have values")
  
  expect_error(twoStepsBenchmark(turnover,construction,set.const=c(1,2)),
               regexp = "single value")
  expect_error(twoStepsBenchmark(ts(matrix(rnorm(900,0,100) ,ncol=3),start=c(2000,1),freq=12) %>%
                                   `unname`,construction),
               regexp = "column names")
  expect_error(twoStepsBenchmark(ts(matrix(rnorm(900,0,100) ,ncol=3),start=c(2000,1),freq=12) %>%
                                   `colnames<-`(c("a","b","c")),
                                 construction,
                                 set.coeff=1:4),
               regexp = "empty or have names")
  expect_error(twoStepsBenchmark_impl(turnover,construction,
                                      start.coeff.calc = 2000,
                                      end.coeff.calc = 2010,
                                      start.domain = 2000,
                                      end.domain= 2020.333,
                                      include.rho=FALSE,
                                      include.differenciation = TRUE,
                                      set_coefficients = numeric(),
                                      maincl = NULL),
               regexp = "Not a matrix")
  expect_error(twoStepsBenchmark(turnover,construction,set.smoothed.part=pi),
               regexp = "univariate time-serie")
  expect_error(twoStepsBenchmark(turnover,cbind(construction,construction)),
               regexp = "one-dimensional")
  expect_error(twoStepsBenchmark(turnover,ts(1:700,start=2000,frequency=7)),
               regexp = "should divide")
  expect_error(twoStepsBenchmark(ts(1:10,frequency=0.5),ts(1:10,frequency=0.25)),
               regexp = "integer")
})

test_that("reUseBenchmark works",{
  benchmark1 <- twoStepsBenchmark(turnover,construction,include.rho = TRUE,
                                  start.coeff.calc=2001,end.coeff.calc=2015,
                                  start.benchmark=2002,end.benchmark=2018,
                                  start.domain = c(2000,2),end.domain=c(2020,12))
  decompose(turnover,type = "multiplicative")
  adjusted_turnover <- window(turnover/decompose(turnover)$seasonal,start=2006)
  benchmark2 <- reUseBenchmark(adjusted_turnover,benchmark1)
  benchmark3 <- reUseBenchmark(adjusted_turnover,benchmark1,reeval.smoothed.part = TRUE)
  m1 <- model.list(benchmark1)
  m2 <- model.list(benchmark2)
  m3 <- model.list(benchmark3)
  
  expect_identical(smoothed.part(benchmark2),smoothed.part(benchmark1))
  expect_identical(coefficients(benchmark1),coefficients(benchmark2))
  expect_identical(m1$include.rho,m2$include.rho)
  expect_identical(m1$include.differenciation,m2$include.differenciation)
  expect_identical(m1$start.coeff.calc,m2$start.coeff.calc)
  expect_identical(m1$end.coeff.calc,m2$end.coeff.calc)
  expect_identical(m1$start.benchmark,m2$start.benchmark)
  expect_identical(m1$end.benchmark,m2$end.benchmark)
  expect_identical(m1$start.domain,m2$start.domain)
  expect_identical(m1$end.domain,m2$end.domain)
  expect_identical(smoothed.part(benchmark1),m2$set.smoothed.part)
  
  expect_false(identical(smoothed.part(benchmark3),smoothed.part(benchmark1)))
  expect_identical(coefficients(benchmark1),coefficients(benchmark3))
  expect_identical(m1$include.rho,m3$include.rho)
  expect_identical(m1$include.differenciation,m3$include.differenciation)
  expect_identical(m1$start.coeff.calc,m3$start.coeff.calc)
  expect_identical(m1$end.coeff.calc,m3$end.coeff.calc)
  expect_identical(m1$start.benchmark,m3$start.benchmark)
  expect_identical(m1$end.benchmark,m3$end.benchmark)
  expect_identical(m1$start.domain,m3$start.domain)
  expect_identical(m1$end.domain,m3$end.domain)
  expect_null(m1$set.smoothed.part)
  expect_null(m3$set.smoothed.part)
  
  expect_false(identical(as.ts(benchmark3),as.ts(benchmark2)))
  
  benchmark <- twoStepsBenchmark(turnover,construction)
  turnover_modif <- turnover
  turnover_modif[1] <- turnover[1]+pi
  benchmark2 <- reUseBenchmark(turnover_modif,benchmark)
  
  coefficients <- coef(benchmark)
  expect_equal((as.ts(benchmark2)-as.ts(benchmark))[1],pi*coefficients[2],
               ignore_attr = TRUE)
})

test_that("residuals extrap sequence doesn't bug if rho==1 and include.differenciation=TRUE",{
  sequence <- residuals_extrap_sequence(1,3,1,10,TRUE)
  expect_equal(sequence[-1]-sequence[-length(sequence)],rep(2,9))
})

test_that("annualBenchmark",{
  set.seed(27)
  mensualts <- ts(diffinv(rnorm(120,1,1)),start=2010,freq=12)
  trimts <- ts(diffinv(rnorm(36,12,1)),start=2010,freq=4)
  expect_error(annualBenchmark(mensualts,trimts),
               "annual time-serie")
  expect_snapshot(annualBenchmark(turnover,construction,
                                  end.coeff.calc = 2018),
                  cran = TRUE)
  expect_equal(as.ts(annualBenchmark(turnover,construction,
                               end.coeff.calc = 2018)),
               as.ts(twoStepsBenchmark(turnover,construction,
                                 end.coeff.calc = 2018,
                                 end.benchmark = 2019,
                                 end.domain = c(2021,12))))
  
  set.seed(5)
  mensualts <- ts(diffinv(rnorm(120,1,1)),start=2010,freq=12)
  annualts <- ts(diffinv(rnorm(9,12,1)),start=2010,freq=1)
  
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                           lfserie = annualts,
                                           include.differenciation = FALSE))),
               c(-4.42319837305,0.07996253268))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                           lfserie = annualts,
                                           include.differenciation = FALSE,
                                           set.const=-4.42319837305,set.coeff=0.07996253268))),
               c(-4.42319837305,0.07996253268))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                           lfserie = annualts,
                                           include.differenciation = FALSE,
                                           set.const=-3))),
               c(-3,0.07851836099))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                           lfserie = annualts,
                                           include.differenciation = FALSE,
                                           set.const=10))),
               c(10,0.06532678329))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                           lfserie = annualts,
                                           include.differenciation = FALSE,
                                           set.coeff=-3))),
               c(2264.800948259,-3))
  
  expect_equal(as.ts(annualBenchmark(mensualts,annualts,end.coeff.calc = 2019)),
               as.ts(annualBenchmark(mensualts,annualts,end.coeff.calc = c(2019,1))))
})

