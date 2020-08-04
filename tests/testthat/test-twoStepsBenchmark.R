test_that("regression calculates the right coeffs", {
  set.seed(5)
  mensualts <- ts(diffinv(rnorm(120,1,1)),start=2010,freq=12)
  annualts <- ts(diffinv(rnorm(9,12,1)),start=2010,freq=1)
  
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.differenciation = FALSE))),
               c(-4.42319837,0.07996253))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                           lfserie = annualts,
                                           include.differenciation = FALSE,
                                           set.const=-4.42319837,set.coeff=0.07996253))),
               c(-4.42319837,0.07996253))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.differenciation = FALSE,
                                    set.const=-3))),
               c(-3,0.07851836))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.differenciation = FALSE,
                                    set.const=10))),
               c(10,0.06532678))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.differenciation = FALSE,
                                    set.coeff=-3))),
               c(2264.80095,-3))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.differenciation = FALSE,
                                    set.coeff=10))),
               c(-7313.2097,10))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.differenciation = FALSE,
                                    set.const=10))),
               c(10.00000000,0.06532678))
  
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.differenciation = TRUE))),
               c(13.26837045,-0.008573728))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.differenciation = TRUE,
                                    set.const=-3))),
               c(-3,0.09641138))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.differenciation = TRUE,
                                    set.const=10))),
               c(10,0.01251813))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.differenciation = TRUE,
                                    set.coeff=-3))),
               c(458.89076,-3))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.differenciation = TRUE,
                                    set.coeff=10))),
               c(-1477.6741,10))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.differenciation = TRUE,
                                    set.const=10))),
               c(10.00000000,0.01251813))

  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.differenciation = TRUE))),
               c(13.26837045,-0.008573728))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.differenciation = TRUE,
                                    set.const=-3))),
               c(-3,0.09641138))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.differenciation = TRUE,
                                    set.const=10))),
               c(10,0.01251813))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.differenciation = TRUE,
                                    set.coeff=-3))),
               c(458.89076,-3))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.differenciation = TRUE,
                                    set.coeff=10))),
               c(-1477.6741,10))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.differenciation = TRUE,
                                    set.const=10))),
               c(10.00000000,0.01251813))
  
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.rho = TRUE))),
               c(-4.39884838,0.07989426))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.rho = TRUE,
                                    set.const=-3))),
               c(-3,0.07853607))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.rho = TRUE,
                                    set.const=10))),
               c(10,0.06933154))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.rho = TRUE,
                                    set.coeff=-3))),
               c(2259.1761334,-3))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.rho = TRUE,
                                    set.coeff=10))),
               c(-7296.1085794,10))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    lfserie = annualts,
                                    include.rho = TRUE,
                                    set.const=10))),
               c(10,0.06933154))
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
  expect_s3_class(twoStepsBenchmark(mensualts,annualts,include.differenciation = TRUE),"twoStepsBenchmark")
  mensualts[10] <- NA
  expect_error(twoStepsBenchmark(mensualts,annualts,include.differenciation = TRUE))
})
test_that("twoStepsBenchmark works",
          {
  set.seed(5)
  mensualts <- ts(diffinv(rnorm(30,1,1)),start=c(2010,4),freq=12)
  trimts <- ts(diffinv(rnorm(2,12,1)),start=2011,freq=4)
  bn <- twoStepsBenchmark(mensualts,trimts,include.differenciation = TRUE)
  expect_equal(as.ts(bn),ts(c(-13.2250655390399,-11.7137209841592,-10.7287373045719,
                              -9.11931306475712,-7.82345954003013,-6.91584498370111,
                              -5.46078544573486,-4.0366520279386,-2.57391348874945,
                              -1.19387012919245,0.0859065845360174,1.10796354465643,
                              2.61006490598375,4.17807049258785,5.52777963978986,
                              7.09374324740536,8.43906492834152,9.89280103027344,
                              11.7218502943277,12.9773318186937,14.3511261089332,
                              15.4505609135171,16.5402128681158,17.5054208765419,
                              18.6506862539418,19.7694000952659,21.1512667853785,
                              22.128153633179,23.0860732705669,24.5539473616461,
                              26.0681162404146),start=c(2010,4),frequency=12))
  set.seed(32)
  mensualts <- ts(diffinv(rnorm(30,1,1)),start=c(2010,3),freq=12)
  trimts <- ts(diffinv(rnorm(2,12,1)),start=2011,freq=4)
  bn <- twoStepsBenchmark(mensualts,trimts,include.differenciation = TRUE)
  expect_equal(aggregate(window(as.ts(bn),start=c(2010,4)),nf=4)-trimts,ts(c(0,0,0),start=2011,freq=4))
})
test_that("standard errors are the same that the vcov diag",{
  set.seed(5)
  mensualts <- ts(diffinv(rnorm(120,1,1)),start=2010,freq=12)
  annualts <- ts(diffinv(rnorm(9,12,1)),start=2010,freq=1)
  
  bn <- annualBenchmark(hfserie = mensualts,
                        lfserie = annualts,
                        include.differenciation = FALSE)
  stderror <- se(bn)
  stderror <- stderror[!is.na(stderror)]
  expect_equal(unname(stderror),unname(sqrt(abs(diag(vcov(bn))))))
  
  set.seed(20)
  mensualts <- ts(diffinv(rnorm(240,1,1)),start=2010,freq=12)
  annualts <- ts(diffinv(rnorm(18,12,1)),start=2010,freq=1)
  bn <- annualBenchmark(hfserie = mensualts,
                        lfserie = annualts,
                        include.differenciation = TRUE)
  stderror <- se(bn)
  stderror <- stderror[!is.na(stderror)]
  expect_equal(unname(stderror),unname(sqrt(abs(diag(vcov(bn))))))
  
  bn <- annualBenchmark(hfserie = mensualts,
                        lfserie = annualts,
                        include.differenciation = TRUE,set.coeff = 3)
  stderror <- se(bn)
  stderror <- stderror[!is.na(stderror)]
  expect_equal(unname(stderror),unname(sqrt(abs(diag(vcov(bn))))))
})

test_that("mts works",{
  bn <- twoStepsBenchmark(ts(matrix(rnorm(900,0,100) ,ncol=3),start=c(2000,1),freq=12) %>%
                            `colnames<-`(c("a","b","c")),construction)
  expect_identical(names(coef(bn)),c("constant","a","b","c"))
})


test_that("The classes in the bn object are the good ones",{
  set.seed(18)
  mensualts <- ts(diffinv(rnorm(120,1,1)),start=2010,freq=12)
  trimts <- ts(diffinv(rnorm(36,12,1)),start=2010,freq=4)
  
  bn <- twoStepsBenchmark(hfserie = mensualts,
                          lfserie = trimts,
                          include.differenciation = TRUE)
  expect_s3_class(bn,"twoStepsBenchmark")
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
  residextrap <- window(residuals(bn),start=start(asp),end=end(asp),extend=TRUE)
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
  
  set.seed(27)
  mensualts <- ts(diffinv(rnorm(120,1,1)),start=2010,freq=12)
  trimts <- ts(diffinv(rnorm(36,12,1)),start=2010,freq=4)
  expect_error(annualBenchmark(mensualts,trimts),
               "annual time-serie")
  
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
                                      include.rho=FALSE,
                                      include.differenciation = TRUE,
                                      set_coefficients = numeric(),
                                      cl = NULL),
               regexp = "Not a matrix")
})