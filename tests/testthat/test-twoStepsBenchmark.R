test_that("regression calculates the right coeffs", {
  set.seed(5)
  mensualts <- ts(diffinv(rnorm(120,1,1)),start=2010,freq=12)
  annualts <- ts(diffinv(rnorm(9,12,1)),start=2010,freq=1)
  
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.differenciation = FALSE))),
               c(-4.42319837,0.07996253))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.differenciation = FALSE,
                                    set.const=-3))),
               c(-3,0.07851836))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.differenciation = FALSE,
                                    set.const=10))),
               c(10,0.06532678))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.differenciation = FALSE,
                                    set.coeff=-3))),
               c(2264.80095,-3))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.differenciation = FALSE,
                                    set.coeff=10))),
               c(-7313.2097,10))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.differenciation = FALSE,
                                    set.const=10))),
               c(10.00000000,0.06532678))
  
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.differenciation = TRUE))),
               c(13.26837045,-0.008573728))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.differenciation = TRUE,
                                    set.const=-3))),
               c(-3,0.09641138))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.differenciation = TRUE,
                                    set.const=10))),
               c(10,0.01251813))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.differenciation = TRUE,
                                    set.coeff=-3))),
               c(458.89076,-3))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.differenciation = TRUE,
                                    set.coeff=10))),
               c(-1477.6741,10))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.differenciation = TRUE,
                                    set.const=10))),
               c(10.00000000,0.01251813))

  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.differenciation = TRUE))),
               c(13.26837045,-0.008573728))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.differenciation = TRUE,
                                    set.const=-3))),
               c(-3,0.09641138))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.differenciation = TRUE,
                                    set.const=10))),
               c(10,0.01251813))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.differenciation = TRUE,
                                    set.coeff=-3))),
               c(458.89076,-3))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.differenciation = TRUE,
                                    set.coeff=10))),
               c(-1477.6741,10))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.differenciation = TRUE,
                                    set.const=10))),
               c(10.00000000,0.01251813))
  
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.rho = TRUE))),
               c(-4.39884838,0.07989426))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.rho = TRUE,
                                    set.const=-3))),
               c(-3,0.07853607))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.rho = TRUE,
                                    set.const=10))),
               c(10,0.06933154))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.rho = TRUE,
                                    set.coeff=-3))),
               c(2259.1761334,-3))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.rho = TRUE,
                                    set.coeff=10))),
               c(-7296.1085794,10))
  expect_equal(unname(coef(annualBenchmark(hfserie = mensualts,
                                    annualserie = annualts,
                                    include.rho = TRUE,
                                    set.const=10))),
               c(10,0.06933154))
})
test_that("twoStepsBenchmark works",
          {
  set.seed(5)
  mensualts <- ts(diffinv(rnorm(40,1,1)),start=c(2010,3),freq=12)
  annualts <- ts(diffinv(rnorm(2,12,1)),start=2011,freq=1)
  bn <- twoStepsBenchmark(mensualts,annualts,include.differenciation = TRUE)
  expect_equal(as.ts(bn),ts(c(NA,NA,NA,NA,NA,NA,
                              NA,NA,NA,NA,-0.571214149938883,-0.512042896613151,
                              -0.401403059531388,-0.282289055895422,-0.1842054898379,-0.0620389824403372,0.0388535880411178,0.152633567733004,
                              0.307009342572201,0.403471799610014,0.513849273402788,0.597376062897958,0.681519482104183,0.750707543517581,
                              0.836552385594653,0.917604677945365,1.02394039945798,1.08619662101594,1.1444476679459,1.2536041237055,
                              1.36553065084533,1.44672101263651,1.50638242044474,1.53685338426944,1.58980498562774,1.63672356191596,
                              1.69700344611747,1.80584616418847,1.93942728268172,2.06756060229733,2.15632568100171),start=c(2010,3),frequency=12))
})
test_that("standard errors are the same that the vcov diag",{
  set.seed(5)
  mensualts <- ts(diffinv(rnorm(120,1,1)),start=2010,freq=12)
  annualts <- ts(diffinv(rnorm(9,12,1)),start=2010,freq=1)
  
  bn <- annualBenchmark(hfserie = mensualts,
                        annualserie = annualts,
                        include.differenciation = FALSE)
  stderror <- se(bn)
  stderror <- stderror[!is.na(stderror)]
  expect_equal(unname(stderror),unname(sqrt(abs(diag(vcov(bn))))))
  
  set.seed(20)
  mensualts <- ts(diffinv(rnorm(240,1,1)),start=2010,freq=12)
  annualts <- ts(diffinv(rnorm(18,12,1)),start=2010,freq=1)
  bn <- annualBenchmark(hfserie = mensualts,
                        annualserie = annualts,
                        include.differenciation = TRUE)
  stderror <- se(bn)
  stderror <- stderror[!is.na(stderror)]
  expect_equal(unname(stderror),unname(sqrt(abs(diag(vcov(bn))))))
  
  bn <- annualBenchmark(hfserie = mensualts,
                        annualserie = annualts,
                        include.differenciation = TRUE,set.coeff = 3)
  stderror <- se(bn)
  stderror <- stderror[!is.na(stderror)]
  expect_equal(unname(stderror),unname(sqrt(abs(diag(vcov(bn))))))
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
  expect_true(is.numeric(bn$model.list$start.coeff.calc))
  expect_true(is.numeric(bn$model.list$end.coeff.calc))
  
  expect_true(is.numeric(bn$regression$coefficients))
  expect_true(is.ts(bn$regression$residuals))
  expect_true(is.ts(bn$regression$fitted.values))
  expect_true(is.numeric(bn$regression$se))
  expect_true(is.numeric(bn$regression$df.residual))
  expect_true(is.numeric(bn$regression$rho))
  expect_true(is.ts(bn$regression$residuals.decorrelated))
  expect_true(is.ts(bn$regression$fitted.values.decorrelated))
})