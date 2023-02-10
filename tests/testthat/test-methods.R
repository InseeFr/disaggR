test_that("print praislm", {
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = TRUE,
                                 include.rho = TRUE,
                                 set.const = pi^2)
  expect_snapshot_output(print(prais(benchmark), digits = 4L),cran = FALSE)
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = TRUE,
                                 include.rho = FALSE,
                                 set.coeff = exp(15))
  sm <- summary(prais(benchmark))
  expect_equal(sm$r.squared,0.5000000257)
  expect_equal(prais(benchmark)$fitted.values.decorrelated,
               ts(c(84212471.2,-19895530.4,370363.4,64479501.0,46574097.4,199097697.6,
                    231003846.2,135283655.8,-331155574.2,-169950996.9,90566640.7,-67288426.5,
                    -123703311.5,-238436888.4,-219277486.0,-130769308.8,153197380.2,70785592.9,  
                    224906383.1),start=2001,frequency=1))
  expect_equal(prais(benchmark)$residuals.decorrelated,
               ts(c(-84212465.7,19895533.8,-370354.8,-64479487.6,-46574085.3,-199097679.8,
                    -231003828.7,-135283645.5,331155559.0,169950995.2,-90566630.3,67288425.9,
                    123703311.7,238436884.6,219277478.2,130769309.3,-153197367.6,-70785583.2,
                    -224906369.3),start=2001,frequency=1))
  
  expect_output(print(summary(prais(benchmark))),"^\nCall:\ntwoStepsBenchmark\\(hfserie = turnover, lfserie = construction(.*?)The model includes a differenciation.")
})

test_that("print threeRuleSmooth",{
  expect_snapshot_output(print(threeRuleSmooth(turnover,construction),
                               digits = 4L),
                         cran = FALSE)
})

test_that("methods tests", {
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = TRUE,
                                 include.rho = TRUE,
                                 set.const = pi^2)
  expect_s3_class(prais(benchmark),"praislm")
  expect_s3_class(as.ts(benchmark),"ts")
  expect_equal(frequency(as.ts(benchmark)),frequency(turnover))
  expect_s3_class(residuals(benchmark),"ts")
  expect_equal(frequency(residuals(benchmark)),frequency(construction))
  expect_output(print(summary(benchmark)),"^\nCall:\ntwoStepsBenchmark\\(hfserie = turnover, lfserie = construction")
  expect_snapshot_output(print(benchmark, digits = 4L),cran = FALSE)
  
  digits_save <- getOption("digits")
  options(digits = 4L)
  expect_snapshot_output(show(benchmark),cran = FALSE)
  expect_snapshot_output(show(threeRuleSmooth(turnover,construction)),
                         cran = FALSE)
  options(digits = digits_save)
  
  a <- diff(aggregate(smoothed.part(benchmark)))
  b <- residuals(benchmark)
  a <- window(a,end=end(b))
  expect_equal(a,b)
  
  a <- smoothed.part(benchmark)
  b <- as.ts(benchmark)-fitted(benchmark)
  a <- window(a,end=end(b))
  expect_equal(a,b)
  
  bn<- twoStepsBenchmark(turnover,construction,start.coeff.calc=2001,end.coeff.calc=2015,start.benchmark=2002,end.benchmark=2018,start.domain = 2000,end.domain=c(2020,12))
  m <- model.list(bn)
  expect_equal(m$start.coeff.calc,2001)
  expect_equal(m$end.coeff.calc,2015)
  expect_equal(m$start.benchmark,2002)
  expect_equal(m$end.benchmark,2018)
  expect_equal(m$start.domain,2000)
  expect_equal(m$end.domain,c(2020,12))
})

test_that("as.list",
          {
            benchmark <- twoStepsBenchmark(turnover,construction)
            
            expect_identical(benchmark,
                             new("twoStepsBenchmark",as.list(benchmark)))
            
            benchmark <- threeRuleSmooth(turnover,construction)
            
            expect_identical(benchmark,
                             new("threeRuleSmooth",as.list(benchmark)))
          })

test_that("smoothed.rate",{
  expect_true(all(abs(smoothed.rate(threeRuleSmooth(turnover,aggregate(turnover)*3))-3)<10^-5))
})

test_that("Math group generic",{
  expect_identical(abs(twoStepsBenchmark(turnover,construction)),
                   abs(as.ts(twoStepsBenchmark(turnover,construction))))
  expect_identical(abs(threeRuleSmooth(turnover,construction)),
                   abs(as.ts(threeRuleSmooth(turnover,construction))))
})

test_that("Math2 group generic",{
  expect_identical(round(twoStepsBenchmark(turnover,construction),3),
                   round(as.ts(twoStepsBenchmark(turnover,construction)),3))
  expect_identical(round(threeRuleSmooth(turnover,construction),3),
                   round(as.ts(threeRuleSmooth(turnover,construction)),3))
})

test_that("Math2 group generic",{
  expect_identical(round(twoStepsBenchmark(turnover,construction),3),
                   round(as.ts(twoStepsBenchmark(turnover,construction)),3))
  expect_identical(round(threeRuleSmooth(turnover,construction),3),
                   round(as.ts(threeRuleSmooth(turnover,construction)),3))
})

test_that("Ops group generic",{
  tsnewobject_a <- twoStepsBenchmark(turnover,construction)
  tsnewobject_b <- threeRuleSmooth(turnover,construction)
  tsnewobject_c <- turnover
  
  expect_identical(tsnewobject_a+tsnewobject_a,
                   as.ts(tsnewobject_a)+as.ts(tsnewobject_a))
  expect_identical(tsnewobject_b+tsnewobject_b,
                   as.ts(tsnewobject_b)+as.ts(tsnewobject_b))
  expect_identical(tsnewobject_a+tsnewobject_b,
                   as.ts(tsnewobject_a)+as.ts(tsnewobject_b))
  expect_identical(tsnewobject_b+tsnewobject_a,
                   as.ts(tsnewobject_a)+as.ts(tsnewobject_b))
  
  expect_identical(tsnewobject_c+tsnewobject_a,
                   as.ts(tsnewobject_a)+tsnewobject_c)
  expect_identical(tsnewobject_a+tsnewobject_c,
                   as.ts(tsnewobject_a)+tsnewobject_c)
  expect_identical(tsnewobject_c+tsnewobject_b,
                   as.ts(tsnewobject_b)+tsnewobject_c)
  expect_identical(tsnewobject_b+tsnewobject_c,
                   as.ts(tsnewobject_b)+tsnewobject_c)
  
  expect_identical(-tsnewobject_a,
                   -as.ts(tsnewobject_a))
  expect_identical(tsnewobject_a+1,
                   as.ts(tsnewobject_a)+1)
  expect_identical(tsnewobject_b+1,
                   as.ts(tsnewobject_b)+1)
  expect_identical(1+tsnewobject_a,
                   as.ts(tsnewobject_a)+1)
  expect_identical(1+tsnewobject_b,
                   as.ts(tsnewobject_b)+1)
  
  expect_identical(tsnewobject_c+tsnewobject_c,
                   2*tsnewobject_c)
  # to ensure stats::Ops.ts has not been replaced
})

test_that("diverse ts methods",{
  benchmark <- twoStepsBenchmark(turnover,construction)
  smooth <- threeRuleSmooth(turnover,construction)
  
  expect_identical(aggregate(benchmark),
                   aggregate(as.ts(benchmark)))
  expect_identical(aggregate(smooth),
                   aggregate(as.ts(smooth)))
  
  expect_identical(cycle(benchmark),
                   cycle(as.ts(benchmark)))
  expect_identical(cycle(smooth),
                   cycle(as.ts(smooth)))
  
  expect_identical(diff(benchmark),
                   diff(as.ts(benchmark)))
  expect_identical(diff(smooth),
                   diff(as.ts(smooth)))
  
  expect_identical(time(benchmark),
                   time(as.ts(benchmark)))
  expect_identical(time(smooth),
                   time(as.ts(smooth)))
  
  expect_identical(diffinv(benchmark,xi = 0),
                   diffinv(as.ts(benchmark),xi = 0))
  expect_identical(diffinv(smooth,xi = 0),
                   diffinv(as.ts(smooth),xi = 0))
  
  expect_identical(na.omit(benchmark),
                   na.omit(as.ts(benchmark)))
  expect_identical(na.omit(smooth),
                   na.omit(as.ts(smooth)))
  
  expect_identical(window(benchmark,start=2001,end=c(2014,3)),
                   window(as.ts(benchmark),start=2001,end=c(2014,3)))
  expect_identical(window(smooth,start=2001,end=c(2014,3)),
                   window(as.ts(smooth),start=2001,end=c(2014,3)))
})

test_that("monthplot ts method",{
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if(
    any(
      grepl("openblas",
            as.character(sessionInfo()[c("BLAS","LAPACK")]))
    )
  )
  expect_doppelganger <- vdiffr::expect_doppelganger
  benchmark <- twoStepsBenchmark(turnover,construction)
  smooth <- threeRuleSmooth(turnover,construction)
  expect_doppelganger("monthplot-twoStepsBenchmark",
                      function() monthplot(benchmark))
  expect_doppelganger("monthplot-threeRuleSmooth",
                      function() monthplot(smooth))
})

test_that("outliers",{
  benchmark <- twoStepsBenchmark(turnover,construction)
  expect_null(outliers(benchmark))
  expect_null(outliers(benchmark,as.ts = TRUE))
  expect_null(outliers(prais(benchmark)))
  expect_null(outliers(prais(benchmark),as.ts = TRUE))
  
  benchmark <- twoStepsBenchmark(turnover,construction,
                                 outliers = list(AO2005T1=rep(0.1,12)))
  expect_identical(outliers(benchmark),list(AO2005T1=rep(0.1,12)))
  expect_equal(outliers(benchmark,as.ts = TRUE),
               structure(
                 ts(c(rep(0,60L),
                      rep(0.1,12L),
                      rep(0,173L)),
                    frequency = 12L,
                    start = 2000),
                 dim = c(245L,1L),
                 dimnames = list(NULL,
                                 c("AO2005T1"))))
  
  expect_identical(outliers(prais(benchmark)),list(AO2005T1=rep(0.1,12)))
  expect_equal(outliers(prais(benchmark),as.ts = TRUE),
               structure(
                 ts(c(rep(0,5L),
                      1.2,
                      rep(0,14L)),
                    frequency = 1L,
                    start = 2000),
                 dim = c(20L,1L),
                 dimnames = list(NULL,
                                 c("AO2005T1"))))
})
