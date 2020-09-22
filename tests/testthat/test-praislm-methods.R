test_that("print praislm", {
  benchmark <- twoStepsBenchmark(hfserie = turnover,
                                 lfserie = construction,
                                 include.differenciation = TRUE,
                                 include.rho = TRUE,
                                 set.const = pi^2)
  expect_known_output(print(prais(benchmark)),"outputs/prais.txt",update=FALSE)
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
