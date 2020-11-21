test_that("params for other things than shiny test",{
  expect_identical(csspresetplot(),"{height: calc(100vh - 104px);width: calc(50vw - 39px);}")
  expect_identical(cssmainoutwithtitle(),"{height: calc(100vh - 158px);}")
  expect_identical(cssmainoutwithouttitle (),"{height: calc(100vh - 138px);}")
})

test_that("switch window", {
  tsexample <- ts(1:10,start=2010,freq=12)
  tspex <- tsp(tsexample)
  expect_identical(switch_window(2010,2011.4,tspex),
                   c(2010,2011.4))
  expect_identical(switch_window(c(2010,4),NULL,tspex),
                   c(2010.25,tspex[2L]))
  expect_identical(switch_window(NULL,2011.4,tspex),
                   c(tspex[1L],2011.4))
})

test_that("get clean wins",{
  benchmark <- twoStepsBenchmark(turnover,construction,
                                 start.coeff.calc = 2000,
                                 end.coeff.calc = 2018,
                                 start.benchmark = 2005,
                                 end.benchmark = 2017,
                                 start.domain = c(2005,7),
                                 end.domain = c(2017,7))
  expect_identical(get_clean_wins(benchmark),
                   list(benchmark=c(2005,2017),
                        coeff.calc=c(2000,2018),
                        domain=c(2005.5,2017.5)))
})

test_that("reView output class",{
  benchmark <- twoStepsBenchmark(turnover,construction,
                                 start.coeff.calc = 2000,
                                 end.coeff.calc = 2018,
                                 start.benchmark = 2005,
                                 end.benchmark = 2017,
                                 start.domain = c(2005,7),
                                 end.domain = c(2017,7))
  produced <- reViewOutput(benchmark,benchmark,compare=TRUE)
  expected <-   structure(list(benchmark = benchmark,
                               benchmark_old = benchmark,
                               hfserie_name = "turnover",
                               lfserie_name = "construction",
                               compare = TRUE),
                          class="reViewOutput")
  expect_identical(produced,expected)
})

test_that("presets list fun",{
  produced <- presets_list_fun(turnover,construction)
  
  presets_list <- list(twoStepsBenchmark(turnover,construction,include.differenciation = TRUE),
                       twoStepsBenchmark(turnover,construction,include.differenciation = TRUE,
                                         set.const = 0),
                       twoStepsBenchmark(turnover,construction),
                       twoStepsBenchmark(turnover,construction,include.rho = TRUE),
                       twoStepsBenchmark(turnover,construction,set.const = 0),
                       twoStepsBenchmark(turnover,construction,set.const = 0,include.rho = TRUE))
  expected <- lapply(presets_list,in_sample)
  expect_identical(produced,expected)
})

test_that("rePort produces a report",{
  benchmark <- twoStepsBenchmark(turnover,construction)
  temp_dir <- tempdir()
  temp_html <- tempfile("test",temp_dir,".html")
  rePort(benchmark,output_file = temp_html)
  expect_true(file.exists(temp_html))
  expect_identical(unlink(temp_html),0L)
  rePort(reViewOutput(benchmark,benchmark,compare = TRUE),output_file = temp_html)
  expect_true(file.exists(temp_html))
  unlink(temp_html)
})

test_that("reView",{
  skip_on_cran()
  skip_on_os(c("mac","linux","solaris"))
    # Only check on windows (otherwise the plots SHA1 differ)
  shinytest::expect_pass(shinytest::testApp(testthat::test_path("./shiny"),
                                            compareImages = FALSE,
                                            interactive = FALSE))
  # To edit shinytest, go in the shiny/tests/shinytest.R
  # And launch
  # shinytest::testApp(testthat::test_path("./shiny"), compareImages = FALSE)
  # to update
})

