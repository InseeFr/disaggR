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

test_that("display_vector", {
  expect_identical(display_vector(rep(0.1,12)),
                   "c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)")
  expect_identical(display_vector(2),
                   "2")
  expect_identical(display_vector(c(a=1)),
                   "c(a=1)")
  expect_identical(display_vector(c(a=1,b=2)),
                   "c(a=1,b=2)")
  expect_identical(display_outliers(list(a=c(0.1,0.1))),
                   "list(a=c(0.1,0.1))")
  expect_identical(display_outliers(list(a=c(0.1,0.1),
                                         b=0.3)),
                   "list(a=c(0.1,0.1),b=0.3)")
})

test_that("get_preset", {
  expect_equal(get_preset(twoStepsBenchmark(turnover,construction,include.differenciation = TRUE)),1)
  expect_equal(get_preset(twoStepsBenchmark(turnover,construction,include.differenciation = TRUE,
                                            set.const = 0)),2)
  expect_equal(get_preset(twoStepsBenchmark(turnover,construction)),3)
  expect_equal(get_preset(twoStepsBenchmark(turnover,construction,include.rho = TRUE)),4)
  expect_equal(get_preset(twoStepsBenchmark(turnover,construction,set.const = 0)),5)
  expect_equal(get_preset(twoStepsBenchmark(turnover,construction,
                                            include.rho = TRUE,
                                            set.const = 0)),6)
  
  expect_equal(get_preset(twoStepsBenchmark(turnover,construction,
                                            outliers = list(LS2003=rep(0.1,12)),
                                            include.differenciation = TRUE)),1)
  expect_true(is.na(get_preset(twoStepsBenchmark(turnover,construction,
                                                 outliers = list(LS2003=rep(0.1,12)),
                                                 set.coeff = c(LS2003=1),
                                                 include.differenciation = TRUE))))
  expect_equal(get_preset(twoStepsBenchmark(turnover,construction,include.differenciation = TRUE,
                                            set.const = 0)),2)
  expect_equal(get_preset(twoStepsBenchmark(turnover,construction)),3)
  expect_equal(get_preset(twoStepsBenchmark(turnover,construction,include.rho = TRUE)),4)
  expect_equal(get_preset(twoStepsBenchmark(turnover,construction,set.const = 0)),5)
  expect_equal(get_preset(twoStepsBenchmark(turnover,construction,
                                            include.rho = TRUE,
                                            set.const = 0)),6)
  
  expect_true(is.na(get_preset(twoStepsBenchmark(turnover,construction,
                                                 include.rho = TRUE,include.differenciation = TRUE))))
  expect_true(is.na(get_preset(twoStepsBenchmark(turnover,construction,set.coeff = 1))))
  expect_true(is.na(get_preset(twoStepsBenchmark(turnover,construction,set.const = 1))))
})

test_that("info switch",{
  expect_s3_class(info_switch("Benchmark plot"),"html")
  expect_s3_class(info_switch("Scatter plot"),"html")
  expect_s3_class(info_switch("In-sample predictions"),"html")
  expect_s3_class(info_switch("Benchmark summary"),"html")
  expect_s3_class(info_switch("Comparison benchmark/input"),"html")
  expect_s3_class(info_switch("Revisions"),"html")
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
                               hfserie_name = as.symbol("turnover"),
                               lfserie_name = as.symbol("construction"),
                               compare = TRUE),
                          class="reViewOutput")
  expect_identical(produced,expected)
})

test_that("presets list fun",{
  produced <- presets_list_fun(turnover,construction)
  
  expected <- list(twoStepsBenchmark(turnover,construction,include.differenciation = TRUE),
                   twoStepsBenchmark(turnover,construction,include.differenciation = TRUE,
                                     set.const = 0),
                   twoStepsBenchmark(turnover,construction),
                   twoStepsBenchmark(turnover,construction,include.rho = TRUE),
                   twoStepsBenchmark(turnover,construction,set.const = 0),
                   twoStepsBenchmark(turnover,construction,set.const = 0,include.rho = TRUE))
  
  expect_identical(lapply(produced,in_sample),lapply(expected,in_sample))
})

test_that("rePort produces a report",{
  skip_on_cran()
  testthat::skip_if_not_installed("rmarkdown")
  browser <- options(browser=function(url) message(url))
  on.exit(options(browser))
  
  benchmark <- twoStepsBenchmark(turnover,construction)
  
  temp_dir <- tempdir()
  temp_html <- tempfile("test",temp_dir,".html")
  expect_message(rePort(benchmark,output_file = temp_html, launch.browser = TRUE))
  expect_true(file.exists(temp_html))
  out_html <- readLines(temp_html)
  expect_true(any(vapply(X = out_html,FUN = function(x) grepl("turnover on construction",x),FUN.VALUE = TRUE,USE.NAMES = FALSE)))
  unlink(temp_html)
  
  rePort(reViewOutput(benchmark,benchmark,compare = TRUE),output_file = temp_html)
  expect_true(file.exists(temp_html))
  out_html <- readLines(temp_html)
  expect_true(any(vapply(X = out_html,FUN = function(x) grepl("turnover on construction",x),FUN.VALUE = TRUE,USE.NAMES = FALSE)))
  unlink(temp_html)
  
  temp_rds <- tempfile("test",temp_dir,".rds")
  saveRDS(twoStepsBenchmark(turnover,construction),temp_rds)
  expect_message(url <- rePort(temp_rds))
  out_html <- readLines(url)
  expect_true(any(vapply(X = out_html,FUN = function(x) grepl("turnover on construction",x),FUN.VALUE = TRUE,USE.NAMES = FALSE)))
  unlink(url)
  
  con <- gzcon(gzfile(temp_rds))
  expect_message(url <- rePort(con))
  expect_true(file.exists(url))
  out_html <- readLines(url)
  expect_true(any(vapply(X = out_html,FUN = function(x) grepl("turnover on construction",x),FUN.VALUE = TRUE,USE.NAMES = FALSE)))
  unlink(url)
  
  expect_message(url <- print(reViewOutput(benchmark,benchmark,FALSE)))
  out_html <- readLines(url)
  expect_true(any(vapply(X = out_html,FUN = function(x) grepl("turnover on construction",x),FUN.VALUE = TRUE,USE.NAMES = FALSE)))
  unlink(url)
})

test_that("names args changes the name on rePort",{
  skip_on_cran()
  testthat::skip_if_not_installed("rmarkdown")
  browser <- options(browser=function(url) message(url))
  on.exit(options(browser))
  
  benchmark <- twoStepsBenchmark(turnover,construction)
  
  temp_dir <- tempdir()
  temp_html <- tempfile("test",temp_dir,".html")
  expect_message(rePort(benchmark,output_file = temp_html, launch.browser = TRUE,
                        hfserie_name = "testhf",
                        lfserie_name = "testlf"))
  expect_true(file.exists(temp_html))
  out_html <- readLines(temp_html)
  expect_true(any(vapply(X = out_html,FUN = function(x) grepl("testhf on testlf",x),FUN.VALUE = TRUE,USE.NAMES = FALSE)))
  unlink(temp_html)
  
  rePort(reViewOutput(benchmark,benchmark,compare = TRUE),
         output_file = temp_html,
         hfserie_name = "testhf",
         lfserie_name = "testlf")
  expect_true(file.exists(temp_html))
  out_html <- readLines(temp_html)
  expect_true(any(vapply(X = out_html,FUN = function(x) grepl("testhf on testlf",x),FUN.VALUE = TRUE,USE.NAMES = FALSE)))
  unlink(temp_html)
  
  temp_rds <- tempfile("test",temp_dir,".rds")
  saveRDS(twoStepsBenchmark(turnover,construction),temp_rds)
  expect_message(url <- rePort(temp_rds,
                               hfserie_name = "testhf",
                               lfserie_name = "testlf"))
  out_html <- readLines(url)
  expect_true(any(vapply(X = out_html,FUN = function(x) grepl("testhf on testlf",x),FUN.VALUE = TRUE,USE.NAMES = FALSE)))
  unlink(url)
  
  con <- gzcon(gzfile(temp_rds))
  expect_message(url <- rePort(con,
                               hfserie_name = "testhf",
                               lfserie_name = "testlf"))
  expect_true(file.exists(url))
  out_html <- readLines(url)
  expect_true(any(vapply(X = out_html,FUN = function(x) grepl("testhf on testlf",x),FUN.VALUE = TRUE,USE.NAMES = FALSE)))
  unlink(url)
  
  expect_message(url <- print(reViewOutput(benchmark,benchmark,FALSE),
                              hfserie_name = "testhf",
                              lfserie_name = "testlf"))
  out_html <- readLines(url)
  expect_true(any(vapply(X = out_html,FUN = function(x) grepl("testhf on testlf",x),FUN.VALUE = TRUE,USE.NAMES = FALSE)))
  unlink(url)
})

test_that("rePort produces a report when time boundaries are set",{
  skip_on_cran()
  testthat::skip_if_not_installed("rmarkdown")
  browser <- options(browser=function(url) message(url))
  on.exit(options(browser))
  
  turnover_2001 <- window(turnover, start = 2001)
  benchmark <- twoStepsBenchmark(turnover_2001, construction,
                                 start.domain = 2001,
                                 start.coeff.calc = 2001,
                                 start.benchmark = 2001)
  
  temp_dir <- tempdir()
  temp_html <- tempfile("test",temp_dir,".html")
  expect_message(rePort(benchmark,output_file = temp_html, launch.browser = TRUE))
  expect_true(file.exists(temp_html))
  unlink(temp_html)
})

test_that("reView-withoutset",{
  
  # important : the package should have been rebuilt for these tests
  # (ie with installed and restart in R Studio not loaded with devtools)
  
  skip_on_cran() # no shinytest2 on cran
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if(isTRUE(as.logical(Sys.getenv("CI"))) &&
                      tolower(Sys.info()[["sysname"]]) == "windows")
  # Windows has some problems on CI with shinytest2
  
  app <- shinytest2::AppDriver$new(test_path("shiny-withoutset"),
                                   wait = TRUE)
  
  expect_identical(app$get_js("window.document.title;"),"reView")
  
  get_bn <- function() app$get_values()$export$`reView-reViewtab2-new_bn`
  
  # First tab
  app$set_window_size(800,600)
  model1 <- app$wait_for_value(output = "reView-reViewtab1-model1_plot")
  model2 <- app$wait_for_value(output = "reView-reViewtab1-model2_plot")
  model3 <- app$wait_for_value(output = "reView-reViewtab1-model3_plot")
  model4 <- app$wait_for_value(output = "reView-reViewtab1-model4_plot")
  model5 <- app$wait_for_value(output = "reView-reViewtab1-model5_plot")
  model6 <- app$wait_for_value(output = "reView-reViewtab1-model6_plot")
  
  models <- list(model1,model2,model3,
                 model4,model5,model6)
  
  expect_true(all(vapply(models,`[[`,0,"height") >= 132))
  expect_true(all(vapply(models,`[[`,0,"height") <= 173))
  
  expect_equal(as.ts(get_bn()),as.ts(twoStepsBenchmark(turnover,construction)))
  
  # Click on a model changes navbar
  app$set_inputs(`reView-reViewtab1-model1_plotclick` = 10L,
                 allow_no_input_binding_ = TRUE)
  slidercoeffcalc <- app$wait_for_value(input = "reView-reViewtab2-coeffcalc",
                                        timeout=5000)
  sliderbenchmark <- app$wait_for_value(input = "reView-reViewtab2-benchmark")
  sliderplots <- app$wait_for_value(input = "reView-reViewtab2-plotswin")
  newplot <- app$wait_for_value(output = "reView-reViewtab2-newplot")
  oldplot <-app$wait_for_value(output = "reView-reViewtab2-oldplot")
  expect_equal(app$wait_for_value(input = "reView-menu"),"Modify")
  expect_equal(slidercoeffcalc,c(2000,2019))
  expect_equal(sliderbenchmark,c(2000,2019))
  expect_equal(sliderplots,c(2000,2020))
  
  set_coeff <- app$wait_for_value(input = "reView-reViewtab2-setcoeff")
  set_const <- app$wait_for_value(input = "reView-reViewtab2-setconst")
  
  expect_equal(set_coeff,1)
  expect_equal(set_const,0)
  
  set_coeff_button <- app$wait_for_value(input = "reView-reViewtab2-setcoeff_button")
  set_const_button <- app$wait_for_value(input = "reView-reViewtab2-setconst_button")
  
  expect_equal(set_coeff_button,FALSE)
  expect_equal(set_const_button,FALSE)
  
  plots <- list(newplot,oldplot)
  
  expect_true(all(vapply(plots,`[[`,0,"height") >= 432))
  expect_true(all(vapply(plots,`[[`,0,"height") <= 452))
  expect_true(all(vapply(plots,`[[`,0,"width") >= 269))
  
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction,
                                       include.differenciation = TRUE)))
  
  # info button shows scatter plots modal
  
  app$set_inputs(`reView-reViewtab2-infobtn` = "click")
  app$wait_for_js("($('#shiny-modal').data('bs.modal') || {}).isShown")
  expect_true(grepl("These scatter plots",
                    app$get_html(".modal"),
                    fixed = TRUE))
  expect_true(grepl("reset plot window",
                    app$get_html(".modal"),
                    fixed = TRUE))
  app$get_js("$('.modal').modal('hide');")
  
  # Differenciation
  
  app$set_inputs(`reView-reViewtab3-Reset` = "click",
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`reView-reViewtab2-dif` = TRUE)
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction,
                                       include.differenciation = TRUE)))
  # Rho
  app$set_inputs(`reView-reViewtab3-Reset` = "click",
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`reView-reViewtab2-rho` = TRUE)
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction,
                                       include.rho = TRUE)))
  
  # Setcoeff
  app$set_inputs(`reView-reViewtab3-Reset` = "click",
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`reView-reViewtab2-setcoeff_button` = TRUE)
  app$set_inputs(`reView-reViewtab2-setcoeff` = 100)
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction,
                                       set.coeff = 100)))
  
  # Setconst
  app$set_inputs(`reView-reViewtab3-Reset` = "click",
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`reView-reViewtab2-setconst_button` = TRUE)
  app$set_inputs(`reView-reViewtab2-setconst` = 100)
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction,
                                       set.const = 100)))
  
  # empty numerics -> 0 and and active only if pressed
  app$set_inputs(`reView-reViewtab3-Reset` = "click",
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`reView-reViewtab2-setconst` = NULL)
  app$set_inputs(`reView-reViewtab2-setcoeff` = NULL)
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction)))
  app$set_inputs(`reView-reViewtab2-setconst_button` = TRUE)
  app$set_inputs(`reView-reViewtab2-setcoeff_button` = TRUE)
  
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction,
                                       set.coeff = 0,
                                       set.const = 0)))
  
  # coeffcalc
  app$set_inputs(`reView-reViewtab3-Reset` = "click",
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`reView-reViewtab2-coeffcalc` = c(2004, 2012))
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction,
                                       start.coeff.calc = 2004,
                                       end.coeff.calc = 2012)))
  
  # Benchmark
  app$set_inputs(`reView-reViewtab3-Reset` = "click",
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`reView-reViewtab2-benchmark` = c(2004, 2015))
  
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction,
                                       start.benchmark = 2004,
                                       end.benchmark = 2015)))
  
  # Plots
  app$set_inputs(`reView-reViewtab2-plotswin` = as.numeric(c(2003, 2014)))
  expect_equal(app$get_value(input = "reView-reViewtab2-plotswin"),
               c(2003,2014))
  app$set_inputs(`reView-reViewtab2-click` = 1L,allow_no_input_binding_ = TRUE)
  expect_equal(app$get_value(input = "reView-reViewtab2-plotswin"),
               c(2000,2020))
  
  # Change output to benchmark plots
  expect_equal(app$wait_for_value(input = "reView-reViewtab2-mainout_choice"),
               "Scatter plot")
  app$set_inputs(`reView-reViewtab2-mainout_choice` = "Benchmark plot")
  
  newplot <- app$wait_for_value(output = "reView-reViewtab2-newplot")
  oldplot <- app$wait_for_value(output = "reView-reViewtab2-oldplot")
  
  plots <- list(newplot,oldplot)
  
  expect_true(all(vapply(plots,`[[`,0,"height") >= 432))
  expect_true(all(vapply(plots,`[[`,0,"height") <= 452))
  expect_true(all(vapply(plots,`[[`,0,"width") >= 269))
  
  # info button shows benchmark plots modal
  
  app$set_inputs(`reView-reViewtab2-infobtn` = "click")
  app$wait_for_js("($('#shiny-modal').data('bs.modal') || {}).isShown")
  expect_true(grepl("procedure involved",
                    app$get_html(".modal"),
                    fixed = TRUE))
  expect_true(grepl("to change plot window",
                    app$get_html(".modal"),
                    fixed = TRUE))
  app$get_js("$('.modal').modal('hide');")
  
  # Change output to in sample
  expect_equal(app$wait_for_value(input = "reView-reViewtab2-mainout_choice"),
               "Benchmark plot")
  app$set_inputs(`reView-reViewtab2-mainout_choice` = "In-sample predictions")
  
  newplotlev <- app$wait_for_value(output = "reView-reViewtab2-newplotlev")
  oldplotlev <- app$wait_for_value(output = "reView-reViewtab2-oldplotlev")
  newplotcha <- app$wait_for_value(output = "reView-reViewtab2-newplotcha")
  oldplotcha <- app$wait_for_value(output = "reView-reViewtab2-oldplotcha")
  
  plots <- list(newplotlev,oldplotlev,
                newplotcha,oldplotcha)
  
  expect_true(all(vapply(plots,`[[`,0,"height") >= 211))
  expect_true(all(vapply(plots,`[[`,0,"height") <= 231))
  expect_true(all(vapply(plots,`[[`,0,"width") >= 269))
  
  # Summary
  app$set_inputs(`reView-reViewtab3-Reset` = "click",
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`reView-reViewtab2-mainout_choice` = "Benchmark summary")
  oldsum <- app$wait_for_value(output = "reView-reViewtab2-oldverbat")
  newsum <- app$wait_for_value(output = "reView-reViewtab2-newverbat")
  
  expect_equal(gsub("‘|’|'","",newsum),
               gsub("‘|’|'","",paste(capture.output(print(
                 summary(twoStepsBenchmark(turnover,construction)),
                 call = FALSE
               )),collapse="\n")))
  
  # Indicator
  app$set_inputs(`reView-reViewtab2-mainout_choice` = "Comparison benchmark/input")
  plotlev <- app$wait_for_value(output = "reView-reViewtab2-monoplotlev")
  plotcha <- app$wait_for_value(output = "reView-reViewtab2-monoplotcha")
  plotctb <- app$wait_for_value(output = "reView-reViewtab2-monoplotctb")
  
  plots <- list(plotlev,
                plotcha,
                plotctb)
  
  expect_true(all(vapply(plots,`[[`,0,"height") >= 132))
  expect_true(all(vapply(plots,`[[`,0,"height") <= 173))
  expect_true(all(vapply(plots,`[[`,0,"width") >= 590))
  expect_true(all(vapply(plots,`[[`,0,"width") <= 610))
  
  # Revisions
  app$set_inputs(`reView-reViewtab2-mainout_choice` = "Revisions")
  plotlev <- app$wait_for_value(output = "reView-reViewtab2-monoplotlev")
  plotcha <- app$wait_for_value(output = "reView-reViewtab2-monoplotcha")
  plotctb <- app$wait_for_value(output = "reView-reViewtab2-monoplotctb")
  
  expect_true(all(vapply(plots,`[[`,0,"height") >= 132))
  expect_true(all(vapply(plots,`[[`,0,"height") <= 173))
  expect_true(all(vapply(plots,`[[`,0,"width") >= 590))
  expect_true(all(vapply(plots,`[[`,0,"width") <= 610))
  
  # Reset change menu
  app$set_inputs(`reView-menu` = "Export")
  app$set_inputs(`reView-reViewtab3-Reset` = "click",
                 allow_no_input_binding_ = TRUE)
  app$wait_for_value(output = "reView-reViewtab2-monoplotlev")
  expect_equal(app$wait_for_value(input = "reView-menu"),"Modify")
  
  app$stop()
  
})

test_that("reView-setcoefconst",{
  
  # important : the package should have been rebuilt for these tests
  # (ie with installed and restart in R Studio not loaded with devtools)
  
  skip_on_cran() # no shinytest2 on cran
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if(isTRUE(as.logical(Sys.getenv("CI"))) &&
                      tolower(Sys.info()[["sysname"]]) == "windows")
  # Windows has some problems on CI with shinytest2
  
  app <- shinytest2::AppDriver$new(test_path("shiny-setcoefconst"),
                                   wait = TRUE)
  
  expect_identical(app$get_js("window.document.title;"),"reView")
  
  get_bn <- function() app$get_values()$export$`reView-reViewtab2-new_bn`
  
  app$set_window_size(800,600)
  app$wait_for_value(output = "reView-reViewtab1-model1_plot")
  app$wait_for_value(output = "reView-reViewtab1-model2_plot")
  app$wait_for_value(output = "reView-reViewtab1-model3_plot")
  app$wait_for_value(output = "reView-reViewtab1-model4_plot")
  app$wait_for_value(output = "reView-reViewtab1-model5_plot")
  app$wait_for_value(output = "reView-reViewtab1-model6_plot")
  
  expect_equal(as.ts(get_bn()),as.ts(twoStepsBenchmark(turnover,construction,
                                                       set.coeff = 1,
                                                       set.const = 0,
                                                       include.differenciation = TRUE,
                                                       include.rho = TRUE,
                                                       start.coeff.calc = 2005,
                                                       end.coeff.calc = 2015,
                                                       start.benchmark = 2004,
                                                       end.benchmark = 2018,
                                                       start.domain = 1990,
                                                       end.domain = c(2030,12))))
  # Test in second tab if the sets are OK
  app$set_inputs(`reView-menu` = "Modify")
  slidercoeffcalc <- app$wait_for_value(input = "reView-reViewtab2-coeffcalc",
                                        timeout=5000)
  sliderbenchmark <- app$wait_for_value(input = "reView-reViewtab2-benchmark")
  sliderplots <- app$wait_for_value(input = "reView-reViewtab2-plotswin")
  expect_equal(app$wait_for_value(input = "reView-menu"),"Modify")
  expect_equal(slidercoeffcalc,c(2005,2015))
  expect_equal(sliderbenchmark,c(2004,2018))
  expect_equal(sliderplots,c(2000,2020))
  
  set_coeff <- app$wait_for_value(input = "reView-reViewtab2-setcoeff")
  set_const <- app$wait_for_value(input = "reView-reViewtab2-setconst")
  
  expect_equal(set_coeff,1)
  expect_equal(set_const,0)
  
  set_coeff_button <- app$wait_for_value(input = "reView-reViewtab2-setcoeff_button")
  set_const_button <- app$wait_for_value(input = "reView-reViewtab2-setconst_button")
  
  expect_equal(set_coeff_button,TRUE)
  expect_equal(set_const_button,TRUE)
  
  # Back to first tab to check the summary table
  app$set_inputs(`reView-menu` = "Presets")
  
  app$wait_for_value(output = "reView-reViewtab1-model1_plot")
  app$wait_for_value(output = "reView-reViewtab1-model2_plot")
  app$wait_for_value(output = "reView-reViewtab1-model3_plot")
  app$wait_for_value(output = "reView-reViewtab1-model4_plot")
  app$wait_for_value(output = "reView-reViewtab1-model5_plot")
  app$wait_for_value(output = "reView-reViewtab1-model6_plot")
  
  app$set_inputs(`reView-reViewtab1-firsttab_choice`="Summary table")
  app$wait_for_value(input = "reView-reViewtab1-model1_actionlink")
  app$wait_for_value(input = "reView-reViewtab1-model2_actionlink")
  app$wait_for_value(input = "reView-reViewtab1-model3_actionlink")
  app$wait_for_value(input = "reView-reViewtab1-model4_actionlink")
  app$wait_for_value(input = "reView-reViewtab1-model5_actionlink")
  app$wait_for_value(input = "reView-reViewtab1-model6_actionlink")
  expect_true(grepl("distance",app$get_html(".tab-content"),fixed=TRUE))
  app$set_inputs(`reView-reViewtab1-model1_actionlink` = "click",
                 allow_no_input_binding_ = TRUE)
  expect_equal(app$wait_for_value(input = "reView-menu"),"Modify")
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction,
                                       include.differenciation = TRUE,
                                       start.coeff.calc = 2005,
                                       end.coeff.calc = 2015,
                                       start.benchmark = 2004,
                                       end.benchmark = 2018,
                                       start.domain = 1990,
                                       end.domain = c(2030,12))))
  
  app$click("reView-reViewtab3-Quit")
  app$.__enclos_env__$private$shiny_process$get_exit_status()
  for (i in 1:10) {
    Sys.sleep(0.3)
    if (! (app$.__enclos_env__$private$shiny_process$get_exit_status() %||% 1)) break
  }
  sortie_reView <- app$stop()
  expect_equal(as.ts(sortie_reView$benchmark),
               as.ts(twoStepsBenchmark(turnover,construction,
                                       include.differenciation = TRUE,
                                       start.coeff.calc = 2005,
                                       end.coeff.calc = 2015,
                                       start.benchmark = 2004,
                                       end.benchmark = 2018,
                                       start.domain = 1990,
                                       end.domain = c(2030,12))))

  expect_identical(sortie_reView$hfserie_name, quote(2*x+1))
  expect_identical(sortie_reView$lfserie_name, as.symbol("construction"))
  
})

test_that("reView-outliers",{
  
  # important : the package should have been rebuilt for these tests
  # (ie with installed and restart in R Studio not loaded with devtools)
  
  skip_on_cran() # no shinytest2 on cran
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if(isTRUE(as.logical(Sys.getenv("CI"))) &&
                      tolower(Sys.info()[["sysname"]]) == "windows")
  # Windows has some problems on CI with shinytest2
  
  app <- shinytest2::AppDriver$new(test_path("shiny-outliers"),
                                   options = list(shiny.reactlog = TRUE),
                                   wait = TRUE)
  
  expect_identical(app$get_js("window.document.title;"),"reView")
  
  get_bn <- function() app$get_values()$export$`reView-reViewtab2-new_bn`
  
  app$set_window_size(800,600)
  app$wait_for_value(output = "reView-reViewtab1-model1_plot")
  app$wait_for_value(output = "reView-reViewtab1-model2_plot")
  app$wait_for_value(output = "reView-reViewtab1-model3_plot")
  app$wait_for_value(output = "reView-reViewtab1-model4_plot")
  app$wait_for_value(output = "reView-reViewtab1-model5_plot")
  app$wait_for_value(output = "reView-reViewtab1-model6_plot")
  
  expect_equal(as.ts(get_bn()),as.ts(twoStepsBenchmark(turnover,construction,
                                                       outliers = list(AO2005 = rep(0.1,12L)))))
  
  app$set_inputs(`reView-menu` = "Export")
  expect_equal(app$wait_for_value(output = "reView-reViewtab3-newcall"),
               paste("twoStepsBenchmark(",
                     "hfserie = turnover,",
                     "lfserie = construction,",
                     "include.differenciation = FALSE,",
                     "include.rho = FALSE,",
                     "start.coeff.calc = 2000,",
                     "end.coeff.calc = 2019,",
                     "start.benchmark = 2000,",
                     "end.benchmark = 2019,",
                     "outliers = list(AO2005=c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1))\n)",sep = "\n\t"))
  expect_equal(app$wait_for_value(output = "reView-reViewtab3-oldcall"),
               paste("twoStepsBenchmark(",
                     "hfserie = turnover,",
                     "lfserie = construction,",
                     "include.differenciation = FALSE,",
                     "include.rho = FALSE,",
                     "outliers = list(AO2005=c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1))\n)",sep = "\n\t"))
  
  app$set_inputs(`reView-menu` = "Modify")
  app$set_inputs(`reView-reViewtab2-setcoeff_button` = TRUE)
  app$set_inputs(`reView-reViewtab2-setcoeff` = 100)
  app$set_inputs(`reView-menu` = "Export")
  expect_equal(app$wait_for_value(output = "reView-reViewtab3-newcall"),
               paste("twoStepsBenchmark(",
                     "hfserie = turnover,",
                     "lfserie = construction,",
                     "include.differenciation = FALSE,",
                     "include.rho = FALSE,",
                     "set.coeff = 100,",
                     "start.coeff.calc = 2000,",
                     "end.coeff.calc = 2019,",
                     "start.benchmark = 2000,",
                     "end.benchmark = 2019,",
                     "outliers = list(AO2005=c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1))\n)",sep = "\n\t"))
  
  app$click("reView-reViewtab3-Quit")
  app$.__enclos_env__$private$shiny_process$get_exit_status()
  for (i in 1:10) {
    Sys.sleep(0.3)
    if (! (app$.__enclos_env__$private$shiny_process$get_exit_status() %||% 1)) break
  }
  sortie_reView <- app$stop()
  expect_equal(as.ts(sortie_reView$benchmark),
               as.ts(twoStepsBenchmark(
                 hfserie = turnover, lfserie = construction, 
                 include.differenciation = FALSE, include.rho = FALSE, set.coeff = 100L, 
                 set.const = NULL, start.coeff.calc = 2000L, end.coeff.calc = 2019L, 
                 start.benchmark = 2000L, end.benchmark = 2019L, start.domain = NULL, 
                 end.domain = NULL,
                 outliers = list(AO2005 = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)))
  ))
  expect_identical(sortie_reView$hfserie_name, as.symbol("turnover"))
  expect_identical(sortie_reView$lfserie_name, as.symbol("construction"))
})

test_that("reView-outlierssetcoef",{
  
  # important : the package should have been rebuilt for these tests
  # (ie with installed and restart in R Studio not loaded with devtools)
  
  skip_on_cran() # no shinytest2 on cran
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if(isTRUE(as.logical(Sys.getenv("CI"))) &&
                      tolower(Sys.info()[["sysname"]]) == "windows")
  # Windows has some problems on CI with shinytest2
  
  app <- shinytest2::AppDriver$new(test_path("shiny-outlierssetcoef"),
                                   wait = TRUE)
  
  expect_identical(app$get_js("window.document.title;"),"reView")
  
  get_bn <- function() app$get_values()$export$`reView-reViewtab2-new_bn`
  
  app$set_window_size(800,600)
  app$wait_for_value(output = "reView-reViewtab1-model1_plot")
  app$wait_for_value(output = "reView-reViewtab1-model2_plot")
  app$wait_for_value(output = "reView-reViewtab1-model3_plot")
  app$wait_for_value(output = "reView-reViewtab1-model4_plot")
  app$wait_for_value(output = "reView-reViewtab1-model5_plot")
  app$wait_for_value(output = "reView-reViewtab1-model6_plot")
  
  expect_equal(as.ts(get_bn()),as.ts(twoStepsBenchmark(turnover,construction,
                                                       outliers = list(AO2005 = rep(0.1,12L)),
                                                       set.coeff = c(AO2005 = 1))))
  
  app$set_inputs(`reView-menu` = "Export")
  expect_equal(app$wait_for_value(output = "reView-reViewtab3-newcall"),
               paste("twoStepsBenchmark(",
                     "hfserie = turnover,",
                     "lfserie = construction,",
                     "include.differenciation = FALSE,",
                     "include.rho = FALSE,",
                     "set.coeff = c(AO2005=1),",
                     "start.coeff.calc = 2000,",
                     "end.coeff.calc = 2019,",
                     "start.benchmark = 2000,",
                     "end.benchmark = 2019,",
                     "outliers = list(AO2005=c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1))\n)",sep = "\n\t"))
  expect_equal(app$wait_for_value(output = "reView-reViewtab3-oldcall"),
               paste("twoStepsBenchmark(",
                     "hfserie = turnover,",
                     "lfserie = construction,",
                     "include.differenciation = FALSE,",
                     "include.rho = FALSE,",
                     "set.coeff = c(AO2005=1),",
                     "outliers = list(AO2005=c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1))\n)",sep = "\n\t"))
  
  app$set_inputs(`reView-menu` = "Modify")
  app$set_inputs(`reView-reViewtab2-setcoeff_button` = TRUE)
  app$set_inputs(`reView-reViewtab2-setcoeff` = 100)
  expect_equal(as.ts(get_bn()),as.ts(twoStepsBenchmark(turnover,construction,
                                                       outliers = list(AO2005 = rep(0.1,12L)),
                                                       set.coeff = c(AO2005 = 1,
                                                                     hfserie = 100))))
  
  app$set_inputs(`reView-menu` = "Export")
  expect_equal(app$wait_for_value(output = "reView-reViewtab3-newcall"),
               paste("twoStepsBenchmark(",
                     "hfserie = turnover,",
                     "lfserie = construction,",
                     "include.differenciation = FALSE,",
                     "include.rho = FALSE,",
                     "set.coeff = c(hfserie=100,AO2005=1),",
                     "start.coeff.calc = 2000,",
                     "end.coeff.calc = 2019,",
                     "start.benchmark = 2000,",
                     "end.benchmark = 2019,",
                     "outliers = list(AO2005=c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1))\n)",sep = "\n\t"))
  
  app$stop()
})

test_that("get_benchmark_call NULL",{
  expect_null(get_benchmark_call(NULL,"a","b"))
})

test_that("link_if_in_shiny if not in shiny",{
  expect_equal(link_if_in_shiny("id","label",NULL),"label")
})

test_that("clean set coeff", {
  expect_equal(
    clean_set_coeff(0,
                    twoStepsBenchmark(turnover,construction,
                                      outliers = list(AO2005 = rep(0.1,12L)),
                                      set.coeff = c(AO2005 = 1))),
    c(hfserie = 0, AO2005 = 1))
  expect_equal(
    clean_set_coeff(NULL,twoStepsBenchmark(turnover,construction,outliers = list(AO2005 = rep(0.1,12L)),set.coeff = c(AO2005 = 1))),
    c(AO2005 = 1))
  expect_equal(
    clean_set_coeff(0.5,twoStepsBenchmark(turnover,construction,outliers = list(AO2005 = rep(0.1,12L)))),
    0.5)
  
  expect_equal(
    clean_set_coeff(0,twoStepsBenchmark(turnover,construction)),
    0)
  
})

test_that("reView_name", {
  expect_equal(reViewName("a"), as.symbol("a"))
  expect_equal(reViewName(as.symbol("a")), as.symbol("a"))
  expect_equal(suppressWarnings(reViewName("*")), as.symbol("X."))
  expect_warning(reViewName("*"), "is invalid and has been changed")
  expect_equal(reViewName(quote(1+1)), quote(1+1))
  expect_equal(reViewName("."), as.symbol("."))
  expect_equal(reViewName("azdad__.dqdq.398D00e"), as.symbol("azdad__.dqdq.398D00e"))
  expect_equal(twoStepsBenchmark(turnover, construction)$call, twoStepsBenchmark(turnover, construction)$call)
})

test_that("warning reviewoutput once each session", {
  expect_warning(warning_reviewoutput(),"order of the reViewOutput")
  expect_no_warning(warning_reviewoutput())
})
