test_that("params for other things than shiny test",{
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
                               hfserie_name = "turnover",
                               lfserie_name = "construction",
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
  browser <- options(browser=function(url) message(url))
  on.exit(options(browser))
  
  benchmark <- twoStepsBenchmark(turnover,construction)
  
  temp_dir <- tempdir()
  temp_html <- tempfile("test",temp_dir,".html")
  expect_message(rePort(benchmark,output_file = temp_html, launch.browser = TRUE))
  expect_true(file.exists(temp_html))
  unlink(temp_html)
  
  rePort(reViewOutput(benchmark,benchmark,compare = TRUE),output_file = temp_html)
  expect_true(file.exists(temp_html))
  unlink(temp_html)
  
  temp_rds <- tempfile("test",temp_dir,".rds")
  saveRDS(twoStepsBenchmark(turnover,construction),temp_rds)
  expect_message(url <- rePort(temp_rds))
  unlink(url)
  
  con <- gzcon(gzfile(temp_rds))
  expect_message(url <- rePort(con))
  expect_true(file.exists(url))
  unlink(url)
  
  expect_message(url <- print(reViewOutput(benchmark,benchmark,FALSE)))
  unlink(url)
})

test_that("reView-withoutset",{
  
  # important : the package should have been rebuilt for these tests
  # (ie with installed and restart in R Studio not loaded with devtools)
  # One has also have PhantomJS work with the firewall
  
  skip_on_os("mac") # phantomjs bugs with macos
  testthat::skip_if_not_installed("shinytest")
  
  app <- shinytest::ShinyDriver$new(test_path("shiny-withoutset"))
  
  expect_identical(app$getTitle(),"reView")
  
  get_bn <- function() app$getAllValues()$export$`reView-reViewtab2-new_bn`
  
  # First tab
  app$setWindowSize(800,600)
  model1 <- app$waitForValue("reView-reViewtab1-model1_plot",iotype="output")
  model2 <- app$waitForValue("reView-reViewtab1-model2_plot",iotype="output")
  model3 <- app$waitForValue("reView-reViewtab1-model3_plot",iotype="output")
  model4 <- app$waitForValue("reView-reViewtab1-model4_plot",iotype="output")
  model5 <- app$waitForValue("reView-reViewtab1-model5_plot",iotype="output")
  model6 <- app$waitForValue("reView-reViewtab1-model6_plot",iotype="output")
  
  models <- list(model1,model2,model3,
                 model4,model5,model6)
  
  expect_true(all(vapply(models,`[[`,0,"height") >= 142))
  expect_true(all(vapply(models,`[[`,0,"height") <= 163))
  
  expect_equal(as.ts(get_bn()),as.ts(twoStepsBenchmark(turnover,construction)))
  
  # Click on a model changes navbar
  app$setInputs(`reView-reViewtab1-model1_plotclick` = 10L,
                allowInputNoBinding_ = TRUE)
  slidercoeffcalc <- app$waitForValue("reView-reViewtab2-coeffcalc",iotype="input",
                                      timeout=5000)
  sliderbenchmark <- app$waitForValue("reView-reViewtab2-benchmark",iotype="input")
  sliderplots <- app$waitForValue("reView-reViewtab2-plotswin",iotype="input")
  newplot <- app$waitForValue("reView-reViewtab2-newplot",iotype="output")
  oldplot <-app$waitForValue("reView-reViewtab2-oldplot",iotype="output")
  expect_equal(app$waitForValue("reView-menu",iotype="input"),"Modify")
  expect_equal(slidercoeffcalc,c(2000,2019))
  expect_equal(sliderbenchmark,c(2000,2019))
  expect_equal(sliderplots,c(2000,2020))
  
  set_coeff <- app$waitForValue("reView-reViewtab2-setcoeff",iotype="input")
  set_const <- app$waitForValue("reView-reViewtab2-setconst",iotype="input")
  
  expect_equal(set_coeff,1)
  expect_equal(set_const,0)
  
  set_coeff_button <- app$waitForValue("reView-reViewtab2-setcoeff_button",iotype="input")
  set_const_button <- app$waitForValue("reView-reViewtab2-setconst_button",iotype="input")
  
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
  
  app$setInputs(`reView-reViewtab2-infobtn` = "click")
  app$waitFor("($('#shiny-modal').data('bs.modal') || {}).isShown")
  expect_true(grepl("These scatter plots",
                    app$getSource(),
                    fixed = TRUE))
  expect_true(grepl("reset plot window",
                    app$getSource(),
                    fixed = TRUE))
  app$executeScript("$('.modal').modal('hide');")
  
  # Differenciation
  
  app$setInputs(`reView-reViewtab3-Reset` = "click",
                allowInputNoBinding_ = TRUE)
  app$setInputs(`reView-reViewtab2-dif` = TRUE)
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction,
                                       include.differenciation = TRUE)))
  # Rho
  app$setInputs(`reView-reViewtab3-Reset` = "click",
                allowInputNoBinding_ = TRUE)
  app$setInputs(`reView-reViewtab2-rho` = TRUE)
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction,
                                       include.rho = TRUE)))
  
  # Setcoeff
  app$setInputs(`reView-reViewtab3-Reset` = "click",
                allowInputNoBinding_ = TRUE)
  app$setInputs(`reView-reViewtab2-setcoeff_button` = TRUE)
  app$setInputs(`reView-reViewtab2-setcoeff` = 100)
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction,
                                       set.coeff = 100)))
  
  # Setconst
  app$setInputs(`reView-reViewtab3-Reset` = "click",
                allowInputNoBinding_ = TRUE)
  app$setInputs(`reView-reViewtab2-setconst_button` = TRUE)
  app$setInputs(`reView-reViewtab2-setconst` = 100)
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction,
                                       set.const = 100)))
  
  # empty numerics -> 0 and and active only if pressed
  app$setInputs(`reView-reViewtab3-Reset` = "click",
                allowInputNoBinding_ = TRUE)
  app$setInputs(`reView-reViewtab2-setconst` = NULL)
  app$setInputs(`reView-reViewtab2-setcoeff` = NULL)
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction)))
  app$setInputs(`reView-reViewtab2-setconst_button` = TRUE)
  app$setInputs(`reView-reViewtab2-setcoeff_button` = TRUE)
  
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction,
                                       set.coeff = 0,
                                       set.const = 0)))
  
  # coeffcalc
  app$setInputs(`reView-reViewtab3-Reset` = "click",
                allowInputNoBinding_ = TRUE)
  app$setInputs(`reView-reViewtab2-coeffcalc` = c(2004, 2012))
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction,
                                       start.coeff.calc = 2004,
                                       end.coeff.calc = 2012)))
  
  # Benchmark
  app$setInputs(`reView-reViewtab3-Reset` = "click",
                allowInputNoBinding_ = TRUE)
  app$setInputs(`reView-reViewtab2-benchmark` = c(2004, 2015))
  
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction,
                                       start.benchmark = 2004,
                                       end.benchmark = 2015)))
  
  # Plots
  app$setInputs(`reView-reViewtab2-plotswin` = as.numeric(c(2003, 2014)))
  expect_equal(app$getValue("reView-reViewtab2-plotswin"),
               c(2003,2014))
  app$setInputs(`reView-reViewtab2-click` = 1L,allowInputNoBinding_ = TRUE)
  expect_equal(app$getValue("reView-reViewtab2-plotswin"),
               c(2000,2020))
  
  # Change output to benchmark plots
  expect_equal(app$waitForValue("reView-reViewtab2-mainout_choice",iotype="input"),
               "Scatter plot")
  app$setInputs(`reView-reViewtab2-mainout_choice` = "Benchmark plot")
  
  newplot <- app$waitForValue("reView-reViewtab2-newplot",iotype="output")
  oldplot <- app$waitForValue("reView-reViewtab2-oldplot",iotype="output")
  
  plots <- list(newplot,oldplot)
  
  expect_true(all(vapply(plots,`[[`,0,"height") >= 432))
  expect_true(all(vapply(plots,`[[`,0,"height") <= 452))
  expect_true(all(vapply(plots,`[[`,0,"width") >= 269))
  
  # info button shows benchmark plots modal
  
  app$setInputs(`reView-reViewtab2-infobtn` = "click")
  app$waitFor("($('#shiny-modal').data('bs.modal') || {}).isShown")
  expect_true(grepl("procedure involved",
                    app$getSource(),
                    fixed = TRUE))
  expect_true(grepl("to change plot window",
                    app$getSource(),
                    fixed = TRUE))
  app$executeScript("$('.modal').modal('hide');")
  
  # Change output to in sample
  expect_equal(app$waitForValue("reView-reViewtab2-mainout_choice",iotype="input"),
               "Benchmark plot")
  app$setInputs(`reView-reViewtab2-mainout_choice` = "In-sample predictions")
  
  newplotlev <- app$waitForValue("reView-reViewtab2-newplotlev",iotype="output")
  oldplotlev <- app$waitForValue("reView-reViewtab2-oldplotlev",iotype="output")
  newplotcha <- app$waitForValue("reView-reViewtab2-newplotcha",iotype="output")
  oldplotcha <- app$waitForValue("reView-reViewtab2-oldplotcha",iotype="output")
  
  plots <- list(newplotlev,oldplotlev,
                newplotcha,oldplotcha)
  
  expect_true(all(vapply(plots,`[[`,0,"height") >= 211))
  expect_true(all(vapply(plots,`[[`,0,"height") <= 231))
  expect_true(all(vapply(plots,`[[`,0,"width") >= 269))
  
  # Summary
  app$setInputs(`reView-reViewtab3-Reset` = "click",
                allowInputNoBinding_ = TRUE)
  app$setInputs(`reView-reViewtab2-mainout_choice` = "Benchmark summary")
  oldsum <- app$waitForValue("reView-reViewtab2-oldverbat",iotype="output")
  newsum <- app$waitForValue("reView-reViewtab2-newverbat",iotype="output")
  
  expect_equal(gsub("‘|’|'","",newsum),
               gsub("‘|’|'","",paste(capture.output(print(
                 summary(twoStepsBenchmark(turnover,construction)),
                 call = FALSE
               )),collapse="\n")))
  
  # Indicator
  app$setInputs(`reView-reViewtab2-mainout_choice` = "Comparison benchmark/input")
  plotlev <- app$waitForValue("reView-reViewtab2-monoplotlev",iotype="output")
  plotcha <- app$waitForValue("reView-reViewtab2-monoplotcha",iotype="output")
  plotctb <- app$waitForValue("reView-reViewtab2-monoplotctb",iotype="output")
  
  plots <- list(plotlev,
                plotcha,
                plotctb)
  
  expect_true(all(vapply(plots,`[[`,0,"height") >= 142))
  expect_true(all(vapply(plots,`[[`,0,"height") <= 163))
  expect_true(all(vapply(plots,`[[`,0,"width") >= 603))
  expect_true(all(vapply(plots,`[[`,0,"width") <= 623))
  
  # Revisions
  app$setInputs(`reView-reViewtab2-mainout_choice` = "Revisions")
  plotlev <- app$waitForValue("reView-reViewtab2-monoplotlev",iotype="output")
  plotcha <- app$waitForValue("reView-reViewtab2-monoplotcha",iotype="output")
  plotctb <- app$waitForValue("reView-reViewtab2-monoplotctb",iotype="output")
  
  expect_true(all(vapply(plots,`[[`,0,"height") >= 142))
  expect_true(all(vapply(plots,`[[`,0,"height") <= 163))
  expect_true(all(vapply(plots,`[[`,0,"width") >= 603))
  expect_true(all(vapply(plots,`[[`,0,"width") <= 623))
  
  # Reset change menu
  app$setInputs(`reView-menu` = "Export")
  app$setInputs(`reView-reViewtab3-Reset` = "click",
                allowInputNoBinding_ = TRUE)
  app$waitForValue("reView-reViewtab2-monoplotlev",iotype="output")
  expect_equal(app$waitForValue("reView-menu",iotype="input"),"Modify")
  
  p <- app$.__enclos_env__$private$shinyProcess
  p$interrupt()
  p$wait()
  # the previous code is to quit the shinyprocess for codecov to update
})

test_that("reView-setcoefconst",{
  
  # important : the package should have been rebuilt for these tests
  # (ie with installed and restart in R Studio not loaded with devtools)
  # One has also have PhantomJS work with the firewall
  
  skip_on_os("mac") # phantomjs bugs with macos
  testthat::skip_if_not_installed("shinytest")
  
  app <- shinytest::ShinyDriver$new(test_path("shiny-setcoefconst"))
  
  expect_identical(app$getTitle(),"reView")
  
  get_bn <- function() app$getAllValues()$export$`reView-reViewtab2-new_bn`
  
  app$setWindowSize(800,600)
  app$waitForValue("reView-reViewtab1-model1_plot",iotype="output")
  app$waitForValue("reView-reViewtab1-model2_plot",iotype="output")
  app$waitForValue("reView-reViewtab1-model3_plot",iotype="output")
  app$waitForValue("reView-reViewtab1-model4_plot",iotype="output")
  app$waitForValue("reView-reViewtab1-model5_plot",iotype="output")
  app$waitForValue("reView-reViewtab1-model6_plot",iotype="output")
  
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
  app$setInputs(`reView-menu` = "Modify")
  slidercoeffcalc <- app$waitForValue("reView-reViewtab2-coeffcalc",iotype="input",
                                      timeout=5000)
  sliderbenchmark <- app$waitForValue("reView-reViewtab2-benchmark",iotype="input")
  sliderplots <- app$waitForValue("reView-reViewtab2-plotswin",iotype="input")
  expect_equal(app$waitForValue("reView-menu",iotype="input"),"Modify")
  expect_equal(slidercoeffcalc,c(2005,2015))
  expect_equal(sliderbenchmark,c(2004,2018))
  expect_equal(sliderplots,c(2000,2020))
  
  set_coeff <- app$waitForValue("reView-reViewtab2-setcoeff",iotype="input")
  set_const <- app$waitForValue("reView-reViewtab2-setconst",iotype="input")

  expect_equal(set_coeff,1)
  expect_equal(set_const,0)
  
  set_coeff_button <- app$waitForValue("reView-reViewtab2-setcoeff_button",iotype="input")
  set_const_button <- app$waitForValue("reView-reViewtab2-setconst_button",iotype="input")
  
  expect_equal(set_coeff_button,TRUE)
  expect_equal(set_const_button,TRUE)
  
  # Back to first tab to check the summary table
  app$setInputs(`reView-menu` = "Presets")
  
  app$waitForValue("reView-reViewtab1-model1_plot",iotype="output")
  app$waitForValue("reView-reViewtab1-model2_plot",iotype="output")
  app$waitForValue("reView-reViewtab1-model3_plot",iotype="output")
  app$waitForValue("reView-reViewtab1-model4_plot",iotype="output")
  app$waitForValue("reView-reViewtab1-model5_plot",iotype="output")
  app$waitForValue("reView-reViewtab1-model6_plot",iotype="output")
  
  app$setInputs(`reView-reViewtab1-firsttab_choice`="Summary table")
  app$waitForValue("reView-reViewtab1-model1_actionlink")
  app$waitForValue("reView-reViewtab1-model2_actionlink")
  app$waitForValue("reView-reViewtab1-model3_actionlink")
  app$waitForValue("reView-reViewtab1-model4_actionlink")
  app$waitForValue("reView-reViewtab1-model5_actionlink")
  app$waitForValue("reView-reViewtab1-model6_actionlink")
  expect_true(grepl("distance",app$getSource(),fixed=TRUE))
  app$click("reView-reViewtab1-model1_actionlink")
  expect_equal(app$waitForValue("reView-menu"),"Modify")
  expect_equal(as.ts(get_bn()),
               as.ts(twoStepsBenchmark(turnover,construction,
                                       include.differenciation = TRUE,
                                       start.coeff.calc = 2005,
                                       end.coeff.calc = 2015,
                                       start.benchmark = 2004,
                                       end.benchmark = 2018,
                                       start.domain = 1990,
                                       end.domain = c(2030,12))))
  
  p <- app$.__enclos_env__$private$shinyProcess
  p$interrupt()
  p$wait()
  # the previous code is to quit the shinyprocess for codecov to update
})

test_that("benchmarkCall NULL",{
  expect_null(benchmarkCall(NULL,"a","b"))
})

test_that("link_if_in_shiny if not in shiny",{
  expect_equal(link_if_in_shiny("id","label",NULL),"label")
})