app <- ShinyDriver$new("../../")
# To launch outside
# app <- ShinyDriver$new(testthat::test_path("shiny"))

app$snapshotInit("shinytest")

# First tab
app$setWindowSize(800,600)
app$waitForValue("reView-reViewtab1-model1_plot",iotype="output")
app$waitForValue("reView-reViewtab1-model2_plot",iotype="output")
app$waitForValue("reView-reViewtab1-model3_plot",iotype="output")
app$waitForValue("reView-reViewtab1-model4_plot",iotype="output")
app$waitForValue("reView-reViewtab1-model5_plot",iotype="output")
app$waitForValue("reView-reViewtab1-model6_plot",iotype="output")
app$snapshot()

# Click on a model changes navbar
app$setInputs(`reView-reViewtab1-model3_click` = 1L,
              allowInputNoBinding_ = TRUE)
app$waitForValue("reView-reViewtab2-coeffcalc",iotype="input")
app$waitForValue("reView-reViewtab2-benchmark",iotype="input")
app$waitForValue("reView-reViewtab2-plotswin",iotype="input")
app$waitForValue("reView-reViewtab2-newplot",iotype="output")
app$waitForValue("reView-reViewtab2-oldplot",iotype="output")
app$snapshot()

# Differenciation

app$setInputs(`reView-reViewtab2-dif` = TRUE)
app$snapshot()

# Rho

app$setInputs(`reView-reViewtab2-rho` = TRUE)
app$snapshot()

# Setcoeff
app$setInputs(`reView-reViewtab2-setcoeff_button` = TRUE)
app$snapshot()
app$setInputs(`reView-reViewtab2-setcoeff` = 110)
app$snapshot()

# Setconst
app$setInputs(`reView-reViewtab2-setconst_button` = TRUE)
app$snapshot()
app$setInputs(`reView-reViewtab2-setconst` = 100)
app$snapshot()
app$setInputs(`reView-reViewtab2-setconst` = NA)

# coeffcalc
app$setInputs(`reView-reViewtab2-coeffcalc` = c(2004, 2012))
app$snapshot()

# Benchmark
app$setInputs(`reView-reViewtab2-benchmark` = c(2004, 2015))
app$snapshot()

# Plots
app$setInputs(`reView-reViewtab2-plotswin` = as.numeric(c(2003, 2014)),
              allowInputNoBinding_ = TRUE)
app$snapshot()

# Double click
app$setInputs(`reView-reViewtab1-click` = 2L,allowInputNoBinding_ = TRUE)
app$snapshot()

# Change output to in sample
app$waitForValue("reView-reViewtab2-mainout_choice",iotype="input")
app$setInputs(`reView-reViewtab2-mainout_choice` = "In-sample predictions")
app$waitForValue("reView-reViewtab2-newplotlev",iotype="output")
app$waitForValue("reView-reViewtab2-oldplotlev",iotype="output")
app$waitForValue("reView-reViewtab2-newplotcha",iotype="output")
app$waitForValue("reView-reViewtab2-oldplotcha",iotype="output")
app$snapshot()
app$setInputs(`reView-reViewtab2-setcoeff_button` = FALSE)
app$snapshot()

# Summary
app$setInputs(`reView-reViewtab2-mainout_choice` = "Benchmark summary")
app$waitForValue("reView-reViewtab2-oldverbat",iotype="output")
app$waitForValue("reView-reViewtab2-newverbat",iotype="output")
app$setInputs(`reView-reViewtab2-setconst_button` = FALSE)

# Indicator
app$setInputs(`reView-reViewtab2-mainout_choice` = "Comparison with indicator")
app$waitForValue("reView-reViewtab2-monoplotlev",iotype="output")
app$waitForValue("reView-reViewtab2-monoplotcha",iotype="output")
app$waitForValue("reView-reViewtab2-monoplotctb",iotype="output")
app$snapshot()
app$setInputs(`reView-reViewtab2-plotswin` = c(2000, 2019))
app$snapshot()

# Revisions
app$setInputs(`reView-reViewtab2-mainout_choice` = "Revisions")
app$waitForValue("reView-reViewtab2-monoplotlev",iotype="output")
app$waitForValue("reView-reViewtab2-monoplotcha",iotype="output")
app$waitForValue("reView-reViewtab2-monoplotctb",iotype="output")
app$snapshot()
app$setInputs(`reView-reViewtab2-plotswin` = c(2005, 2010),
              allowInputNoBinding_ = TRUE)
app$setInputs(`reView-reViewtab2-click` = 1,allowInputNoBinding_ = TRUE)

# Tab 3
app$setInputs(`reView-reViewtab2-setcoeff_button` = TRUE)
app$setInputs(`reView-reViewtab2-setconst_button` = TRUE)
app$setInputs(`reView-reViewtab2-setcoeff` = 1)
app$setInputs(`reView-reViewtab2-setconst` = 1)
app$setInputs(`reView-menu` = "Export")
app$snapshot()
app$setInputs(`reView-reViewtab3-Reset` = "click",
              allowInputNoBinding_ = TRUE)
app$waitForValue("reView-reViewtab2-dif",iotype="input")
app$waitForValue("reView-reViewtab2-rho",iotype="input")
app$waitForValue("reView-reViewtab2-coeffcalc",iotype="input")
app$waitForValue("reView-reViewtab2-benchmark",iotype="input")
app$waitForValue("reView-reViewtab2-plotswin",iotype="input")
app$waitForValue("reView-reViewtab2-setcoeff_button",iotype="input")
app$waitForValue("reView-reViewtab2-setcoeff",iotype="input")
app$waitForValue("reView-reViewtab2-setconst_button",iotype="input")
app$waitForValue("reView-reViewtab2-setconst",iotype="input")
app$waitForValue("reView-reViewtab2-newplot",iotype="output")
app$waitForValue("reView-reViewtab2-oldplot",iotype="output")
app$snapshot()

app$setInputs(`reView-menu` = "Export")

tryCatch(app$setInputs("reView-reViewtab3-Quit" = "click"),error=function(e) NULL)

p <- app$.__enclos_env__$private$shinyProcess

testthat::expect_false(p$is_alive())

cleanjson <- function(x) {
  writeChar(
    prettify(
      toJSON(fromJSON(x),
             digits=2),
      indent=2),x, eos = NULL)
}
library(jsonlite)
lapply(list.files("./shinytest-expected", pattern = "json", full.names = TRUE),
       cleanjson)

lapply(list.files("./shinytest-current", pattern = "json", full.names = TRUE),
       cleanjson)

p$wait()