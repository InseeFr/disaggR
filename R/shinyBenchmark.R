update_generator <- function() {
  update <- FALSE
  return(list(get=function() update,
              setTRUE=function() update <<- TRUE,
              setFALSE=function() update <<- FALSE,
              setDate=function(date) update <<- as.Date(date)))
}
.update <- update_generator()

option_shinyBenchmarks_get <- function() .update$get()

update_shinyBenchmarks <- function() {
  switch(
    utils::menu(c("Yes","No","Yes if they were validated before some date"),title = "",graphics = FALSE),
    {
      .update$setTRUE()
      message("Next shinyBenchmarks will be updated !")
    },
    {
      .update$setFALSE()
      message("No more updates for shinyBenchmarks !")
    },
    {
      .update$setDate(readline(prompt="Enter Date (YYYY-MM-DD) : "))
      message("Next shinyBenchmarks will be updated if they were validated before the ",option_shinyBenchmarks_get())
    }

  )
}

shinyBenchmark <- function(hfserie,lfserie,file,benchmark_name) {
  if (!(is.character(benchmark_name) && length(benchmark_name) == 1L)) stop("benchmark_name must be a character of length 1")
  folder <- dirname(file)
  if (!dir.exists(folder)) stop("The requested folder doesn't exist")
  if (!file.exists(file)) {
    if (!isTRUE(option_shinyBenchmarks_get())) warning("Creating shinyBenchmark file",call. = FALSE)
    return(runapp_disaggr(hfserie,lfserie,file,benchmark_name,data.frame()))
  }
  olddf <- read.csv(file)
  if match(benchmark_name,r
  if (isTRUE(option_shinyBenchmarks_get()) || )
}

runapp_disaggr <- function(hfserie,lfserie,file,benchmark_name,olddf) {
  shinyreturn <- shiny::runApp(
    shiny::shinyApp(ui = disaggRui(hfserie,lfserie,
                                   benchmark_name),
                    server = disaggRserver(hfserie,lfserie,
                                           include.differenciation.prec,include.rho.prec,
                                           set.coeff.prec,set.const.prec,
                                           start.coeff.calc.prec,end.coeff.calc.prec,
                                           start.benchmark.prec,end.benchmark.prec,benchmark_name)
    )
  )
  if (inherits(shinyreturn,"error")) stop(shinyreturn, call. = FALSE)
  return(shinyreturn)
}