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

runapp_disaggr <- function(oldbn,benchmark.name,start.domain,end.domain,compare) {
  shinyreturn <- shiny::runApp(
    shiny::shinyApp(ui = shinyBenchmark_ui,
                    server = shinyBenchmark_server(oldbn,benchmark.name,start.domain,end.domain,compare,TRUE)
    )
  )
  if (inherits(shinyreturn,"error")) stop(shinyreturn)
  return(shinyreturn)
}

write_benchmark <- function(newbn,rds.file,version) {
  saveRDS(newbn,file=rds.file,version = 3L)
}

validation_date <- function(object) {
  if (!inherits(object,"twoStepsBenchmark")) stop("Not a twoStepsBenchmark")
  return(attr(object,"ValidationDate"))
}

shinyBenchmark <- function(hfserie,lfserie,rds.file,start.domain = NULL,end.domain = NULL,benchmark.name=basename(rds.file),version = NULL) {
  if (!(is.character(rds.file) && length(rds.file) == 1L)) stop("rds.file must be a file path, a character of length 1", call.= FALSE)
  folder <- dirname(rds.file)
  if (!dir.exists(folder)) stop("The requested folder doesn't exist", call.= FALSE)
  if (!file.exists(rds.file)) {
    if (!isTRUE(option_shinyBenchmarks_get())) warning("Creating shinyBenchmark rds.file",call. = FALSE)
    newbn <- runapp_disaggr(twoStepsBenchmark(hfserie,lfserie),benchmark.name,start.domain,end.domain,compare=FALSE)
    saveRDS(newbn,file=rds.file,version = version)
    return(newbn)
  } else {
    oldbn <- readRDS(rds.file)
    if (isTRUE(option_shinyBenchmarks_get()) || validation_date(oldbn) < option_shinyBenchmarks_get()) {
      newbn <- runapp_disaggr(oldbn,benchmark.name,start.domain,end.domain,compare=TRUE)
      saveRDS(newbn,file=rds.file,version = version)
      return(newbn)
    } else {
      m <- model.list(oldbn)
      return(twoStepsBenchmark(hfserie,lfserie,
                               m$include.differenciation,m$include.rho,
                               m$set.coeff,m$set.const,
                               m$start.coeff.calc,m$end.coeff.calc,
                               m$start.benchmark,m$end.benchmark,
                               start.domain,end.domain,cl=match.call()))
    }
    
  }
}
