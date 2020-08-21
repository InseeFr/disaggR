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

shinyBenchmark <- function(hfserie,lfserie,file) {
  folder <- dirname(file)
  if (!dir.exists(folder)) stop("The requested folder doesn't exist")
  if (!file.exists(file)) {
    
  }
}
