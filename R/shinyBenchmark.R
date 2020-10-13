runapp_disaggr <- function(oldbn,benchmark.name,start.domain,end.domain,compare) {
  shinyreturn <- shiny::runApp(
    shiny::shinyApp(ui = shinyBenchmark_ui,
                    server = shinyBenchmark_server(oldbn,benchmark.name,start.domain,end.domain,compare,TRUE)
    ),quiet = TRUE
  )
  if (inherits(shinyreturn,"error")) stop(shinyreturn)
  shinyreturn
}

#' @import shiny
viewBenchmark <- function(benchmark,start.domain = NULL,end.domain = NULL) {
  runapp_disaggr(benchmark,deparse(a$call),start.domain,end.domain,compare=TRUE)
}
