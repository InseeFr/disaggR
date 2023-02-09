library(shiny)
library(disaggR)
ui <- disaggR:::reView_ui()

server <-  disaggR:::reView_server(
  twoStepsBenchmark(turnover,construction,
                    set.coeff = 1,
                    set.const = 0,
                    include.differenciation = TRUE,
                    include.rho = TRUE,
                    start.coeff.calc = 2005,end.coeff.calc = 2015,
                    start.benchmark = 2004,end.benchmark = 2018,
                    start.domain = 1990,end.domain = c(2030,12)),
  disaggR:::reViewName("turnover"),disaggR:::reViewName("construction"),
  TRUE)

shinyApp(ui = ui, server = server)
