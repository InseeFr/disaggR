library(shiny)
library(disaggR)
ui <- disaggR:::reView_ui()

server <-  disaggR:::reView_server(twoStepsBenchmark(turnover,construction),
                                   as.symbol("turnover"),as.symbol("construction"),
                                   TRUE)

shinyApp(ui = ui, server = server)