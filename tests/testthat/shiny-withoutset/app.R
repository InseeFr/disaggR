library(shiny)
library(disaggR)
ui <- disaggR:::reView_ui()

server <-  disaggR:::reView_server(twoStepsBenchmark(turnover,construction),
                                   disaggR:::reViewName("turnover"),disaggR:::reViewName("construction"),
                                   TRUE)

shinyApp(ui = ui, server = server)