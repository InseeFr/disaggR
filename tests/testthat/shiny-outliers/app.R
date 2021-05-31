library(shiny)
library(disaggR)
ui <- disaggR:::reView_ui()

server <-  disaggR:::reView_server(twoStepsBenchmark(turnover,construction,
                                                     outliers = list(AO2005 = rep(0.1,12L))),
                                   "turnover","construction",
                                   TRUE)

shinyApp(ui = ui, server = server)
