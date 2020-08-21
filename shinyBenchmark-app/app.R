#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- function(hfserie,lfserie,indicname) {
    fluidPage(
        
        # Application title
        titlePanel(paste0("shinyBenchmark : ",indicname)),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
            sidebarPanel(
                width = 2,
                h5(tags$u("Include"),align="center"),
                checkboxInput("dif","Differenciation"),
                checkboxInput("rho","Rho"),
                h5(tags$u("Set"),align="center"),
                checkboxInput("setcoeff_button","Coefficient"),
                conditionalPanel("input.setcoeff_button",numericInput("setcoeff","",1)),
                checkboxInput("setconst_button","Constant"),
                conditionalPanel("input.setconst_button",numericInput("setconst","",0)),
                uiOutput("setconst"),
                h5(tags$u("Windows"),align="center"),
                sliderInput("coeffcalc",
                            "Coefficients:",
                            min = tsp(lfserie)[1],
                            max = tsp(lfserie)[2],
                            value = c(tsp(lfserie)[1],tsp(lfserie)[2]),
                            step = deltat(lfserie)),
                sliderInput("benchmark",
                            "Benchmark:",
                            min = tsp(lfserie)[1],
                            max = tsp(lfserie)[2],
                            value = c(tsp(lfserie)[1],tsp(lfserie)[2]),
                            step = deltat(lfserie)),
                actionButton("validation","Validate",width = "100%")
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                width = 10,
                fluidRow(
                    column(12,
                    radioButtons("plotchoice","",
                                 choiceNames = c("Benchmark","In-sample predictions","Summary"),
                                 choiceValues = c("benchmark","insample","summary"),
                                 inline = TRUE)
                         ),
                    align="center"
                    ),
                conditionalPanel(condition = "input.plotchoice == 'benchmark' || input.plotchoice == 'insample'",{
                    fluidRow(
                        column(width=6,h5(tags$u("Before"),align="center"),plotOutput("preplot")),
                        column(width=6,h5(tags$u("After"),align="center"),plotOutput("newplot"))
                    )
                }),
                conditionalPanel(condition = "input.plotchoice == 'summary'",
                                 fluidRow(
                                     column(width=6,h5(tags$u("Before"),align="center"),textOutput("presum")),
                                     column(width=6,h5(tags$u("After"),align="center"),textOutput("newsum"))
                                 ),
                                 tags$style(type="text/css","#newsum {white-space: pre-wrap;}"),
                                 tags$style(type="text/css","#presum {white-space: pre-wrap;}")
                )
            )
        )
    )
}

# Define server logic required to draw a histogram
server <- function(hfserie,lfserie,
                   include.differenciation.prec,include.rho.prec,
                   set.coeff.prec,set.const.prec,
                   start.coeff.calc.prec,end.coeff.calc.prec,
                   start.benchmark.prec,end.benchmark.prec,
                   indicname) {
    oldbn <- twoStepsBenchmark(hfserie,lfserie,
                               include.differenciation.prec,include.rho.prec,
                               set.coeff.prec,set.const.prec,
                               start.coeff.calc.prec,end.coeff.calc.prec,
                               start.benchmark.prec,end.benchmark.prec)
    function(input, output,session) {
        set_coeff <- reactive({
            if (input$setcoeff_button) {
                if (is.null(input$setcoeff) || is.na(input$setcoeff)) return(0)
                else return(input$setcoeff)
                return(input$setcoeff)
            }
            else return(NULL)
        })
        set_const <- reactive({
            if (input$setconst_button) {
                if (is.null(input$setconst) || is.na(input$setconst)) return(0)
                else return(input$setconst)
                return(input$setconst)
            }
            else return(NULL)
        })
        observe({
            if (input$validation > 0) stopApp(list(input$dif,input$rho,input$setcoeff))
        })
        bn <- reactive({twoStepsBenchmark(hfserie,lfserie,
                                          include.differenciation = input$dif,
                                          include.rho = input$rho,
                                          set.coeff = set_coeff(),
                                          set.const = set_const(),
                                          start.coeff.calc = input$coeffcalc[1],
                                          end.coeff.calc = input$coeffcalc[2],
                                          start.benchmark = input$benchmark[1],
                                          end.benchmark = input$benchmark[2])})
        # output$summary <- renderPrint({
        #     smry <- summary(prais(bn()))
        #     printCoefmat(smry$coefficients, P.values = TRUE, has.Pvalue = TRUE,digits=3,signif.stars=TRUE)
        # })
        
        observe({switch(input$plotchoice,
               benchmark={
                   output$newplot <- renderPlot(autoplot(bn()))
                   output$preplot <- renderPlot(autoplot(oldbn))
               },
               insample={
                   output$newplot <- renderPlot(autoplot(in_sample(bn())))
                   output$preplot <- renderPlot(autoplot(in_sample(oldbn)))
               },
               summary={
                   output$newsum <- renderPrint(print(summary(bn())))
                   output$presum <- renderPrint(print(summary(oldbn)))
               })})
    }
}

# Run the application 
lfserie <- construction
hfserie <- turnover
indicname <- "test"
include.differenciation.prec <- TRUE
include.rho.prec <- FALSE
set.coeff.prec <- NULL
set.const.prec <- NULL
start.coeff.calc.prec <- NULL
end.coeff.calc.prec <- NULL
start.benchmark.prec <- NULL
end.benchmark.prec <- NULL
test <- runApp(shinyApp(ui = ui(hfserie,lfserie,
                                indicname),
                        server = server(hfserie,lfserie,
                                        include.differenciation.prec,include.rho.prec,
                                        set.coeff.prec,set.const.prec,
                                        start.coeff.calc.prec,end.coeff.calc.prec,
                                        start.benchmark.prec,end.benchmark.prec,indicname)))
