disaggRui <- function(hfserie,lfserie,indicname) {
    fluidPage(
        titlePanel(paste0("shinyBenchmark : ",indicname)),
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
                                     column(width=6,h5(tags$u("Before"),align="center"),verbatimTextOutput("presum")),
                                     column(width=6,h5(tags$u("After"),align="center"),verbatimTextOutput("newsum"))
                                 )
                )
            )
        )
    )
}

disaggRserver <- function(hfserie,lfserie,
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
                return(input$setcoeff)
            }
            return(NULL)
        })
        set_const <- reactive({
            if (input$setconst_button) {
                if (is.null(input$setconst) || is.na(input$setconst)) return(0)
                return(input$setconst)
            }
            return(NULL)
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
        observe(if (input$validation > 0) stopApp(bn()))
        session$onSessionEnded(function() stopApp(errorCondition("shinyBenchmark cancelled")))
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
                            output$newsum <- renderPrint(print(summary(bn()),call=FALSE))
                            output$presum <- renderPrint(print(summary(oldbn),call=FALSE))
                        })})
    }
}