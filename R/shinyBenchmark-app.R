shinyBenchmark_ui_module <- function(id) {
    ns <- NS(id)
    fluidPage(
        uiOutput("titlePanel"),
        sidebarLayout(
            sidebarPanel(
                width = 2,
                h5(tags$u("Include"),align="center"),
                checkboxInput(ns("dif"),"Differenciation"),
                checkboxInput(ns("rho"),"Rho"),
                h5(tags$u("Set"),align="center"),
                checkboxInput(ns("setcoeff_button"),"Coefficient"),
                conditionalPanel("input.setcoeff_button",numericInput(ns("setcoeff"),"",1),ns = ns),
                checkboxInput(ns("setconst_button"),"Constant"),
                conditionalPanel("input.setconst_button",numericInput(ns("setconst"),"",0),ns = ns),
                uiOutput(ns("setconst")),
                h5(tags$u("Windows"),align="center"),
                uiOutput(ns("coeffcalcsliderInput")),
                uiOutput(ns("benchmarksliderInput")),
                actionButton(ns("validation"),"Validate")
            ),
            mainPanel(
                width = 10,
                fluidRow(
                    column(12,
                           radioButtons(ns("plotchoice"),"",
                                        choiceNames = c("Benchmark","In-sample predictions","Summary"),
                                        choiceValues = c("benchmark","insample","summary"),
                                        selected = "benchmark",
                                        inline = TRUE)
                    ),
                    align="center"
                ),
                uiOutput(ns("mainOutput"))
            )
        )
    )
}

shinyBenchmark_server_module <- function(id,hfserie,lfserie,oldbn,indicname) {
    moduleServer(id,function(input, output, session) {
        output$titlePanel <- renderUI(titlePanel(paste0("shinyBenchmark : ",indicname())))
        output$coeffcalcsliderInput <- renderUI(
            sliderInput(session$ns("coeffcalc"),
                        "Coefficients:",
                        min = tsp(lfserie())[1],
                        max = tsp(lfserie())[2],
                        value = c(tsp(lfserie())[1],tsp(lfserie())[2]),
                        step = deltat(lfserie()))
        )
        output$benchmarksliderInput <- renderUI(
            sliderInput(session$ns("benchmark"),
                        "Benchmark:",
                        min = tsp(lfserie())[1],
                        max = tsp(lfserie())[2],
                        value = c(tsp(lfserie())[1],tsp(lfserie())[2]),
                        step = deltat(lfserie()))
        )
        output$mainOutput <- renderUI({
            if (input$plotchoice == "benchmark" || input$plotchoice == "insample") {
                if (is.null(oldbn())) plotOutput(session$ns("newplot"))
                else {
                    fluidRow(
                        column(width=6,h5(tags$u("Before"),align="center"),plotOutput(session$ns("oldplot"))),
                        column(width=6,h5(tags$u("After"),align="center"),plotOutput(session$ns("newplot")))
                    )
                }
            } else {
                if (is.null(oldbn())) verbatimTextOutput(session$ns("newsum"))
                else {
                    fluidRow(
                        column(width=6,h5(tags$u("Before"),align="center"),verbatimTextOutput(session$ns("oldsum"))),
                        column(width=6,h5(tags$u("After"),align="center"),verbatimTextOutput(session$ns("newsum")))
                    )
                }
            }
        })
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
        bn <- reactive({twoStepsBenchmark(hfserie(),lfserie(),
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
                            output$oldplot <- renderPlot(autoplot(oldbn()))
                        },
                        insample={
                            output$newplot <- renderPlot(autoplot(in_sample(bn())))
                            output$oldplot <- renderPlot(autoplot(in_sample(oldbn())))
                        },
                        summary={
                            output$newsum <- renderPrint(print(summary(bn()),call=FALSE))
                            output$oldsum <- renderPrint(print(summary(oldbn()),call=FALSE))
                        })})
    })
}

shinyBenchmark_ui <- shinyBenchmark_ui_module("shinyBenchmark")
shinyBenchmark_server <- function(hfserie,lfserie,oldbn,indicname) {
    function(input,output,session) {
        shinyBenchmark_server_module("shinyBenchmark",reactive(hfserie),reactive(lfserie),reactive(oldbn),reactive(indicname))
    }
}
