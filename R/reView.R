#' @import shiny
reView_ui_module <- function(id) {
  ns <- NS(id)
  navbarPage(title = "reView",id = ns("menu"),selected = "Modify",
             tabPanel("Modify",
                      sidebarLayout(
                        sidebarPanel(
                          width = 2,
                          tags$style("h4 { font-family: 'Source Sans Pro', sans-serif; font-weight: 400; line-height: 32px; text-align: center;}"),
                          h4("Include"),
                          prettySwitch(ns("dif"),"Differenciation",fill = TRUE,status = "primary"),
                          prettySwitch(ns("rho"),"Rho", fill = TRUE,status = "primary"),
                          h4("Set"),
                          # prettySwitch(ns("setcoeff_button"),"Coefficient",fill=TRUE,status="primary"),
                          # conditionalPanel("input.setcoeff_button",numericInput(ns("setcoeff"),NULL,1),ns = ns),
                          # prettySwitch(ns("setconst_button"),"Constant",fill=TRUE,status="primary"),
                          # conditionalPanel("input.setconst_button",numericInput(ns("setconst"),NULL,0),ns = ns),
                          fluidRow(column(6,align="left",prettySwitch(ns("setcoeff_button"),"Coefficient", fill = TRUE,status = "primary")),
                                   column(6,align="right",dropdownButton(numericInput(ns("setcoeff"),NULL,1),size="xs", icon = icon("gear")))),
                          fluidRow(column(6,align="left",prettySwitch(ns("setconst_button"),"Constant", fill = TRUE,status = "primary")),
                                   column(6,align="right",dropdownButton(numericInput(ns("setconst"),NULL,0),size="xs", icon = icon("gear")))),
                          uiOutput(ns("setconst")),
                          h4("Windows"),
                          uiOutput(ns("coeffcalcsliderInput")),
                          uiOutput(ns("benchmarksliderInput"))
                        ),
                        mainPanel(
                          width = 10,
                          fluidRow(
                            column(12,
                                   radioGroupButtons(ns("plotchoice"),NULL,
                                                     choices = c("Benchmark","In-sample predictions","Summary"),
                                                     selected = "Benchmark",
                                                     justified = TRUE)
                            ),
                            align="center"
                          ),
                          uiOutput(ns("mainOutput"),style="  padding: 6px 8px;
  margin-top: 6px;
  margin-bottom: 6px;
  background-color: #ffffff;
  border: 1px solid #e3e3e3;
  border-radius: 2px;")
                        )
                      )
             )
  )
}

slider_windows <- function(ns,lfserie,ui_out,label) {
  renderUI(
    sliderInput(ns(ui_out),
                label,
                min = tsp(lfserie())[1],
                max = tsp(lfserie())[2],
                value = c(tsp(lfserie())[1],tsp(lfserie())[2]),
                step = deltat(lfserie()))
  )
}

reView_server_module <- function(id,oldbn,benchmark.name,start.domain,end.domain,compare,function.mode=TRUE) {
  moduleServer(id,function(input, output, session) {
    
    lfserie <- reactive(model.list(oldbn())$lfserie)
    hfserie <- reactive({
      res <- model.list(oldbn())$hfserie
      return(res[,colnames(res) != "constant"])
    })
    
    output$coeffcalcsliderInput <- slider_windows(session$ns,lfserie,"coeffcalc","Coefficients:")
    output$benchmarksliderInput <- slider_windows(session$ns,lfserie,"benchmark","Benchmark:")
    
    output$mainOutput <- renderUI({
      switch(input$plotchoice,
             "Benchmark"={
               output$newplot <- renderPlot(ggplot2::autoplot(newbn()))
               if (!compare) return(plotOutput(session$ns("newplot")))
               output$oldplot <- renderPlot(ggplot2::autoplot(oldbn()))
               return(fluidRow(
                 column(width=6,h4("Before"),plotOutput(session$ns("oldplot")),
                        style="border-right:1px dashed #e3e3e3;"),
                 column(width=6,h4("After"),plotOutput(session$ns("newplot")))
               ))
             },
             "In-sample predictions"={
               output$newplot <- renderPlot(ggplot2::autoplot(in_sample(newbn())))
               if (!compare) return(plotOutput(session$ns("newplot")))
               output$oldplot <- renderPlot(ggplot2::autoplot(in_sample(oldbn())))
               return(fluidRow(
                 column(width=6,h4("Before"),plotOutput(session$ns("oldplot")),
                        style="border-right:1px dashed #e3e3e3;"),
                 column(width=6,h4("After"),plotOutput(session$ns("newplot")))
               ))
             },
             "Summary"={
               output$newsum <- renderPrint(print(summary(newbn()),call=FALSE))
               if (!compare) return(verbatimTextOutput(session$ns("newsum")))
               output$oldsum <- renderPrint(print(summary(oldbn()),call=FALSE))
               return(fluidRow(
                 column(width=6,h4("Before"),verbatimTextOutput(session$ns("oldsum")),
                        style="border-right:1px dashed #e3e3e3;"),
                 column(width=6,h4("After"),verbatimTextOutput(session$ns("newsum")))
               ))
             })
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
    
    newbn <- reactive({twoStepsBenchmark(hfserie(),lfserie(),
                                         include.differenciation = input$dif,
                                         include.rho = input$rho,
                                         set.coeff = set_coeff(),
                                         set.const = set_const(),
                                         start.coeff.calc = input$coeffcalc[1],
                                         end.coeff.calc = input$coeffcalc[2],
                                         start.benchmark = input$benchmark[1],
                                         end.benchmark = input$benchmark[2],
                                         start.domain = start.domain(),
                                         end.domain = end.domain())})
  })
}

reView_ui <- reView_ui_module("reView")
reView_server <- function(oldbn,benchmark.name,start.domain,end.domain,compare) {
  function(input,output,session) {
    reView_server_module("reView",reactive(oldbn),reactive(benchmark.name),reactive(start.domain),reactive(end.domain),compare)
  }
}

runapp_disaggr <- function(oldbn,benchmark.name,start.domain,end.domain,compare) {
  shinyreturn <- shiny::runApp(
    shiny::shinyApp(ui = reView_ui,
                    server = reView_server(oldbn,benchmark.name,start.domain,end.domain,compare)
    ),
    quiet = TRUE
  )
  if (inherits(shinyreturn,"error")) stop(shinyreturn)
  shinyreturn
}

#' @import shiny shinyWidgets
reView <- function(benchmark,start.domain = NULL,end.domain = NULL) {
  runapp_disaggr(benchmark,deparse(a$call),start.domain,end.domain,compare=TRUE)
}
