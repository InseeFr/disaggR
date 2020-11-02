#' @import shiny
reView_ui_module <- function(id) {
  ns <- NS(id)
  navbarPage(title = "reView",id = ns("menu"),selected = "Presets",
             tabPanel("Presets",
                      fluidRow(column(6,
                                      plotOutput(ns("model1_plot"),click=ns("model1_click"),height = "200px"),
                                      plotOutput(ns("model3_plot"),click=ns("model3_click"),height = "200px"),
                                      plotOutput(ns("model5_plot"),click=ns("model3_click"),height = "200px"))
                               ,column(6,
                                       plotOutput(ns("model2_plot"),click=ns("model2_click"),height = "200px"),
                                       plotOutput(ns("model4_plot"),click=ns("model4_click"),height = "200px"),
                                       plotOutput(ns("model6_plot"),click=ns("model6_click"),height = "200px")))),
             tabPanel("Modify",
                      sidebarLayout(
                        sidebarPanel(
                          width = 2,
                          tags$style("h4 { font-family: 'Source Sans Pro', sans-serif; font-weight: 400; line-height: 32px; text-align: center;}"),
                          h4("Include"),
                          checkboxInput(ns("dif"),"Differenciation"),
                          checkboxInput(ns("rho"),"Rho"),
                          h4("Set"),
                          checkboxInput(ns("setcoeff_button"),"Coefficient"),
                          conditionalPanel("input.setcoeff_button",numericInput(ns("setcoeff"),NULL,1),ns = ns),
                          checkboxInput(ns("setconst_button"),"Constant"),
                          conditionalPanel("input.setconst_button",numericInput(ns("setconst"),NULL,0),ns = ns),
                          h4("Windows"),
                          uiOutput(ns("coeffcalcsliderInput")),
                          uiOutput(ns("benchmarksliderInput")),
                          uiOutput(ns("domainsliderInput")),
                          actionButton(ns("validation"),"Validate",width = "100%")
                        ),
                        mainPanel(
                          width = 10,
                          fluidRow(
                            column(12,
                                   radioButtons(ns("plotchoice"),NULL,
                                                choices = c("Benchmark","In-sample predictions","Summary"),
                                                selected = "Benchmark",inline=TRUE)
                            ),
                            align="center"
                          ),
                          uiOutput(ns("mainOutput"),
                                   style = "padding: 6px 8px;
                                   margin-top: 6px;
                                   margin-bottom: 6px;
                                   background-color: #ffffff;
                                   border: 1px solid #e3e3e3;
                                   border-radius: 2px;")
                        )
                      )
             ),
             tabPanel("Export",
                      fluidRow(
                        column(width=6,h4("Before"),verbatimTextOutput(ns("oldcall")),
                               style="border-right:1px dashed #e3e3e3;"),
                        column(width=6,h4("After"),verbatimTextOutput(ns("newcall")))
                      )
             )
  )
}

slider_windows <- function(ns,initserie,ui_out,label,extend=0) {
  renderUI(
    sliderInput(ns(ui_out),
                label,
                min = tsp(initserie)[1] - ceiling(length(initserie) * extend/100) * deltat(initserie),
                max = tsp(initserie)[2] + ceiling(length(initserie) * extend/100) * deltat(initserie),
                value = c(tsp(initserie)[1],tsp(initserie)[2]),
                step = deltat(initserie),
                sep = " ")
  )
}

reView_server_module <- function(id,oldbn,benchmark.name,compare,function.mode=TRUE) {
  moduleServer(id,function(input, output, session) {
    
    lfserie <- reactive(model.list(oldbn())$lfserie)
    hfserie <- reactive({
      res <- model.list(oldbn())$hfserie
      return(res[,colnames(res) != "constant"])
    })
    
    model1 <- reactive({twoStepsBenchmark(hfserie(),lfserie(),
                                          include.differenciation = TRUE,
                                          include.rho = FALSE,
                                          set.coeff = NULL,
                                          set.const = NULL,
                                          start.coeff.calc = input$coeffcalc[1],
                                          end.coeff.calc = input$coeffcalc[2],
                                          start.benchmark = input$benchmark[1],
                                          end.benchmark = input$benchmark[2],
                                          start.domain = input$domain[1],
                                          end.domain = input$domain[2])})
    model2 <- reactive({twoStepsBenchmark(hfserie(),lfserie(),
                                          include.differenciation = TRUE,
                                          include.rho = FALSE,
                                          set.coeff = NULL,
                                          set.const = 0,
                                          start.coeff.calc = input$coeffcalc[1],
                                          end.coeff.calc = input$coeffcalc[2],
                                          start.benchmark = input$benchmark[1],
                                          end.benchmark = input$benchmark[2],
                                          start.domain = input$domain[1],
                                          end.domain = input$domain[2])})
    model3 <- reactive({twoStepsBenchmark(hfserie(),lfserie(),
                                          include.differenciation = FALSE,
                                          include.rho = FALSE,
                                          set.coeff = NULL,
                                          set.const = NULL,
                                          start.coeff.calc = input$coeffcalc[1],
                                          end.coeff.calc = input$coeffcalc[2],
                                          start.benchmark = input$benchmark[1],
                                          end.benchmark = input$benchmark[2],
                                          start.domain = input$domain[1],
                                          end.domain = input$domain[2])})
    model4 <- reactive({twoStepsBenchmark(hfserie(),lfserie(),
                                          include.differenciation = FALSE,
                                          include.rho = TRUE,
                                          set.coeff = NULL,
                                          set.const = NULL,
                                          start.coeff.calc = input$coeffcalc[1],
                                          end.coeff.calc = input$coeffcalc[2],
                                          start.benchmark = input$benchmark[1],
                                          end.benchmark = input$benchmark[2],
                                          start.domain = input$domain[1],
                                          end.domain = input$domain[2])})
    model5 <- reactive({twoStepsBenchmark(hfserie(),lfserie(),
                                          include.differenciation = FALSE,
                                          include.rho = FALSE,
                                          set.coeff = NULL,
                                          set.const = 0,
                                          start.coeff.calc = input$coeffcalc[1],
                                          end.coeff.calc = input$coeffcalc[2],
                                          start.benchmark = input$benchmark[1],
                                          end.benchmark = input$benchmark[2],
                                          start.domain = input$domain[1],
                                          end.domain = input$domain[2])})
    model6 <- reactive({twoStepsBenchmark(hfserie(),lfserie(),
                                          include.differenciation = FALSE,
                                          include.rho = TRUE,
                                          set.coeff = NULL,
                                          set.const = 0,
                                          start.coeff.calc = input$coeffcalc[1],
                                          end.coeff.calc = input$coeffcalc[2],
                                          start.benchmark = input$benchmark[1],
                                          end.benchmark = input$benchmark[2],
                                          start.domain = input$domain[1],
                                          end.domain = input$domain[2])})
   
    output$model1_plot <- renderPlot(ggplot2::autoplot(in_sample(model1())) + ggplot2::theme(legend.position = "none"))
    output$model2_plot <- renderPlot(ggplot2::autoplot(in_sample(model2())) + ggplot2::theme(legend.position = "none"))
    output$model3_plot <- renderPlot(ggplot2::autoplot(in_sample(model3())) + ggplot2::theme(legend.position = "none"))
    output$model4_plot <- renderPlot(ggplot2::autoplot(in_sample(model4())) + ggplot2::theme(legend.position = "none"))
    output$model5_plot <- renderPlot(ggplot2::autoplot(in_sample(model5())))
    output$model6_plot <- renderPlot(ggplot2::autoplot(in_sample(model6())))
    
    observeEvent(input$model1_click,{
      updateCheckboxInput(session,"dif",value = TRUE)
      updateCheckboxInput(session,"rho",value = FALSE)
      updateCheckboxInput(session,"setcoeff_button",value = FALSE)
      updateCheckboxInput(session,"setconst_button",value = FALSE)
      updateNavbarPage(session,"menu","Modify")
    })
    
    observeEvent(input$model2_click,{
      updateCheckboxInput(session,"dif",value = TRUE)
      updateCheckboxInput(session,"rho",value = FALSE)
      updateCheckboxInput(session,"setcoeff_button",value = FALSE)
      updateCheckboxInput(session,"setconst_button",value = TRUE)
      updateNumericInput(session,"setconst",value = 0)
      updateNavbarPage(session,"menu","Modify")
    })
    
    observeEvent(input$model3_click,{
      updateCheckboxInput(session,"dif",value = FALSE)
      updateCheckboxInput(session,"rho",value = FALSE)
      updateCheckboxInput(session,"setcoeff_button",value = FALSE)
      updateCheckboxInput(session,"setconst_button",value = FALSE)
      updateNavbarPage(session,"menu","Modify")
    })
    
    observeEvent(input$model4_click,{
      updateCheckboxInput(session,"dif",value = FALSE)
      updateCheckboxInput(session,"rho",value = TRUE)
      updateCheckboxInput(session,"setcoeff_button",value = FALSE)
      updateCheckboxInput(session,"setconst_button",value = FALSE)
      updateNavbarPage(session,"menu","Modify")
    })
    
    observeEvent(input$model5_click,{
      updateCheckboxInput(session,"dif",value = FALSE)
      updateCheckboxInput(session,"rho",value = FALSE)
      updateCheckboxInput(session,"setcoeff_button",value = FALSE)
      updateCheckboxInput(session,"setconst_button",value = TRUE)
      updateNumericInput(session,"setconst",value = 0)
      updateNavbarPage(session,"menu","Modify")
    })
    
    observeEvent(input$model6_click,{
      updateCheckboxInput(session,"dif",value = FALSE)
      updateCheckboxInput(session,"rho",value = TRUE)
      updateCheckboxInput(session,"setcoeff_button",value = FALSE)
      updateCheckboxInput(session,"setconst_button",value = TRUE)
      updateNumericInput(session,"setconst",value = 0)
      updateNavbarPage(session,"menu","Modify")
    })
    
    output$coeffcalcsliderInput <- slider_windows(session$ns,lfserie(),"coeffcalc","Coefficients:")
    output$benchmarksliderInput <- slider_windows(session$ns,lfserie(),"benchmark","Benchmark:")
    output$domainsliderInput <- slider_windows(session$ns,lfserie(),"domain","Domain:",extend=10)
    
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
    
    observeEvent(input$validation, updateNavbarPage(session,"menu","Export"))

    output$oldcall <- renderText(deparse(oldbn()$call)) # a ameliorer
    output$newcall <- renderText(paste0("twoStepsBenchmark(",
                                         "hfserie = ",deparse(oldbn()$call$hfserie),",\n\t",
                                         "lfserie = ",deparse(oldbn()$call$lfserie),",\n\t",
                                         "include.differenciation = ",input$dif,",\n\t",
                                         "include.rho = ", input$rho,",\n\t",
                                         "set.coeff = ", set_coeff(),",\n\t",
                                         "set.const = ", set_const(),",\n\t",
                                         "start.coeff.calc = ", input$coeffcalc[1],",\n\t",
                                         "end.coeff.calc = ", input$coeffcalc[2],",\n\t",
                                         "start.benchmark = ", input$benchmark[1],",\n\t",
                                         "end.benchmark = ", input$benchmark[2],",\n\t",
                                         "start.domain = ", input$domain[1],",\n\t",
                                         "end.domain = ", input$domain[2]))
    
    newbn <- reactive({twoStepsBenchmark(hfserie(),lfserie(),
                                         include.differenciation = input$dif,
                                         include.rho = input$rho,
                                         set.coeff = set_coeff(),
                                         set.const = set_const(),
                                         start.coeff.calc = input$coeffcalc[1],
                                         end.coeff.calc = input$coeffcalc[2],
                                         start.benchmark = input$benchmark[1],
                                         end.benchmark = input$benchmark[2],
                                         start.domain = input$domain[1],
                                         end.domain = input$domain[2])})
  })
}

reView_ui <- reView_ui_module("reView")
reView_server <- function(oldbn,benchmark.name,compare) {
  function(input,output,session) {
    reView_server_module("reView",reactive(oldbn),reactive(benchmark.name),compare)
  }
}

runapp_disaggr <- function(oldbn,benchmark.name,compare) {
  shinyreturn <- shiny::runApp(
    shiny::shinyApp(ui = reView_ui,
                    server = reView_server(oldbn,benchmark.name,compare)
    ),
    quiet = TRUE
  )
  if (inherits(shinyreturn,"error")) stop(shinyreturn)
  shinyreturnl
}

#' @import shiny
reView <- function(benchmark) {
  runapp_disaggr(benchmark,deparse(a$call),compare=TRUE)
}
