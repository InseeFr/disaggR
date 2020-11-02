#' @import shiny
reView_ui_module <- function(id) {
  ns <- NS(id)
  navbarPage(title = "reView",id = ns("menu"),selected = "Presets",
             tabPanel("Presets",
                      fluidRow(column(6,
                                      p("Model 1 (",em("differences — with constant",.noWS = "outside"),"): "),
                                      plotOutput(ns("model1_plot"),click=ns("model1_click"),height = "200px"),
                                      p("Model 3 (",em("levels - with constant",.noWS = "outside"),"):"),
                                      plotOutput(ns("model3_plot"),click=ns("model3_click"),height = "200px"),
                                      p("Model 5 (",em("levels",.noWS = "outside"),")"),
                                      plotOutput(ns("model5_plot"),click=ns("model5_click"),height = "200px"))
                               ,column(6,
                                       p("Model 2 (",em("differences",.noWS = "outside"),")"),
                                       plotOutput(ns("model2_plot"),click=ns("model2_click"),height = "200px"),
                                       p("Model 4 (",em("levels - with constant and rho",.noWS = "outside"),")"),
                                       plotOutput(ns("model4_plot"),click=ns("model4_click"),height = "200px"),
                                       p("Model 6 (",em("levels - with rho",.noWS = "outside"),")"),
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
                min = tsp(initserie)[1],
                max = tsp(initserie)[2],
                value = c(tsp(initserie)[1],tsp(initserie)[2]),
                step = deltat(initserie),
                sep = " ")
  )
}

presets <- list(include.differenciation = c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE),
                include.rho = c(FALSE,FALSE,FALSE,TRUE,FALSE,TRUE),
                set.const = list(NULL,0,NULL,NULL,0,0))

presets_ggplot <- function(hfserie,lfserie,
                           start.coeff.calc,end.coeff.calc,
                           start.benchmark,end.benchmark,
                           type) {
  lapply(1L:6L,function(type) {
    ggplot2::autoplot(
      in_sample(
        twoStepsBenchmark(hfserie,lfserie,
                          include.differenciation = presets$include.differenciation[type],
                          include.rho = presets$include.rho[type],
                          set.const = presets$set.const[[type]],
                          start.coeff.calc = start.coeff.calc,
                          end.coeff.calc = end.coeff.calc,
                          start.benchmark = start.benchmark,
                          end.benchmark = end.benchmark)
      )
    )
  })
}

presets_observeClicks <- function(session,input) {
  lapply(1L:6L,
         function(type) {
           observeEvent(input[[paste0("model",type,"_click")]],{
             updateCheckboxInput(session,"dif",value = presets$include.differenciation[type])
             updateCheckboxInput(session,"rho",value = presets$include.rho[type])
             updateCheckboxInput(session,"setcoeff_button",value = FALSE)
             setconst <- presets$set.const[[type]]
             updateCheckboxInput(session,"setconst_button",value = !is.null(setconst))
             updateNumericInput(session,"setconst",value = setconst)
             updateNavbarPage(session,"menu","Modify")
           })
         })
}

reView_server_module <- function(id,oldbn,benchmark.name,compare,function.mode=TRUE) {
  moduleServer(id,function(input, output, session) {
    
    lfserie <- reactive(model.list(oldbn())$lfserie)
    hfserie <- reactive({
      res <- model.list(oldbn())$hfserie
      return(res[,colnames(res) != "constant"])
    })
    
    presets_ggplot_list <- reactive(presets_ggplot(hfserie(),lfserie(),
                                                   start.coeff.calc = input$coeffcalc[1],
                                                   end.coeff.calc = input$coeffcalc[2],
                                                   start.benchmark = input$benchmark[1],
                                                   end.benchmark = input$benchmark[2],
                                                   type = type))
    output$model1_plot <- renderPlot(presets_ggplot_list()[[1L]]  + ggplot2::theme(legend.position = "none"))
    output$model2_plot <- renderPlot(presets_ggplot_list()[[2L]]  + ggplot2::theme(legend.position = "none"))
    output$model3_plot <- renderPlot(presets_ggplot_list()[[3L]]  + ggplot2::theme(legend.position = "none"))
    output$model4_plot <- renderPlot(presets_ggplot_list()[[4L]]  + ggplot2::theme(legend.position = "none"))
    output$model5_plot <- renderPlot(presets_ggplot_list()[[5L]])
    output$model6_plot <- renderPlot(presets_ggplot_list()[[6L]])
    
    presets_observeClicks(session,input)
    
    output$coeffcalcsliderInput <- slider_windows(session$ns,lfserie(),"coeffcalc","Coefficients:")
    output$benchmarksliderInput <- slider_windows(session$ns,lfserie(),"benchmark","Benchmark:")
    
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
                                        "end.benchmark = ", input$benchmark[2],",\n\t"))
    
    newbn <- reactive({twoStepsBenchmark(hfserie(),lfserie(),
                                         include.differenciation = input$dif,
                                         include.rho = input$rho,
                                         set.coeff = set_coeff(),
                                         set.const = set_const(),
                                         start.coeff.calc = input$coeffcalc[1],
                                         end.coeff.calc = input$coeffcalc[2],
                                         start.benchmark = input$benchmark[1],
                                         end.benchmark = input$benchmark[2])})
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

#' @export
#' @import shiny
reView <- function(benchmark) {
  runapp_disaggr(benchmark,deparse(a$call),compare=TRUE)
}
