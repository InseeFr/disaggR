slider_windows <- function(ns,initserie,ui_out,label) {
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
                           type) {
  lapply(1L:6L,function(type) {
    ggplot2::autoplot(
      in_sample(
        twoStepsBenchmark(hfserie,lfserie,
                          include.differenciation = presets$include.differenciation[type],
                          include.rho = presets$include.rho[type],
                          set.const = presets$set.const[[type]])
      )
    )
  })
}

display_vector <- function(x) switch(length(x),
                                     as.character(x),
                                     paste0("c(",do.call(paste,c(as.list(as.character(x)),sep=",")),")"))

renderBenchmarkCall <- function(benchmark,hfserie_name,lfserie_name) {
  model <- model.list(benchmark)
  
  coefs <- model$set.coefficients
  set.const <- coefs[names(coefs) == "constant"]
  set.coeff <- coefs[names(coefs) != "constant"]
  
  paste0("twoStepsBenchmark(",
         "\n\thfserie = ",hfserie_name,
         ",\n\tlfserie = ",lfserie_name,
         ",\n\tinclude.differenciation = ",model$include.differenciation,
         ",\n\tinclude.rho = ", model$include.rho,
         if (!is.null(set.coeff) && !length(set.coeff) == 0) paste0(",\n\tset.coeff = ", display_vector(set.coeff)),
         if (!is.null(set.const) && !length(set.const) == 0) paste0(",\n\tset.const = ", display_vector(set.const)),
         if (!is.null(model$start.coeff.calc)) paste0(",\n\tstart.coeff.calc = ", display_vector(model$start.coeff.calc)),
         if (!is.null(model$end.coeff.calc)) paste0(",\n\tend.coeff.calc = ", display_vector(model$end.coeff.calc)),
         if (!is.null(model$start.benchmark)) paste0(",\n\tstart.benchmark = ", display_vector(model$start.benchmark)),
         if (!is.null(model$end.benchmark)) paste0(",\n\tend.benchmark = ", display_vector(model$end.benchmark)),
         if (!is.null(model$start.domain)) paste0(",\n\tstart.domain = ", display_vector(model$start.domain)),
         if (!is.null(model$end.domain)) paste0(",\n\tend.domain = ", display_vector(model$end.domain)),
         "\n)")
}

#### ui ####

reView_ui_module_tab1 <- function(id) {
  ns <- NS(id)
  fluidRow(column(6,
                  p("Model 1 (",em("differences — with constant",.noWS = "outside"),"): "),
                  plotOutput(ns("model1_plot"),click=ns("model1_click"),height = "200px"),
                  p("Model 3 (",em("levels — with constant",.noWS = "outside"),"):"),
                  plotOutput(ns("model3_plot"),click=ns("model3_click"),height = "200px"),
                  p("Model 5 (",em("levels — without constant",.noWS = "outside"),")"),
                  plotOutput(ns("model5_plot"),click=ns("model5_click"),height = "200px"))
           ,column(6,
                   p("Model 2 (",em("differences — without constant",.noWS = "outside"),")"),
                   plotOutput(ns("model2_plot"),click=ns("model2_click"),height = "200px"),
                   p("Model 4 (",em("autocorrelated levels — with constant",.noWS = "outside"),")"),
                   plotOutput(ns("model4_plot"),click=ns("model4_click"),height = "200px"),
                   p("Model 6 (",em("autocorrelated levels — without constant",.noWS = "outside"),")"),
                   plotOutput(ns("model6_plot"),click=ns("model6_click"),height = "200px")))
}

reView_ui_module_tab2 <- function(id) {
  ns <- NS(id)
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
}

reView_ui_module_tab3 <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(width=6,h4("Before"),verbatimTextOutput(ns("oldcall")),
           style="border-right:1px dashed #e3e3e3;"),
    column(width=6,h4("After"),verbatimTextOutput(ns("newcall")))
  )
}

#' @import shiny
#' @export
#' @keywords internal
reView_ui_module <- function(id) {
  ns <- NS(id)
  navbarPage(title = "reView",id = ns("menu"),selected = "Presets",
             tabPanel("Presets",
                      reView_ui_module_tab1(ns("reViewtab1")),
             ),
             tabPanel("Modify",
                      reView_ui_module_tab2(ns("reViewtab2"))
             ),
             tabPanel("Export",
                      reView_ui_module_tab3(ns("reViewtab3"))
             )
  )
}
reView_ui <- reView_ui_module("reView")

#### server ####

reView_server_module_tab1 <- function(id,hfserie,lfserie) {
  moduleServer(id,
               function(input,output,session) {
                 
                 presets_ggplot_list <- reactive(presets_ggplot(hfserie(),lfserie(),type = type))
                 output$model1_plot <- renderPlot(presets_ggplot_list()[[1L]]  + ggplot2::theme(legend.position = "none"))
                 output$model2_plot <- renderPlot(presets_ggplot_list()[[2L]]  + ggplot2::theme(legend.position = "none"))
                 output$model3_plot <- renderPlot(presets_ggplot_list()[[3L]]  + ggplot2::theme(legend.position = "none"))
                 output$model4_plot <- renderPlot(presets_ggplot_list()[[4L]]  + ggplot2::theme(legend.position = "none"))
                 output$model5_plot <- renderPlot(presets_ggplot_list()[[5L]])
                 output$model6_plot <- renderPlot(presets_ggplot_list()[[6L]])
                 
                 selected_preset <- reactiveVal(NULL)
                 lapply(1L:6L,function(type) {
                   observeEvent(input[[paste0("model",type,"_click")]],
                                {
                                  selected_preset(NULL)
                                  selected_preset(type)
                                })
                 })
                 return(selected_preset)
               })
}

reView_server_module_tab2 <- function(id,lfserie,hfserie,oldbn,compare,selected_preset) {
  moduleServer(id,
               function(input,output,session) {
                 
                 observeEvent(selected_preset(),{
                   updateCheckboxInput(session,"dif",value = presets$include.differenciation[selected_preset()])
                   updateCheckboxInput(session,"rho",value = presets$include.rho[selected_preset()])
                   updateCheckboxInput(session,"setcoeff_button",value = FALSE)
                   setconst <- presets$set.const[[selected_preset()]]
                   updateCheckboxInput(session,"setconst_button",value = !is.null(setconst))
                   updateNumericInput(session,"setconst",value = setconst)
                 },ignoreNULL = TRUE)
                 
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
                 newbn <- reactive({twoStepsBenchmark(hfserie(),lfserie(),
                                                      include.differenciation = input$dif,
                                                      include.rho = input$rho,
                                                      set.coeff = set_coeff(),
                                                      set.const = set_const(),
                                                      start.coeff.calc = input$coeffcalc[1],
                                                      end.coeff.calc = input$coeffcalc[2],
                                                      start.benchmark = input$benchmark[1],
                                                      end.benchmark = input$benchmark[2])})
                 selected_bn <- reactiveVal(NULL)
                 observeEvent(input$validation,{
                   selected_bn(NULL)
                   selected_bn(newbn())
                 })
                 return(selected_bn)
               })
}

reView_server_module_tab3 <- function(id,oldbn,selected_bn) {
  moduleServer(id,
               function(input,output,session) {
                 hfserie_name <- reactive(deparse(oldbn()$call$hfserie))
                 lfserie_name <- reactive(deparse(oldbn()$call$lfserie))
                 output$oldcall <- reactive(renderBenchmarkCall(oldbn(),hfserie_name(),lfserie_name()))
                 output$newcall <- reactive(renderBenchmarkCall(selected_bn(),hfserie_name(),lfserie_name()))
               })
}

#' @export
#' @keywords internal
reView_server_module <- function(id,oldbn,compare,function.mode=TRUE) {
  moduleServer(id,function(input, output, session) {
    
    lfserie <- reactive(model.list(oldbn())$lfserie)
    hfserie <- reactive({
      res <- model.list(oldbn())$hfserie
      return(res[,colnames(res) != "constant"])
    })
    
    # tab 1 : Presets
    
    selected_preset <- reView_server_module_tab1("reViewtab1",hfserie,lfserie)
    observeEvent(selected_preset(),updateNavbarPage(session,"menu","Modify"),ignoreInit = TRUE)
    
    # tab 2 : Modify
    
    selected_bn <- reView_server_module_tab2("reViewtab2",lfserie,hfserie,oldbn,compare,selected_preset)
    observeEvent(selected_bn(),updateNavbarPage(session,"menu","Export"),ignoreInit = TRUE)
    
    # tab3 : Export
    
    reView_server_module_tab3("reViewtab3",oldbn,selected_bn)
    
  })
}
reView_server <- function(oldbn,compare) {
  function(input,output,session) {
    reView_server_module("reView",reactive(oldbn),compare)
  }
}

#### runner ####

runapp_disaggr <- function(oldbn,compare) {
  shinyreturn <- shiny::runApp(
    shiny::shinyApp(ui = reView_ui,
                    server = reView_server(oldbn,compare)
    ),
    quiet = TRUE
  )
  if (inherits(shinyreturn,"error")) stop(shinyreturn)
  shinyreturn
}

#' @export
#' @import shiny
reView <- function(benchmark) {
  runapp_disaggr(benchmark,compare=TRUE)
}
