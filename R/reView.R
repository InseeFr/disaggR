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


presets_ggplot <- function(hfserie,lfserie) {
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

get_model <- function(benchmark) {
  model <- model.list(benchmark)
  model$set.coeff <- model$set.coefficients[names(model$set.coefficients) != "constant"]
  model$set.const <- model$set.coefficients[names(model$set.coefficients) == "constant"]
  model$set.coefficents <- NULL
  model
}

benchmarkCall <- function(benchmark,hfserie_name,lfserie_name) {
  if (is.null(benchmark)) return(NULL)
  model <- get_model(benchmark)
  
  paste0("twoStepsBenchmark(",
         "\n\thfserie = ",hfserie_name,
         ",\n\tlfserie = ",lfserie_name,
         ",\n\tinclude.differenciation = ",model$include.differenciation,
         ",\n\tinclude.rho = ", model$include.rho,
         if (!length(model$set.coeff) == 0) paste0(",\n\tset.coeff = ", display_vector(model$set.coeff)),
         if (!length(model$set.const) == 0) paste0(",\n\tset.const = ", display_vector(model$set.const)),
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
                  p("Model 1 (",em("differences \u2014 with constant",.noWS = "outside"),"): "),
                  plotOutput(ns("model1_plot"),click=ns("model1_click"),height = "200px"),
                  p("Model 3 (",em("levels \u2014 with constant",.noWS = "outside"),"):"),
                  plotOutput(ns("model3_plot"),click=ns("model3_click"),height = "200px"),
                  p("Model 5 (",em("levels \u2014 without constant",.noWS = "outside"),")"),
                  plotOutput(ns("model5_plot"),click=ns("model5_click"),height = "200px"))
           ,column(6,
                   p("Model 2 (",em("differences \u2014 without constant",.noWS = "outside"),")"),
                   plotOutput(ns("model2_plot"),click=ns("model2_click"),height = "200px"),
                   p("Model 4 (",em("autocorrelated levels \u2014 with constant",.noWS = "outside"),")"),
                   plotOutput(ns("model4_plot"),click=ns("model4_click"),height = "200px"),
                   p("Model 6 (",em("autocorrelated levels \u2014 without constant",.noWS = "outside"),")"),
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
      checkboxInput(ns("setcoeff_button"),"Coefficient",),
      conditionalPanel("input.setcoeff_button",numericInput(ns("setcoeff"),NULL,1),ns = ns),
      checkboxInput(ns("setconst_button"),"Constant"),
      conditionalPanel("input.setconst_button",numericInput(ns("setconst"),NULL,0),ns = ns),
      h4("Windows"),
      uiOutput(ns("coeffcalcsliderInput")),
      uiOutput(ns("benchmarksliderInput"))
    ),
    mainPanel(
      width = 10,
      fluidRow(
        column(12,
               radioButtons(ns("mainout_choice"),NULL,
                            choices = c("Benchmark","In-sample predictions",
                                        "Benchmark summary","Comparison with indicator",
                                        "Revisions"),
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

copyjs <- function() {
  includeScript(system.file("js/copy.js", package = "disaggR"))
}

reView_ui_module_tab3 <- function(id) {
  ns <- NS(id)
  column(12,
         tags$head(copyjs()),
         fluidRow(
           column(width=6,h4("Before"),verbatimTextOutput(ns("oldcall")),
                  style="border-right:1px dashed #e3e3e3;"),
           column(width=6,h4("After"),div(id=ns("tocopy"),verbatimTextOutput(ns("newcall"))))
         ),
         fluidRow(
           column(6,),
           column(3,actionButton(ns("Export"),"Export to PDF",width = "100%")),
           column(3,actionButton(ns("Copy"), "Copy to clipboard",
                                 width = "100%",class="btn-primary"))
           )
  )
}

#' @rdname reView
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

reView_ui <- function(old_bn) reView_ui_module("reView")

#### server ####

reView_server_module_tab1 <- function(id,hfserie,lfserie) {
  moduleServer(id,
               function(input,output,session) {
                 
                 presets_ggplot_list <- reactive(presets_ggplot(hfserie(),lfserie()))
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
                 selected_preset
               })
}

#' @importFrom gridExtra grid.arrange
tab2_mainout_switch_impl <- function(benchmark,mainout_choice,output,old_or_new,ns,oldbn=NULL) {
                                    # The oldbn arg is only for revisions
  switch(mainout_choice,
    "Benchmark" = {
      output_name <- paste0(old_or_new,"plot")
      output[[output_name]] <- renderPlot(ggplot2::autoplot(benchmark()))
      plotOutput(ns(output_name))
    },
    "In-sample predictions" = {
      output_name <- paste0(old_or_new,"plot")
      output[[output_name]] <- renderPlot(ggplot2::autoplot(in_sample(benchmark())))
      plotOutput(ns(output_name))
    },
    "Benchmark summary" = {
      output_name <- paste0(old_or_new,"verbat")
      output[[output_name]] <- renderPrint(print(summary(benchmark()),call=FALSE))
      verbatimTextOutput(ns(output_name))
    },
    "Comparison with indicator" = {
      output_name <- paste0(old_or_new,"plot")
      output[[output_name]] <- renderPlot(grid.arrange(
        ggplot2::autoplot(in_dicator(benchmark(),type="levels-rebased")),
        ggplot2::autoplot(in_dicator(benchmark(),type="changes")),
        ggplot2::autoplot(in_dicator(benchmark(),type="contributions")),
        ncol=1L, nrow = 3L
      ))
      plotOutput(ns(output_name),height="500px")
    },
    "Revisions" = {
      output_name <- paste0(old_or_new,"plot")
      output[[output_name]] <- renderPlot(grid.arrange(
        ggplot2::autoplot(in_revisions(benchmark(),oldbn(),type="levels-rebased")),
        ggplot2::autoplot(in_revisions(benchmark(),oldbn(),type="changes")),
        ggplot2::autoplot(in_revisions(benchmark(),oldbn(),type="contributions")),
        ncol=1L, nrow = 3L
      ))
      plotOutput(ns(output_name),height="500px")
    }
  )
}

tab2_mainout_switch <- function(new_bn,old_bn,mainout_choice,output,ns,compare) {
  if (compare && !(mainout_choice %in% c("Comparison with indicator","Revisions"))) {
    fluidRow(
      column(width=6,h4("Before"),
             tab2_mainout_switch_impl(old_bn,mainout_choice,output,"old",ns),
             style="border-right:1px dashed #e3e3e3;"),
      column(width=6,h4("After"),
             tab2_mainout_switch_impl(new_bn,mainout_choice,output,"new",ns))
    )
  }
  else fluidRow(column(12,tab2_mainout_switch_impl(new_bn,mainout_choice,output,"newoutput",ns,old_bn)))
}

reView_server_module_tab2 <- function(id,lfserie,hfserie,old_bn,compare,selected_preset) {
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
                 
                 observeEvent(old_bn(),{
                   model <- get_model(old_bn())
                   updateCheckboxInput(session,"dif",value = model$include.differenciation)
                   updateCheckboxInput(session,"rho",value = model$include.rho)
                   if (length(model$set.coeff) != 0) {
                     updateCheckboxInput(session,"setcoeff_button",value = TRUE)
                     updateNumericInput(session,"setcoeff",value = as.numeric(model$set.coeff))
                   } else updateCheckboxInput(session,"setcoeff_button",value = FALSE)
                   if (length(model$set.const) != 0) {
                     updateCheckboxInput(session,"setconst_button",value = TRUE)
                     updateNumericInput(session,"setconst",value = as.numeric(model$set.const))
                   } else updateCheckboxInput(session,"setconst_button",value = FALSE)
                 })
                 
                 output$coeffcalcsliderInput <- slider_windows(session$ns,lfserie(),"coeffcalc","Coefficients:")
                 output$benchmarksliderInput <- slider_windows(session$ns,lfserie(),"benchmark","Benchmark:")
                 
                 output$mainOutput <- renderUI({
                   tab2_mainout_switch(new_bn,old_bn,input$mainout_choice,output,session$ns,compare)
                 })
                 
                 new_bn <- reactive({twoStepsBenchmark(hfserie(),lfserie(),
                                                      include.differenciation = input$dif,
                                                      include.rho = input$rho,
                                                      set.coeff = {
                                                        if (input$setcoeff_button) {
                                                          if (is.na(input$setcoeff)) 0
                                                          else input$setcoeff
                                                        }
                                                        else NULL
                                                      },
                                                      set.const = {
                                                        if (input$setconst_button) {
                                                          if (is.na(input$setconst)) 0
                                                          else input$setconst
                                                        }
                                                        else NULL
                                                      },
                                                      start.coeff.calc = input$coeffcalc[1],
                                                      end.coeff.calc = input$coeffcalc[2],
                                                      start.benchmark = input$benchmark[1],
                                                      end.benchmark = input$benchmark[2])})
                 new_bn})
}

reView_server_module_tab3 <- function(id,old_bn,new_bn) {
  moduleServer(id,
               function(input,output,session) {
                 hfserie_name <- reactive(deparse(old_bn()$call$hfserie))
                 lfserie_name <- reactive(deparse(old_bn()$call$lfserie))
                 selected_call <- reactive(benchmarkCall(new_bn(),hfserie_name(),lfserie_name()))
                 output$oldcall <- renderText(benchmarkCall(old_bn(),hfserie_name(),lfserie_name()))
                 output$newcall <- renderText(selected_call())
                 observeEvent(input$Copy,{
                   session$sendCustomMessage("copy", selected_call())
                 })
               })
}

#' @rdname reView
#' @export
#' @keywords internal
reView_server_module <- function(id,old_bn,compare,function.mode=TRUE) {
  moduleServer(id,function(input, output, session) {
    
    lfserie <- reactive(model.list(old_bn())$lfserie)
    hfserie <- reactive({
      res <- model.list(old_bn())$hfserie
      res[,colnames(res) != "constant"]
    })
    
    # tab 1 : Presets
    
    selected_preset <- reView_server_module_tab1("reViewtab1",hfserie,lfserie)
    observeEvent(selected_preset(),updateNavbarPage(session,"menu","Modify"),ignoreInit = TRUE)
    
    # tab 2 : Modify
    
    new_bn <- reView_server_module_tab2("reViewtab2",lfserie,hfserie,old_bn,compare,selected_preset)

    # tab3 : Export
    
    reView_server_module_tab3("reViewtab3",old_bn,new_bn)
    
  })
}
reView_server <- function(old_bn,compare) {
  function(input,output,session) {
    reView_server_module("reView",reactive(old_bn),compare)
  }
}

#### runner ####

runapp_disaggr <- function(old_bn,compare) {
  shinyreturn <- shiny::runApp(
    shiny::shinyApp(ui = reView_ui(old_bn),
                    server = reView_server(old_bn,compare)
    ),
    quiet = TRUE
  )
  if (inherits(shinyreturn,"error")) stop(shinyreturn)
  shinyreturn
}

#' reView
#' 
#'  A shiny app to reView twoStepsBenchmarks.
#' 
#' @export
#' @import shiny
reView <- function(benchmark) {
  if (length(coef(benchmark)) > 2) stop("This reviewing application is
                                        only for univariate benchmarks.")
  runapp_disaggr(benchmark,compare=TRUE)
}
