#' @import shiny
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
        uiOutput(ns("validationbutton"))
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

shinyBenchmark_server_module <- function(id,oldbn,benchmark.name,start.domain,end.domain,compare,function.mode=TRUE) {
  moduleServer(id,function(input, output, session) {
    
    lfserie <- reactive(model.list(oldbn())$lfserie)
    hfserie <- reactive({
      res <- model.list(oldbn())$hfserie
      return(res[,colnames(res) != "constant"])
    })
    
    output$titlePanel <- renderUI(titlePanel(paste0("shinyBenchmark : ",benchmark.name())))
    
    output$coeffcalcsliderInput <- slider_windows(session$ns,lfserie,"coeffcalc","Coefficients:")
    output$benchmarksliderInput <- slider_windows(session$ns,lfserie,"benchmark","Benchmark:")
    
    if (function.mode) {
      output$validationbutton <- renderUI(actionButton(session$ns("validation"),"Validate"))
      observeEvent(input$validation,stopApp(`attr<-`(newbn(),"ValidationDate",Sys.Date())))
      onSessionEnded(function() stopApp(structure(list(message = "shinyBenchmark cancelled", call = NULL),
                                                  class = c("error", "condition"))))
    } else output$validationbutton <- NULL
    
    output$mainOutput <- renderUI({
      switch(input$plotchoice,
             benchmark={
               output$newplot <- renderPlot(ggplot2::autoplot(newbn()))
               if (!compare) return(plotOutput(session$ns("newplot")))
               output$oldplot <- renderPlot(ggplot2::autoplot(oldbn()))
               return(fluidRow(
                 column(width=6,h5(tags$u("Before"),align="center"),plotOutput(session$ns("oldplot"))),
                 column(width=6,h5(tags$u("After"),align="center"),plotOutput(session$ns("newplot")))
               ))
             },
             insample={
               output$newplot <- renderPlot(ggplot2::autoplot(in_sample(newbn())))
               if (!compare) return(plotOutput(session$ns("newplot")))
               output$oldplot <- renderPlot(ggplot2::autoplot(in_sample(oldbn())))
               return(fluidRow(
                 column(width=6,h5(tags$u("Before"),align="center"),plotOutput(session$ns("oldplot"))),
                 column(width=6,h5(tags$u("After"),align="center"),plotOutput(session$ns("newplot")))
               ))
             },
             summary={
               output$newsum <- renderPrint(print(summary(newbn()),call=FALSE))
               if (!compare) return(verbatimTextOutput(session$ns("newsum")))
               output$oldsum <- renderPrint(print(summary(oldbn()),call=FALSE))
               return(fluidRow(
                 column(width=6,h5(tags$u("Before"),align="center"),verbatimTextOutput(session$ns("oldsum"))),
                 column(width=6,h5(tags$u("After"),align="center"),verbatimTextOutput(session$ns("newsum")))
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

shinyBenchmark_ui <- shinyBenchmark_ui_module("shinyBenchmark")
shinyBenchmark_server <- function(oldbn,benchmark.name,start.domain,end.domain,compare,function.mode) {
  function(input,output,session) {
    shinyBenchmark_server_module("shinyBenchmark",reactive(oldbn),reactive(benchmark.name),reactive(start.domain),reactive(end.domain),compare,function.mode)
  }
}

runapp_disaggr <- function(oldbn,benchmark.name,start.domain,end.domain,compare) {
  shinyreturn <- shiny::runApp(
    shiny::shinyApp(ui = shinyBenchmark_ui,
                    server = shinyBenchmark_server(oldbn,benchmark.name,start.domain,end.domain,compare,TRUE)
    ),
    quiet = TRUE
  )
  if (inherits(shinyreturn,"error")) stop(shinyreturn)
  shinyreturn
}

#' @import shiny
viewBenchmark <- function(benchmark,start.domain = NULL,end.domain = NULL) {
  runapp_disaggr(benchmark,deparse(a$call),start.domain,end.domain,compare=TRUE)
}
