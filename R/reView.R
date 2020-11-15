copyjs <- function() includeScript(system.file("js/copy.js", package = "disaggR"))
closewindowjs <- function() includeScript(system.file("js/closewindow.js", package = "disaggR"))

plotOutBrushAndRender <- function(object,output,output_name,ns,...) {
  output[[output_name]] <- renderPlot(object())
  plotOutput(ns(output_name),
             brush = brushOpts(ns("brush"), direction = "x", resetOnNew = TRUE),
             dblclick = ns("click"),
             ...)
}

#' @importFrom grid grid.draw
draw_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  grid.draw(tmp$grobs[[leg]])
}

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


presets_ggplot <- function(hfserie,lfserie,...) {
  lapply(1L:6L,function(type) {
    autoplot(
      in_sample(
        twoStepsBenchmark(hfserie,lfserie,
                          include.differenciation = presets$include.differenciation[type],
                          include.rho = presets$include.rho[type],
                          set.const = presets$set.const[[type]])
      ), ...
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

reView_ui_tab1 <- function(id) {
  ns <- NS(id)
  fluidRow(
    tags$style(type = "text/css", ".presetplot {height: calc(33vh - 62px) !important;}"),
    column(6,
           p("Model 1 (",em("differences \u2014 with constant",.noWS = "outside"),"): "),
           div(plotOutput(ns("model1_plot"),click=ns("model1_click"),height = "100%"),class="presetplot"),
           p("Model 3 (",em("levels \u2014 with constant",.noWS = "outside"),"):"),
           div(plotOutput(ns("model3_plot"),click=ns("model3_click"),height = "100%"),class="presetplot"),
           p("Model 5 (",em("levels \u2014 without constant",.noWS = "outside"),")"),
           div(plotOutput(ns("model5_plot"),click=ns("model5_click"),height = "100%"),class="presetplot"))
    ,column(6,
            p("Model 2 (",em("differences \u2014 without constant",.noWS = "outside"),")"),
            div(plotOutput(ns("model2_plot"),click=ns("model2_click"),height = "100%"),class="presetplot"),
            p("Model 4 (",em("autocorrelated levels \u2014 with constant",.noWS = "outside"),")"),
            div(plotOutput(ns("model4_plot"),click=ns("model4_click"),height = "100%"),class="presetplot"),
            p("Model 6 (",em("autocorrelated levels \u2014 without constant",.noWS = "outside"),")"),
            div(plotOutput(ns("model6_plot"),click=ns("model6_click"),height = "100%"),class="presetplot")),
    column(12,plotOutput(ns("legend"),height="30px"))
  )
}

boxstyle <- "padding: 6px 8px;
             margin-top: 6px;
             margin-bottom: 6px;
             background-color: #fdfdfd;
             border: 1px solid #e3e3e3;
             border-radius: 4px;
             -webkit-box-shadow: inset 0 1px 1px rgba(0, 0, 0, 0.05);
             box-shadow: inset 0 1px 1px rgba(0, 0, 0, 0.05);"


lrmargins <- "margin-left: 3px;
              margin-right: 3px"

reView_ui_tab2 <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 2,
      div("Include",class="section"),
      checkboxInput(ns("dif"),"Differenciation"),
      checkboxInput(ns("rho"),"Rho"),
      div("Set",class="section"),
      checkboxInput(ns("setcoeff_button"),"Coefficient",),
      conditionalPanel("input.setcoeff_button",numericInput(ns("setcoeff"),NULL,1),ns = ns),
      checkboxInput(ns("setconst_button"),"Constant"),
      conditionalPanel("input.setconst_button",numericInput(ns("setconst"),NULL,0),ns = ns),
      div("Windows",class="section"),
      uiOutput(ns("coeffcalcsliderInput")),
      uiOutput(ns("benchmarksliderInput")),
      uiOutput(ns("plotswinsliderInput")),
      style="padding-top: 5px;
             padding-bottom: 0px;
             margin-top: 0px;
             margin-bottom: 0px;"
    ),
    mainPanel(
      width = 10,
      fluidRow(
        tags$style(type = "text/css", ".mainouttitle {height: calc(100vh - 158px) !important;}
                                       .mainoutmono {height: calc(100vh - 138px) !important;"),
        column(12,
               radioButtons(ns("mainout_choice"),NULL,
                            choices = c("Benchmark","In-sample predictions",
                                        "Benchmark summary","Comparison with indicator",
                                        "Revisions"),
                            selected = "Benchmark",inline=TRUE)
        ),
        align="center"
      ),
      uiOutput(ns("mainOutput"))
    )
  )
}

reView_ui_tab3 <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12,
           fluidRow(
             column(width=6,div("Before",class="section"),verbatimTextOutput(ns("oldcall"))),
             column(width=6,div("After",class="section"),div(id=ns("tocopy"),verbatimTextOutput(ns("newcall"))))
           ),
           fluidRow(
             column(3,actionButton(ns("Reset"),"Reset",width = "100%",
                                   class="btn-warning")),
             column(3,actionButton(ns("Quit"),"Quit",width = "100%",
                                   class="btn-danger")),
             column(3,downloadButton(ns("Export"),"Export to PDF",style="width:100%;")),
             column(3,actionButton(ns("Copy"), "Copy to clipboard",
                                   width = "100%",class="btn-primary"))
           ),
           style = boxstyle
    ),style=lrmargins)
}

#' @rdname reView
#' @export
#' @keywords internal
reView_ui_module <- function(id) {
  ns <- NS(id)
  navbarPage(title = "reView",id = ns("menu"),selected = "Presets",
             tags$head(copyjs(),
                       closewindowjs()),
             tags$style(".section { font-family: 'Source Sans Pro', sans-serif; font-weight: 420; line-height: 20px; text-align: center;}"),
             tabPanel("Presets",
                      reView_ui_tab1(ns("reViewtab1")),
             ),
             tabPanel("Modify",
                      reView_ui_tab2(ns("reViewtab2"))
             ),
             tabPanel("Export",
                      reView_ui_tab3(ns("reViewtab3"))
             )
  )
}

reView_ui <- function(old_bn) reView_ui_module("reView")

#### server ####

reView_server_tab1 <- function(id,hfserie,lfserie) {
  moduleServer(id,
               function(input,output,session) {
                 
                 presets_ggplot_list <- reactive(presets_ggplot(hfserie(),lfserie()))
                 
                 output$legend <- renderPlot(draw_legend(presets_ggplot_list()[[1L]]))
                 
                 lapply(1L:6L, function(n) {
                   output[[paste0("model",n,"_plot")]] <- renderPlot(presets_ggplot_list()[[n]] +
                                                                       theme(legend.position = "none"))
                 })
                 
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

reView_server_tab2_switch_impl <- function(benchmark,mainout_choice,plotswin,output,old_or_new,ns,oldbn=NULL) {
  
  switch(old_or_new,
         old={
           title <- div("Before",class="section")
           outputclass <- "mainouttitle"
         }
         ,
         new={
           title <- div("After",class="section")
           outputclass <- "mainouttitle"
         },
         mono={
           title <- NULL
           outputclass <- "mainoutmono"
         })
  
  # The oldbn arg is only for revisions
  switch(mainout_choice,
         "Benchmark" = {
           fluidRow(
             column(12,
                    title,
                    div(
                      plotOutBrushAndRender(reactive(autoplot(benchmark(),
                                                              start=plotswin()[1L],
                                                              end=plotswin()[2L])),
                                            output,
                                            paste0(old_or_new,"plot"),
                                            ns,height="100%"),class=outputclass)))
         },
         "In-sample predictions" = {
           fluidRow(
             column(12,
                    title,
                    div(
                      plotOutBrushAndRender(reactive(autoplot(in_sample(benchmark(),
                                                                        type="levels"),
                                                              start=plotswin()[1L],
                                                              end=plotswin()[2L])),
                                            output,
                                            paste0(old_or_new,"plotlev"),
                                            ns,
                                            height="50%"),
                      plotOutBrushAndRender(reactive(autoplot(in_sample(benchmark(),
                                                                        type="changes"),
                                                              start=plotswin()[1L],
                                                              end=plotswin()[2L])),
                                            output,
                                            paste0(old_or_new,"plotcha"),
                                            ns,
                                            height="50%"),class=outputclass)
             )
           )
         },
         "Benchmark summary" = {
           output_name <- paste0(old_or_new,"verbat")
           output[[output_name]] <- renderPrint(print(summary(benchmark()),call=FALSE))
           fluidRow(column(12,title,
                           div(verbatimTextOutput(ns(output_name)),
                               class=outputclass)))
         },
         "Comparison with indicator" = {
           fluidRow(
             column(12,
                    title,
                    div(plotOutBrushAndRender(reactive(autoplot(in_dicator(benchmark(),
                                                                           type="levels-rebased"),
                                                                start=plotswin()[1L],
                                                                end=plotswin()[2L])),
                                              output,
                                              paste0(old_or_new,"plotlev"),
                                              ns,
                                              height="33%"),
                        plotOutBrushAndRender(reactive(autoplot(in_dicator(benchmark(),
                                                                           type="changes"),
                                                                start=plotswin()[1L],
                                                                end=plotswin()[2L])),
                                              output,
                                              paste0(old_or_new,"plotcha"),
                                              ns,
                                              height="33%"),
                        plotOutBrushAndRender(reactive(autoplot(in_dicator(benchmark(),
                                                                           type="contributions"),
                                                                start=plotswin()[1L],
                                                                end=plotswin()[2L])),
                                              output,
                                              paste0(old_or_new,"plotctb"),
                                              ns,
                                              height="33%"),class=outputclass)
             )
           )
         },
         "Revisions" = {
           fluidRow(
             column(12,
                    title,
                    div(plotOutBrushAndRender(reactive(autoplot(in_revisions(benchmark(),oldbn(),
                                                                             type="levels"),
                                                                start=plotswin()[1L],
                                                                end=plotswin()[2L])),
                                              output,
                                              paste0(old_or_new,"plotlev"),
                                              ns,
                                              height="33%"),
                        plotOutBrushAndRender(reactive(autoplot(in_revisions(benchmark(),oldbn(),
                                                                             type="changes"),
                                                                start=plotswin()[1L],
                                                                end=plotswin()[2L])),
                                              output,
                                              paste0(old_or_new,"plotcha"),
                                              ns,
                                              height="33%"),
                        plotOutBrushAndRender(reactive(autoplot(in_revisions(benchmark(),oldbn(),
                                                                             type="contributions"),
                                                                start=plotswin()[1L],
                                                                end=plotswin()[2L])),
                                              output,
                                              paste0(old_or_new,"plotctb"),
                                              ns,
                                              height="33%"),class=outputclass)
             ))
         }
  )
}

reView_server_tab2_switch <- function(new_bn,old_bn,mainout_choice,plotswin,output,ns,compare) {
  if (compare && !(mainout_choice %in% c("Comparison with indicator","Revisions"))) {
    fluidRow(
      column(6,fluidRow(column(12,reView_server_tab2_switch_impl(old_bn,mainout_choice,plotswin,output,"old",ns),
                               style=boxstyle),style=lrmargins)),
      column(6,fluidRow(column(12,
                               reView_server_tab2_switch_impl(new_bn,mainout_choice,plotswin,output,"new",ns),
                               style=boxstyle),style="margin-right: 6px"))
    )
  }
  else fluidRow(column(12,reView_server_tab2_switch_impl(new_bn,mainout_choice,plotswin,output,"mono",ns,old_bn),
                       style=boxstyle),style=lrmargins)
}

reView_server_tab2 <- function(id,lfserie,hfserie,old_bn,compare,selected_preset,reset) {
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
                 
                 observeEvent({reset();old_bn()},{
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
                 output$plotswinsliderInput <- slider_windows(session$ns,lfserie(),"plotswin","Plots:")
                 
                 output$mainOutput <- renderUI({
                   reView_server_tab2_switch(new_bn,old_bn,
                                             input$mainout_choice,
                                             reactive(c(input$plotswin[1L],
                                                        input$plotswin[2L]+
                                                          deltat(lfserie())-
                                                          deltat(hfserie()))),
                                             output,session$ns,compare)
                 })
                 
                 observeEvent(input$brush,{
                   tsplf <- tsp(lfserie())
                   updateSliderInput(session, "plotswin",
                                     value = c(round(tsplf[3L]*(input$brush$xmin-tsplf[1L])) + tsplf[1L],
                                               round(tsplf[3L]*(input$brush$xmax-tsplf[2L])) + tsplf[2L]))
                   session$resetBrush("brush")
                 },
                 ignoreNULL = TRUE)
                 
                 observeEvent(input$click,{
                   tsplf <- tsp(lfserie())
                   updateSliderInput(session, "plotswin",
                                     value = c(tsplf[1L],tsplf[2L]))
                 },
                 ignoreNULL = TRUE)
                 
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

#' @importFrom rmarkdown render
reView_server_tab3 <- function(id,old_bn,new_bn) {
  moduleServer(id,
               function(input,output,session) {
                 hfserie_name <- reactive(deparse(old_bn()$call$hfserie))
                 lfserie_name <- reactive(deparse(old_bn()$call$lfserie))
                 new_call_text <- reactive(benchmarkCall(new_bn(),hfserie_name(),lfserie_name()))
                 old_call_text <- reactive(benchmarkCall(old_bn(),hfserie_name(),lfserie_name()))
                 output$oldcall <- renderText(old_call_text())
                 output$newcall <- renderText(new_call_text())
                 
                 file_name <- reactive(paste("benchmark",hfserie_name(),lfserie_name(),sep="-"))
                 
                 output$Export <- downloadHandler(
                   filename = paste0(file_name(),".pdf"),
                   content = function(file) {
                     withProgress(message = "Generating PDF. Please wait...", {
                       setProgress(0, "Creating temporary files")
                       temp_report <- file.path(tempdir(), "report.Rmd")
                       file.copy(system.file("rmd/report.Rmd", package = "disaggR"), temp_report, overwrite = TRUE)
                       params <- list(new_bn=new_bn(),
                                      old_bn=old_bn(),
                                      new_call_text=new_call_text(),
                                      old_call_text=old_call_text(),
                                      file_name=file_name(),
                                      session=session)
                       pdf <- rmarkdown::render(temp_report,output_file = file,
                                                params = params,
                                                envir = new.env(parent = globalenv()),
                                                output_format = "pdf_document")
                       pdf
                     })
                   },contentType="application/octet-stream"
                 )
                 
                 session$onSessionEnded(function() {
                   if (Sys.getenv('SHINY_PORT') == "") stopApp()
                 })
                 
                 observeEvent(input$Quit,{
                   if (Sys.getenv('SHINY_PORT') == "") stopApp(new_bn())
                   else session$sendCustomMessage("closewindow", "anymessage")
                 })
                 
                 observeEvent(input$Copy,{
                   session$sendCustomMessage("copy", new_call_text())
                   showModal(
                     modalDialog(title = "reView",
                                 "New model copied in the clipboard !",
                                 easyClose = TRUE,
                                 footer = NULL)
                   )
                 })
                 
                 reactive(input$Reset)
               })
}

#' @rdname reView
#' @export
#' @keywords internal
reView_server_module <- function(id,old_bn,compare) {
  moduleServer(id,function(input, output, session) {
    
    lfserie <- reactive(model.list(old_bn())$lfserie)
    hfserie <- reactive({
      res <- model.list(old_bn())$hfserie
      res[,colnames(res) != "constant"]
    })
    
    # tab 1 : Presets
    
    selected_preset <- reView_server_tab1("reViewtab1",hfserie,lfserie)
    observeEvent(selected_preset(),updateNavbarPage(session,"menu","Modify"),ignoreInit = TRUE)
    
    # tab 2 : Modify
    
    new_bn <- reView_server_tab2("reViewtab2",lfserie,hfserie,old_bn,compare,selected_preset,reset)
    
    # tab3 : Export
    
    reset <- reView_server_tab3("reViewtab3",old_bn,new_bn)
    observeEvent(reset(),updateNavbarPage(session,"menu","Modify"),ignoreInit = TRUE)    
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
  if (inherits(shinyreturn,"error")) shinyreturn <- NULL
  invisible(shinyreturn)
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
