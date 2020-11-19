switch_window <- function(start,end,init_tsp) {
  start <- {
    if (is.null(start)) init_tsp[1L]
    else switch(length(start),
                start,
                start[1L] + (start[2L] - 1)/init_tsp[3L])
  }
  end <- {
    if (is.null(end)) init_tsp[2L]
    else switch(length(end),
                end,
                end[1L] + (end[2L] - 1)/init_tsp[3L])
  }
  c(start,end)
}

get_clean_wins <- function(benchmark) {
  m <- model.list(benchmark)
  win_benchmark <- switch_window(m$start.benchmark,
                                 m$end.benchmark,
                                 tsp(m$lfserie))
  win_coeffcalc <- switch_window(m$start.coeff.calc,
                                 m$end.coeff.calc,
                                 tsp(m$lfserie))
  win_domain    <- switch_window(m$start.domain,
                                 m$end.domain,
                                 tsp(m$hfserie))
  
  list(benchmark = win_benchmark,
       coeff.calc = win_coeffcalc,
       domain = win_domain)
}

reView_output <- function(benchmark,benchmark_old,compare) {
  structure(list(benchmark = benchmark,
                 benchmark_old = benchmark_old,
                 hfserie_name = deparse(benchmark$call$hfserie),
                 lfserie_name = deparse(benchmark$call$lfserie),
                 compare = compare),
            class="reViewOutput")
}

plotOutBrushAndRender <- function(object,output,output_name,ns,...) {
  output[[output_name]] <- renderPlot(object())
  plotOutput(ns(output_name),
             brush = brushOpts(ns("brush"), direction = "x", resetOnNew = TRUE),
             dblclick = ns("click"),
             ...)
}

presets <- list(include.differenciation = c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE),
                include.rho = c(FALSE,FALSE,FALSE,TRUE,FALSE,TRUE),
                set.const = list(NULL,0,NULL,NULL,0,0))

presets_list_fun <- function(hfserie,lfserie,...) {
  lapply(1L:6L,function(type) {
    in_sample(
      twoStepsBenchmark(hfserie,lfserie,
                        include.differenciation = presets$include.differenciation[type],
                        include.rho = presets$include.rho[type],
                        set.const = presets$set.const[[type]])
    )
  })
}

# The function set_new_bn is made for setting new_bn in shiny
# while cleaning up its call argument

make_new_bn <- function(hfserie_name,lfserie_name,
                        hfserie,lfserie,
                        include.differenciation,include.rho,
                        set.coeff,
                        set.const,
                        start.coeff.calc,
                        end.coeff.calc,
                        start.benchmark,
                        end.benchmark,
                        start.domain,
                        end.domain) {
  
  assign(hfserie_name,hfserie)
  assign(lfserie_name,lfserie)
  
  force(include.differenciation);force(include.rho)
  force(set.coeff);force(set.const)
  force(start.coeff.calc);force(end.coeff.calc)
  force(start.benchmark);force(end.benchmark)
  force(start.domain);force(end.domain)
  
  bn <- eval(substitute(twoStepsBenchmark(hfserie = hfserie_arg,lfserie = lfserie_arg,
                                          include.differenciation = include.differenciation_arg,
                                          include.rho = include.rho_arg,
                                          set.coeff = set.coeff_arg,
                                          set.const = set.const_arg,
                                          start.coeff.calc = start.coeff.calc_arg,
                                          end.coeff.calc = end.coeff.calc_arg,
                                          start.benchmark = start.benchmark_arg,
                                          end.benchmark = end.benchmark_arg,
                                          start.domain = start.domain_arg,
                                          end.domain = end.domain_arg),
                        list(hfserie_arg = parse(text=hfserie_name)[[1L]],
                             lfserie_arg = parse(text=lfserie_name)[[1L]],
                             include.differenciation_arg = include.differenciation,
                             include.rho_arg = include.rho,
                             set.coeff_arg = set.coeff,
                             set.const_arg = set.const,
                             start.coeff.calc_arg = start.coeff.calc,
                             end.coeff.calc_arg = end.coeff.calc,
                             start.benchmark_arg = start.benchmark,
                             end.benchmark_arg = end.benchmark,
                             start.domain_arg = start.domain,
                             end.domain_arg = end.domain)))
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
    tags$style(type = "text/css", paste0(".",ns("presetplot")," {width: calc(100vh - 19px);height: calc(33vh - 52px);}")),
    column(6,
           p("Model 1 (",em("differences \u2014 with constant",.noWS = "outside"),"): "),
           div(plotOutput(ns("model1_plot"),click=ns("model1_click"),height = "100%"),class=ns("presetplot")),
           p("Model 3 (",em("levels \u2014 with constant",.noWS = "outside"),"):"),
           div(plotOutput(ns("model3_plot"),click=ns("model3_click"),height = "100%"),class=ns("presetplot")),
           p("Model 5 (",em("levels \u2014 without constant",.noWS = "outside"),")"),
           div(plotOutput(ns("model5_plot"),click=ns("model5_click"),height = "100%"),class=ns("presetplot")))
    ,column(6,
            p("Model 2 (",em("differences \u2014 without constant",.noWS = "outside"),")"),
            div(plotOutput(ns("model2_plot"),click=ns("model2_click"),height = "100%"),class=ns("presetplot")),
            p("Model 4 (",em("autocorrelated levels \u2014 with constant",.noWS = "outside"),")"),
            div(plotOutput(ns("model4_plot"),click=ns("model4_click"),height = "100%"),class=ns("presetplot")),
            p("Model 6 (",em("autocorrelated levels \u2014 without constant",.noWS = "outside"),")"),
            div(plotOutput(ns("model6_plot"),click=ns("model6_click"),height = "100%"),class=ns("presetplot")))
  )
}

boxstyle <- "padding: 6px 8px;
             margin-top: 6px;
             margin-bottom: 6px;
             background-color: #fdfdfd;
             border: 1px solid #e3e3e3;
             border-radius: 4px;
             -webkit-box-shadow: inset 0 1px 1px rgba(0, 0, 0, 0.03);
             box-shadow: inset 0 1px 1px rgba(0, 0, 0, 0.03);"


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
        tags$style(type = "text/css", paste0(".",ns("mainouttitle")," {height: calc(100vh - 158px);}\n",
                                             ".",ns("mainoutmono")," {height: calc(100vh - 138px);}")),
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
             tags$script(
               paste0(
                 "Shiny.addCustomMessageHandler('",ns("closewindow"),"' closewindow );\n\n",
                 "function closewindow(anymessage) {window.close();}"
                 
               )
             ),
             tags$script(
               paste0(
                 "Shiny.addCustomMessageHandler('",ns("copy"),"', copy );\n\n",
                 "function copy(text) {
                    var input = document.createElement('textarea');
                    input.innerHTML = text;
                    document.body.appendChild(input);
                    input.select();
                    document.execCommand('copy');
                    document.body.removeChild(input);
                  }")
             ),
             column(3,actionButton(ns("Reset"),"Reset",width = "100%",
                                   class="btn-warning")),
             column(3,actionButton(ns("Quit"),"Quit",width = "100%",
                                   class="btn-danger")),
             column(3,downloadButton(ns("Export"),"Export to RDS",style="width:100%;")),
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
  uiOutput(ns("navbar"))
}

reView_ui <- reView_ui_module("reView")

#### server ####

reView_server_tab1 <- function(id,hfserie,lfserie) {
  moduleServer(id,
               function(input,output,session) {
                 
                 presets_list <- reactive(presets_list_fun(hfserie(),lfserie()))
                 
                 selected_preset <- reactiveVal(NULL)
                 
                 lapply(1L:6L, function(n) {
                   output[[paste0("model",n,"_plot")]] <- renderPlot(plot(presets_list()[[n]]))
                 })
                 
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

reView_server_tab2_switch_impl <- function(benchmark,mainout_choice,plotswin,output,old_or_new,ns,old_bn = NULL) {
  
  switch(old_or_new,
         old={
           title <- div("Before",class="section")
           outputclass <- ns("mainouttitle")
         }
         ,
         new={
           title <- div("After",class="section")
           outputclass <- ns("mainouttitle")
         },
         mono={
           title <- NULL
           outputclass <- ns("mainoutmono")
         })
  
  # The old_bn arg is only for revisions
  switch(mainout_choice,
         "Benchmark" = {
           fluidRow(
             column(12,
                    title,
                    div(
                      plotOutBrushAndRender(reactive(plot(benchmark(),
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
                      plotOutBrushAndRender(reactive(plot(in_sample(benchmark(),
                                                                    type="levels"),
                                                          start=plotswin()[1L],
                                                          end=plotswin()[2L])),
                                            output,
                                            paste0(old_or_new,"plotlev"),
                                            ns,
                                            height="50%"),
                      plotOutBrushAndRender(reactive(plot(in_sample(benchmark(),
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
                    div(plotOutBrushAndRender(reactive(plot(in_dicator(benchmark(),
                                                                       type="levels-rebased"),
                                                            start=plotswin()[1L],
                                                            end=plotswin()[2L])),
                                              output,
                                              paste0(old_or_new,"plotlev"),
                                              ns,
                                              height="33%"),
                        plotOutBrushAndRender(reactive(plot(in_dicator(benchmark(),
                                                                       type="changes"),
                                                            start=plotswin()[1L],
                                                            end=plotswin()[2L])),
                                              output,
                                              paste0(old_or_new,"plotcha"),
                                              ns,
                                              height="33%"),
                        plotOutBrushAndRender(reactive(plot(in_dicator(benchmark(),
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
                    div(plotOutBrushAndRender(reactive(plot(in_revisions(benchmark(),old_bn(),
                                                                         type="levels"),
                                                            start=plotswin()[1L],
                                                            end=plotswin()[2L])),
                                              output,
                                              paste0(old_or_new,"plotlev"),
                                              ns,
                                              height="33%"),
                        plotOutBrushAndRender(reactive(plot(in_revisions(benchmark(),old_bn(),
                                                                         type="changes"),
                                                            start=plotswin()[1L],
                                                            end=plotswin()[2L])),
                                              output,
                                              paste0(old_or_new,"plotcha"),
                                              ns,
                                              height="33%"),
                        plotOutBrushAndRender(reactive(plot(in_revisions(benchmark(),old_bn(),
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

reView_server_tab2 <- function(id,lfserie,hfserie,
                               hfserie_name,lfserie_name,
                               old_bn,compare,
                               selected_preset,reset) {
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
                   updateSliderInput(session,"coeffcalc",value = clean_wins_old_bn()$coeff.calc)
                   updateSliderInput(session,"benchmark",value = clean_wins_old_bn()$benchmark)
                   updateSliderInput(session,"plotswin",value = maxwin())
                   if (length(model$set.coeff) != 0) {
                     updateCheckboxInput(session,"setcoeff_button",value = TRUE)
                     updateNumericInput(session,"setcoeff",value = as.numeric(model$set.coeff))
                   } else updateCheckboxInput(session,"setcoeff_button",value = FALSE)
                   if (length(model$set.const) != 0) {
                     updateCheckboxInput(session,"setconst_button",value = TRUE)
                     updateNumericInput(session,"setconst",value = as.numeric(model$set.const))
                   } else updateCheckboxInput(session,"setconst_button",value = FALSE)
                 })
                 
                 observeEvent(compare(),{
                   updateRadioButtons(session,"mainout_choice", NULL,
                                      choices = c("Benchmark","In-sample predictions",
                                                  "Benchmark summary","Comparison with indicator",
                                                  if (compare()) "Revisions"),
                                      selected = "Benchmark",
                                      inline = TRUE)
                 })
                 
                 tsplf <- reactive(tsp(lfserie()))
                 
                 tsphf <- reactive(tsp(hfserie()))
                 
                 clean_wins_old_bn <- reactive(get_clean_wins(old_bn()))
                 
                 output$coeffcalcsliderInput <- {
                   renderUI(
                     sliderInput(session$ns("coeffcalc"),"Coefficients:",
                                 min = tsplf()[1L],max = tsplf()[2L],
                                 value = clean_wins_old_bn()$coeff.calc,
                                 step = 1/tsplf()[3L],sep = " ")
                   )
                 }
                 
                 output$benchmarksliderInput <- {
                   renderUI(
                     sliderInput(session$ns("benchmark"),"Benchmark:",
                                 min = tsplf()[1L],max = tsplf()[2L],
                                 value = clean_wins_old_bn()$benchmark,
                                 step = 1/tsplf()[3L],sep = " ")
                   )
                 }
                 
                 maxwin <- reactive({
                   
                   startmin <- floor(min(tsphf()[1L],tsplf()[1L],
                                         clean_wins_old_bn()$benchmark[1L],
                                         clean_wins_old_bn()$coeff.calc[1L]))
                   
                   endmax <- floor(max(tsphf()[2L],tsplf()[2L],
                                       clean_wins_old_bn()$benchmark[2L],
                                       clean_wins_old_bn()$coeff.calc[2L]))
                   
                   c(startmin,endmax)
                 })
                 
                 output$plotswinsliderInput <- {
                   renderUI(
                     sliderInput(session$ns("plotswin"),"Plots:",
                                 min = maxwin()[1L],max = maxwin()[2L],
                                 value = c(maxwin()[1L],maxwin()[2L]),
                                 step = 1/tsplf()[3L],sep = " ")
                   )
                 }
                 
                 output$mainOutput <- renderUI({
                   reView_server_tab2_switch(new_bn,old_bn,
                                             input$mainout_choice,
                                             reactive(c(input$plotswin[1L],
                                                        input$plotswin[2L]+
                                                          deltat(lfserie())-
                                                          deltat(hfserie()))),
                                             output,session$ns,compare())
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
                                     value = maxwin)
                 },
                 ignoreNULL = TRUE)
                 
                 new_bn <- reactive({
                   make_new_bn(hfserie_name(),lfserie_name(),
                               hfserie(),lfserie(),
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
                               end.benchmark = input$benchmark[2],
                               start.domain = model.list(old_bn())$start.domain,
                               end.domain = model.list(old_bn())$end.domain)})
                 new_bn})
}

#' @importFrom rmarkdown render
reView_server_tab3 <- function(id,old_bn,new_bn,hfserie_name,lfserie_name,compare) {
  moduleServer(id,
               function(input,output,session) {
                 new_call_text <- reactive(benchmarkCall(new_bn(),hfserie_name(),lfserie_name()))
                 old_call_text <- reactive(if (compare()) benchmarkCall(old_bn(),hfserie_name(),lfserie_name())
                                           else "No model to compare.")
                 output$oldcall <- renderText(old_call_text())
                 output$newcall <- renderText(new_call_text())
                 
                 file_name <- reactive(paste("benchmark",hfserie_name(),lfserie_name(),sep="-"))
                 
                 output$Export <- downloadHandler(
                   filename = paste0("benchmark-",hfserie_name(),"-",lfserie_name(),".rds"),
                   content = function(file) saveRDS(reView_output(old_bn(),new_bn(),compare()),file)
                 )
                 
                 session$onSessionEnded(function() {
                   if (Sys.getenv('SHINY_PORT') == "") isolate(stopApp(reView_output(old_bn(),new_bn(),compare())))
                 })
                 
                 observeEvent(input$Quit,{
                   if (Sys.getenv('SHINY_PORT') == "") stopApp(reView_output(old_bn(),new_bn(),compare()))
                   else session$sendCustomMessage(session$ns("closewindow"), "anymessage")
                 })
                 
                 observeEvent(input$Copy,{
                   session$sendCustomMessage(session$ns("copy"), new_call_text())
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
reView_server_module <- function(id,old_bn,hfserie_name,lfserie_name,compare) {
  moduleServer(id,function(input, output, session) {
    
    output$navbar <- renderUI({
      navbarPage(title = paste("reView:",hfserie_name(),"on", lfserie_name()),
                 id = session$ns("menu"),
                 selected = "Presets",
                 tags$style(".section { font-family: 'Source Sans Pro', sans-serif; font-weight: 420; line-height: 20px; text-align: center;}"),
                 tabPanel("Presets",
                          reView_ui_tab1(session$ns("reViewtab1")),
                 ),
                 tabPanel("Modify",
                          reView_ui_tab2(session$ns("reViewtab2"))
                 ),
                 tabPanel("Export",
                          reView_ui_tab3(session$ns("reViewtab3"))
                 ))
    })
    
    lfserie <- reactive(model.list(old_bn())$lfserie)
    hfserie <- reactive({
      res <- model.list(old_bn())$hfserie
      res[,colnames(res) != "constant"]
    })
    
    # tab 1 : Presets
    
    selected_preset <- reView_server_tab1("reViewtab1",hfserie,lfserie)
    observeEvent(selected_preset(),updateNavbarPage(session,"menu","Modify"),ignoreInit = TRUE)
    
    # tab 2 : Modify
    
    new_bn <- reView_server_tab2("reViewtab2",lfserie,hfserie,
                                 hfserie_name,lfserie_name,
                                 old_bn,compare,
                                 selected_preset,reset)
    
    # tab3 : Export
    
    reset <- reView_server_tab3("reViewtab3",old_bn,new_bn,hfserie_name,lfserie_name,compare)
    observeEvent(reset(),updateNavbarPage(session,"menu","Modify"),ignoreInit = TRUE)    
  })
}
reView_server <- function(old_bn,hfserie_name,lfserie_name,compare) {
  function(input,output,session) {
    reView_server_module("reView",reactive(old_bn),
                         reactive(hfserie_name),reactive(lfserie_name),
                         reactive(compare))
  }
}

#### runner ####

runapp_reView <- function(old_bn,hfserie_name,lfserie_name,compare) {
  shinyreturn <- shiny::runApp(
    shiny::shinyApp(ui = reView_ui,
                    server = reView_server(old_bn,
                                           hfserie_name,lfserie_name,
                                           compare)
    ),
    quiet = TRUE
  )
  if (inherits(shinyreturn,"error")) shinyreturn <- NULL
  invisible(shinyreturn)
}

#' A shiny app to reView and modify twoStepsBenchmarks
#'
#' reView allows the user to easily access diverse outputs in order to
#' review a benchmark object, made with \link{twoStepsBenchmark}.
#'  
#' The `hfserie_name` and `lfserie_name` define :
#'
#' * the default file name of the RDS file
#' * the names of the series in the output `call` element
#'  
#' By default, these are set as defined in their `call` element.
#'  
#' The app is made of exported \pkg{shiny} modules in order to allow integration
#' in a wider non-local application. In the module part, every input
#' are defined as reactive variables.
#'
#' @param object a twoStepsBenchmark with an univariate hfserie, a reViewOutput,
#' or a character of length 1 with the path of their RDS file. If a reViewOutput
#' is chosen, the former new benchmark is taken as the old one.
#' @param hfserie_name a character of length 1. The name of the hfserie.
#' @param lfserie_name a character of length 1. The name of the lfserie.
#' @param compare a boolean of length 1, that tells if the outputs of
#' the old benchmark should be displayed.
#'
#' @return a list, of class reViewOutput, containing the new benchmark,
#' the old one, the names of the series and the boolean compare.
#' This object can also be saved in RDS format through the app.
#' The reViewOutput object can be displayed as a html report with the same
#' informations than in shiny, with the \link{rePort} method.
#'
#' @seealso rePort
#'
#' @examples
#' \dontrun{
#' reView(twoStepsBenchmark(turnover,construction))
#' }
#' 
#' @export
#' @import shiny
reView <- function(object,
                   hfserie_name = NULL,
                   lfserie_name = NULL,
                   compare = TRUE) {
  UseMethod("reView")
}


#' @export
reView.character <- function(object,
                             hfserie_name = NULL,
                             lfserie_name = NULL,
                             compare = TRUE, ...) {
  reView(readRDS(object),hfserie_name,lfserie_name,compare)
}

#' @export
reView.connection <- function(object,
                              hfserie_name = NULL,
                              lfserie_name = NULL,
                              compare = TRUE) {
  reView(readRDS(object),hfserie_name,lfserie_name,compare)
}

#' @export
reView.reViewOutput <- function(object,
                                hfserie_name = NULL,
                                lfserie_name = NULL,
                                compare = TRUE) {
  reView(object$benchmark,
         hfserie_name = {
           if (is.null(hfserie_name)) object$hfserie_name
           else hfserie_name
         },
         lfserie_name = {
           if (is.null(lfserie_name)) object$lfserie_name
           else lfserie_name
         },
         compare = compare)
}

#' @export
reView.twoStepsBenchmark <- function(object,
                                     hfserie_name = NULL,
                                     lfserie_name = NULL,
                                     compare = TRUE) {
  if (is.null(hfserie_name)) hfserie_name <- deparse(benchmark$call$hfserie)
  if (is.null(lfserie_name)) lfserie_name <- deparse(benchmark$call$lfserie)
  if (length(coef(benchmark)) > 2) stop("This reviewing application is
                                        only for univariate benchmarks.")
  runapp_reView(benchmark,hfserie_name,lfserie_name,compare=compare)
}

#' Producing a report
#' 
#' This function takes an output of the \link{reView} \pkg{shiny} application
#' and produces an html report with the same outputs than in shiny.
#' 
#' It can also directly take a \link{twoStepsBenchmark} as an input.
#' 
#' @param object a twoStepsBenchmark with an univariate hfserie, a reViewOutput,
#' or a character of length 1 with the path of their RDS file. If a reViewOutput
#' is chosen, the former new benchmark is taken as the old one.
#' @param output_file The file in which the html should be saved. If `NULL`
#' the file is temporary, and opened in a tab of the default browser.
#' @param ... other arguments passed to rmarkdown::render
#' 
#' @seealso reView
#' 
#' @export
rePort <- function(object, output_file = NULL, ...) UseMethod("rePort")

#' @export
rePort.character <- function(object, output_file = NULL, ...)
  rePort(readRDS(object), output_file, ...)

#' @export
rePort.connection <- function(object, output_file = NULL, ...)
  rePort(readRDS(object), output_file, ...)

#' @export
rePort.twoStepsBenchmark <- function(object, output_file = NULL, ...) {
  rePort(reView_output(object,benchmark_old=NULL,compare=FALSE),
         output_file,
         ...)
}

#' @export
rePort.reViewOutput <- function(object, output_file = NULL, ...) {
  temp_dir <- tempdir()
  temp_rmd <- file.path(temp_dir, "report.Rmd")
  temp_html <- file.path(temp_dir, "report.html")
  
  if (object$compare) file.copy(system.file("rmd/report.Rmd", package = "disaggR"), temp_rmd, overwrite = TRUE)
  else file.copy(system.file("rmd/report_nocompare.Rmd", package = "disaggR"), temp_rmd, overwrite = TRUE)
  rmarkdown::render(temp_rmd,output_file=temp_html,
                    params = list(new_bn=object$benchmark,
                                  old_bn=object$benchmark_old,
                                  hfserie_name=object$hfserie_name,
                                  lfserie_name=object$lfserie_name),
                    envir = new.env(parent = globalenv()),
                    ...)
  if (is.null(output_file))  utils::browseURL(temp_html)
  else file.copy(temp_html, output_file, overwrite = TRUE)
  invisible()
}

#' @export
print.reViewOutput <- function(x, ...) rePort(x, output_file=NULL, ...) 