cssmainoutwithtitle <- "{height: calc(100vh - 158px);}"

cssmainoutwithouttitle <- "{height: calc(100vh - 138px);}"

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

lfserie <- function(benchmark) model.list(benchmark)$lfserie
hfserie_alone <- function(benchmark) {
  res <- model.list(benchmark)$hfserie
  res[,!(colnames(res) %in% c("constant",
                              names(outliers(benchmark))))]
}

info_switch <- function(mainout_choice)
  switch(mainout_choice,
         "Scatter plot" = {
           shiny::HTML("These scatter plots display the relationship between",
                       "both series after aggregation and, eventually,",
                       "differenciation.<br><br>The regression line is computed",
                       "with the coefficients of the benchmark. The main arrows",
                       "represent the values inside the coefficients calculation",
                       "window, when the secondary ones, dashed by default,",
                       "stand for the points that are external to the",
                       "regression. These are eventually added through the",
                       "benchmark window.<br><br>The X coordinates are the",
                       "aggregated high-frequency series and the Y coordinates are",
                       "the low-frequency series minus the estimated outliers",
                       "effects, if any.")
         },
         "In-sample predictions" = {
           shiny::HTML("These plots display in-sample predictions produced with",
                       "the prais-winsten regression of the benchmark.<br><br>",
                       "The predicted values are different from the fitted values:",
                       "<ul><li>they are eventually reintegrated</li>",
                       "<li>they contain the autocorrelated part of the",
                       "residuals</li></ul>Besides, changes are relative to ",
                       "latest benchmark value, not the latest predicted",
                       "value.")
         },
         "Comparison benchmark/input" = {
           shiny::HTML("These plots compare the input high-frequency series with",
                       "the resulting time-series of the benchmark. These are",
                       "intended to check that the information of the input is",
                       "preserved.<br><br>If the indicator isn't correlated to",
                       "the account, its coefficient tends to zero, and the",
                       "benchmark to a simple smooth. If so, the contributions",
                       "of the smoothed part become high.<br><br>Denton",
                       "benchmarks don't change with the level of their",
                       "high-frequency input series. Hence, the level of the ",
                       "smoothed part can be seen as arbitrary.",
                       "Hence it is omitted in these plots.")
         },
         "Revisions" = {
           shiny::HTML("These plots display the differences between the former",
                       "benchmark and the newer one.")
         },
         "Benchmark plot" = {
           shiny::HTML("Two-steps benchmarks bend a time series with a",
                       "time series of lower frequency. The procedure involved",
                       "is a Prais-Winsten regression, then an additive Denton",
                       "benchmark.<br><br>Therefore, the resulting time series",
                       "is the sum of a regression fit, eventually reintegrated,",
                       "and of a smoothed part. The smoothed part minimizes",
                       "the sum of squares of its differences.<br><br>This plot",
                       "displays both the bending series, disaggregated with an",
                       "evenly distribution, and the resulting time series of",
                       "the benchmark.")
         },
         "Benchmark summary" = {
           shiny::HTML("The portmanteau test here is of lag 1. If the test is",
                       "positive, either <i>include.differenciation</i> or",
                       "<i>include.rho</i> should be set to TRUE.")
         })

info_dialog <- function(session,mainout_choice) {
  shiny::showModal(
    session = session,
    shiny::modalDialog(
      title = mainout_choice,
      info_switch(mainout_choice),
      easyClose = TRUE,
      footer = {
        if (mainout_choice == "Scatter plot")
          shiny::HTML("<center><b>Double-click</b> to reset plot window</center>")
        else shiny::HTML("<div style=\"display:flex;justify-content: space-evenly\">",
                         "<div><b>Brush</b> to change plot window</div>",
                         "<div><b>Double-click</b> to reset it</div>",
                         "</div>")
      },
      fade = FALSE))
  
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

get_maxwin <- function(benchmark) {
  
  tsphf <- tsp(hfserie_alone(benchmark))
  
  verysmall <- getOption("ts.eps")/tsphf[3L]
  
  startmin <- floor(tsphf[1L]+verysmall)
  
  endmax <- floor(tsphf[2L]+verysmall)
  
  c(startmin,endmax)
}

reViewOutput <- function(benchmark,benchmark_old,
                         compare) {
  structure(list(benchmark = benchmark,
                 benchmark_old = benchmark_old,
                 hfserie_name = benchmark$call$hfserie,
                 lfserie_name = benchmark$call$lfserie,
                 compare = compare),
            class="reViewOutput")
}

plotOutBrushAndRender <- function(object,plotswin,output,output_name,ns,
                                  is.brush=TRUE,height,...) {
  output[[output_name]] <- {
    if (is.null(tryCatch(object(),error=function(e) NULL))) NULL
    else shiny::renderPlot(plot(object(),
                                start=plotswin()[1L],
                                end=plotswin()[2L],
                                ...))
  }
  
  shiny::plotOutput(ns(output_name),
                    brush = if (is.brush) shiny::brushOpts(ns("brush"),
                                                           direction = "x",
                                                           resetOnNew = TRUE),
                    dblclick = ns("click"),
                    height = height)
}

presets <- list(include.differenciation = c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE),
                include.rho = c(FALSE,FALSE,FALSE,TRUE,FALSE,TRUE),
                set.const = list(NULL,0,NULL,NULL,0,0),
                label = c("differences - with constant",
                          "differences - without constant",
                          "levels - with constant",
                          "autocorrelated levels - with constant",
                          "levels - without constant",
                          "autocorrelated levels - without constant"
                ))

get_preset <- function(benchmark) {
  
  if (is.null(benchmark)) return(NA)
  
  m <- get_model(benchmark)
  
  match <- which(m$include.differenciation == presets$include.differenciation &
                   m$include.rho == presets$include.rho &
                   vapply(presets$set.const,
                          function(x,y) if ((length(x) == 0 &&
                                             length(y) == 0) ||
                                            isTRUE(x == y)) TRUE else FALSE,
                          m$set.const,
                          FUN.VALUE = TRUE) &
                   length(m$set.coeff) == 0)
  
  if (length(match) == 0) NA else match
  
}

presets_list_fun <- function(hfserie,lfserie,...) {
  lapply(1L:6L,function(type) {
    twoStepsBenchmark(hfserie,lfserie,
                      include.differenciation = presets$include.differenciation[type],
                      include.rho = presets$include.rho[type],
                      set.const = presets$set.const[[type]],
                      ...)
  })
}

valid_identifier <- function(x) {
  is.character(x) &&
    grepl("^((([[:alpha:]]|\\.[[:alpha:]_\\.])[[:alnum:]_\\.]*)|\\.)$", x) &&
    ! grepl("^(..[0-9]+|...)$", x)
}

# Class union for the series name, can be a language object (usually coming from
# an expression sent to reView) or a valid identifier character
reViewName <- function(x) {
  if (is.language(x)) x
  else if (valid_identifier(x)) as.symbol(x)
  else stop("The names provided to reView should be either a language object or a valid identifier character",
            call. = FALSE)
}

# The function make_new_bn is made for setting new_bn in shiny
# while cleaning up the call to evaluated args

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
                        end.domain,
                        outliers) {
  
  twoStepsBenchmark(hfserie = hfserie,
                    lfserie = lfserie,
                    include.differenciation = include.differenciation,
                    include.rho = include.rho,
                    set.coeff = set.coeff,
                    set.const = set.const,
                    start.coeff.calc = start.coeff.calc,
                    end.coeff.calc = end.coeff.calc,
                    start.benchmark = start.benchmark,
                    end.benchmark = end.benchmark,
                    start.domain = start.domain,
                    end.domain = end.domain,
                    outliers = outliers,
                    cl = 
                      call("twoStepsBenchmark",
                           hfserie = hfserie_name,
                           lfserie = lfserie_name,
                           include.differenciation = include.differenciation,
                           include.rho = include.rho,
                           set.coeff = set.coeff,
                           set.const = set.const,
                           start.coeff.calc = start.coeff.calc,
                           end.coeff.calc = end.coeff.calc,
                           start.benchmark = start.benchmark,
                           end.benchmark = end.benchmark,
                           start.domain = start.domain,
                           end.domain = end.domain,
                           outliers = outliers))
  
}

zero_if_na <- function(x) if (is.na(x)) 0 else x

clean_set_coeff <- function(set_hfserie,new_bn_external_setter) {
  
  set_coeff <- model.list(new_bn_external_setter)$set.coefficients
  set_coeff <- set_coeff[names(set_coeff) != "constant"]
  
  set_outliers <- set_coeff[intersect(names(set_coeff),
                                      names(outliers(new_bn_external_setter)))]
  
  
  if (length(set_outliers) == 0L) set_hfserie else {
    if (!is.null(set_hfserie)) names(set_hfserie) <- "hfserie"
    c(set_hfserie,set_outliers)
  }
}

get_new_bn <- function(input,hfserie_name,lfserie_name,new_bn_external_setter) {
  
  tryCatch(
    make_new_bn(hfserie_name(),lfserie_name(),
                hfserie_alone(new_bn_external_setter()),lfserie(new_bn_external_setter()),
                include.differenciation = input$dif,
                include.rho = input$rho,
                set.coeff = clean_set_coeff(
                  if (input$setcoeff_button) zero_if_na(input$setcoeff)
                  else NULL,
                  new_bn_external_setter()),
                set.const = {
                  if (input$setconst_button) zero_if_na(input$setconst)
                  else NULL
                },
                start.coeff.calc = input$coeffcalc[1],
                end.coeff.calc = input$coeffcalc[2],
                start.benchmark = input$benchmark[1],
                end.benchmark = input$benchmark[2],
                start.domain = model.list(new_bn_external_setter())$start.domain,
                end.domain = model.list(new_bn_external_setter())$end.domain,
                outliers = outliers(new_bn_external_setter())),
    error = function(e) NULL)
}

set_inputs_to_default <- function(session,new_bn_external_setter) {
  tsplf <- tsp(lfserie(new_bn_external_setter()))
  model <- get_model(new_bn_external_setter())
  maxwin <- get_maxwin(new_bn_external_setter())
  clean_wins_new_bn_external_setter <- get_clean_wins(new_bn_external_setter())
  shiny::updateCheckboxInput(session,"dif",value = model$include.differenciation)
  shiny::updateCheckboxInput(session,"rho",value = model$include.rho)
  shiny::updateSliderInput(session,"coeffcalc",
                           min = tsplf[1L],max = tsplf[2L],
                           value = clean_wins_new_bn_external_setter$coeff.calc,
                           step = 1/tsplf[3L])
  shiny::updateSliderInput(session,"benchmark",
                           min = tsplf[1L],max = tsplf[2L],
                           value = clean_wins_new_bn_external_setter$benchmark,
                           step = 1/tsplf[3L])
  shiny::updateSliderInput(session,"plotswin",
                           min = maxwin[1L],max = maxwin[2L],
                           value = c(maxwin[1L],maxwin[2L]),
                           step = 1/tsplf[3L])
  
  set_coeff_wo_outliers <- model$set.coeff
  if (!is.null(names(set_coeff_wo_outliers))) {
    set_coeff_wo_outliers <-
      set_coeff_wo_outliers[names(set_coeff_wo_outliers) == "hfserie"]
  }
  
  if (length(set_coeff_wo_outliers) != 0) {
    shiny::updateCheckboxInput(session,"setcoeff_button",value = TRUE)
    shiny::updateNumericInput(session,"setcoeff",value = as.numeric(set_coeff_wo_outliers))
  } else shiny::updateCheckboxInput(session,"setcoeff_button",value = FALSE)
  if (length(model$set.const) != 0) {
    shiny::updateCheckboxInput(session,"setconst_button",value = TRUE)
    shiny::updateNumericInput(session,"setconst",value = as.numeric(model$set.const))
  } else shiny::updateCheckboxInput(session,"setconst_button",value = FALSE)
}

set_plots_window_with_brush <- function(session,input,old_bn) {
  tsplf <- tsp(lfserie(old_bn()))
  shiny::updateSliderInput(session, "plotswin",
                           value = c(round(tsplf[3L]*(input$brush$xmin-tsplf[1L])) + tsplf[1L],
                                     round(tsplf[3L]*(input$brush$xmax-tsplf[2L])) + tsplf[2L]))
  session$resetBrush("brush")
}

set_preset <- function(session,selected_preset_tab1) {
  shiny::updateCheckboxInput(session,"dif",value = presets$include.differenciation[selected_preset_tab1()])
  shiny::updateCheckboxInput(session,"rho",value = presets$include.rho[selected_preset_tab1()])
  shiny::updateCheckboxInput(session,"setcoeff_button",value = FALSE)
  setconst <- presets$set.const[[selected_preset_tab1()]]
  shiny::updateCheckboxInput(session,"setconst_button",value = !is.null(setconst))
  shiny::updateNumericInput(session,"setconst",value = setconst)
}

display_vector <- function(x) {
  if (!is.null(names(x))) paste0("c(",
                                 paste(Map(paste0,names(x),"=",as.character(x)),collapse=","),
                                 ")")
  else if (length(x) == 1L) as.character(x)
  else paste0("c(",paste(as.character(x),collapse=","),")")
}

display_outliers <- function(x) {
  paste0("list(",
         paste(Map(paste0,names(x),"=",lapply(x,display_vector)),collapse=","),
         ")")
}

get_model <- function(benchmark) {
  model <- model.list(benchmark)
  model$set.coeff <- model$set.coefficients[names(model$set.coefficients) != "constant"]
  
  if (ncol(model$hfserie)-length(outliers(benchmark)) == 2L &&
      length(model$set.coeff) == 1L &&
      !(names(model$set.coeff) %in% names(outliers(benchmark)))) {
    model$set.coeff <- unname(model$set.coeff)
  }
  
  model$set.const <- unname(model$set.coefficients[names(model$set.coefficients) == "constant"])
  model$set.coefficients <- NULL
  model
}

get_benchmark_call <- function(benchmark,hfserie_name,lfserie_name) {
  if (is.null(benchmark)) return(NULL)
  model <- get_model(benchmark)
  outliers <- outliers(benchmark)
  
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
         if (!is.null(outliers)) paste0(",\n\toutliers = ", display_outliers(outliers)),
         "\n)")
}

#### ui ####

reView_ui_tab1 <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(shiny::fluidRow(
    shiny::tags$style(type = "text/css", paste0(".",ns("presetplot"),cssmainoutwithouttitle)),
    shiny::column(12,
                  shiny::radioButtons(ns("firsttab_choice"),NULL,
                                      choices = c("In-sample changes","Summary table"),
                                      selected = "In-sample changes",inline=TRUE),
                  align="center"
    )),
    shiny::div(shiny::uiOutput(ns("firstTabOutput")),style=boxstyle)
  )
}

reView_ui_tab2 <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 2,
      shiny::div("Include",class="section"),
      shiny::checkboxInput(ns("dif"),"Differenciation"),
      shiny::checkboxInput(ns("rho"),"Rho"),
      shiny::div("Set",class="section"),
      shiny::checkboxInput(ns("setcoeff_button"),"Coefficient",),
      shiny::conditionalPanel("input.setcoeff_button",shiny::numericInput(ns("setcoeff"),NULL,1),ns = ns),
      shiny::checkboxInput(ns("setconst_button"),"Constant"),
      shiny::conditionalPanel("input.setconst_button",shiny::numericInput(ns("setconst"),NULL,0),ns = ns),
      shiny::div("Windows",class="section"),
      shiny::sliderInput(ns("coeffcalc"),"Coefficients:",0,1,c(0,1),sep=" ",ticks = FALSE),
      shiny::sliderInput(ns("benchmark"),"Benchmark:",0,1,c(0,1),sep=" ",ticks = FALSE),
      shiny::sliderInput(ns("plotswin"),"Plots:",0,1,c(0,1),sep=" ",ticks = FALSE),
      style="padding-top: 5px;
             padding-bottom: 0px;
             margin-top: 0px;
             margin-bottom: 0px;"
    ),
    shiny::mainPanel(
      width = 10,
      shiny::fluidRow(
        shiny::tags$style(type = "text/css", paste0(".",ns("mainoutwithtitle"),cssmainoutwithtitle,"\n",
                                                    ".",ns("mainoutwithouttitle"),cssmainoutwithouttitle)),
        shiny::column(11,
                      shiny::radioButtons(ns("mainout_choice"),NULL,
                                          choices = c("Scatter plot","In-sample predictions","Comparison benchmark/input",
                                                      "Revisions","Benchmark plot","Benchmark summary"),
                                          selected = "Benchmark plot",inline=TRUE)
        ),
        shiny::column(1,
                      shiny::actionButton(ns("infobtn"),label = NULL, icon = shiny::icon("circle-info"),
                                          class = "btn-success")),
        align="center"
      ),
      shiny::uiOutput(ns("mainOutput"))
    )
  )
}

reView_ui_tab3 <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(12,
                  shiny::fluidRow(
                    shiny::column(width=6,shiny::div("Before",class="section"),shiny::verbatimTextOutput(ns("oldcall"))),
                    shiny::column(width=6,shiny::div("After",class="section"),shiny::div(id=ns("tocopy"),shiny::verbatimTextOutput(ns("newcall"))))
                  ),
                  shiny::fluidRow(
                    shiny::tags$script(
                      paste0(
                        "Shiny.addCustomMessageHandler('",ns("closewindow"),"', closewindow );\n\n",
                        "function closewindow(anymessage) {window.close();}\n\n",
                        "Shiny.addCustomMessageHandler('",ns("copy"),"', copy );\n\n",
                        "function copy(text) {
                    navigator.clipboard.writeText(text).then(function() {
                      Shiny.onInputChange('",ns("Copymade"),"', 'TRUE');
                    }, function() {
                      Shiny.onInputChange('",ns("Copymade"),"', 'FALSE');
                    });
                  }")
                    ),
                    shiny::column(3,shiny::actionButton(ns("Reset"),"Reset",width = "100%",
                                                        class="btn-warning")),
                    shiny::column(3,shiny::actionButton(ns("Quit"),"Quit",width = "100%",
                                                        class="btn-danger")),
                    shiny::column(3,shiny::downloadButton(ns("Export"),"Export to RDS",style="width:100%;")),
                    shiny::column(3,shiny::actionButton(ns("Copy"), "Copy to clipboard",
                                                        width = "100%",class="btn-primary"))
                  ),
                  style = boxstyle
    ),style=lrmargins)
}

reView_ui_module <- function(id) {
  ns <- shiny::NS(id)
  shiny::navbarPage(title = shiny::textOutput(ns("titlenavbar")),
                    windowTitle = "reView",
                    id = ns("menu"),
                    selected = "Presets",
                    header = shiny::tags$style(".section { font-family: 'Source Sans Pro', sans-serif; font-weight: 420; line-height: 20px; text-align: center;}"),
                    shiny::tabPanel("Presets",
                                    reView_ui_tab1(ns("reViewtab1")),
                    ),
                    shiny::tabPanel("Modify",
                                    reView_ui_tab2(ns("reViewtab2"))
                    ),
                    shiny::tabPanel("Export",
                                    reView_ui_tab3(ns("reViewtab3"))
                    ))
}

reView_ui <- function() reView_ui_module("reView")

#### server ####

format_table_row <- function(object,digits,hide=integer(),signif.stars = FALSE,
                             background.format = NULL) {
  
  char_content <- format(round(object,digits),nsmall=digits,trim=TRUE)
  
  if (signif.stars) {
    char_content <- paste(symnum(object, corr = FALSE, na = FALSE, 
                                 cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                 symbols = c("***", "**", "*", 
                                             ".", " "),
                                 legend=FALSE),
                          char_content)
  }
  
  bckg.class <- vector("list",length(object))
  is.bold <- rep(FALSE,length(object))
  
  if (!is.null(background.format)) {
    switch(background.format,
           min.is.green = {
             bckg.class[which(object<=1.05*min(object))] <- "bg-success text-white"
             is.bold[which.min(object)] <- TRUE
           },
           success.is.red = bckg.class[object <= 0.05 & !is.na(object)] <- "bg-danger text-white",
           fail.is.red = bckg.class[object > 0.05 & !is.na(object)] <- "bg-danger text-white")
  }
  
  res <- Map(shiny::tags$td,
             char_content,
             class = bckg.class,
             style=paste(ifelse(is.bold,"font-weight: bold;",""),"text-align: right"))
  
  res[hide] <- rep(list(shiny::tags$td()),length(hide))
  
  shiny::HTML(do.call(paste,res))
}

link_if_in_shiny <- function(id,label,ns,...) {
  if (is.null(ns)) label
  else shiny::actionLink(ns(id),label,...)
}

summary_table_html <- function(presets_list,old_bn,distance_p,ns=NULL,selected_preset_tab2 = NULL, compare = NULL) {
  
  summ <- lapply(presets_list,summary)
  
  shiny::tags$table(
    do.call(shiny::tags$tr,
            c(list(shiny::tags$td(colspan = 2)),
              lapply(1:6,function(n) shiny::tags$th(link_if_in_shiny(paste0("model",n,"_actionlink"),paste("Model",n),ns,
                                                                     style = paste(
                                                                       if (isTRUE(selected_preset_tab2 == n)) "color: #FF0000;",
                                                                       if (isTRUE(get_preset(old_bn) == n) && compare) "font-style: italic;")),
                                                    style = "text-align:center")))
    ),
    shiny::tags$tr(shiny::tags$th("Distance",
                                  rowspan=if (is.null(old_bn)) 2 else 3),
                   shiny::tags$th("In-sample predictions (lf changes)",
                                  format_table_row(vapply(presets_list,
                                                          function(x) distance(in_sample(x,
                                                                                         type="changes"),
                                                                               distance_p),
                                                          0),
                                                   digits = 2L,background.format = "min.is.green"))),
    shiny::tags$tr(shiny::tags$th("Benchmark contributions (hf changes)",
                                  format_table_row(vapply(presets_list,
                                                          function(x) distance(in_disaggr(x,
                                                                                          type="contributions"),
                                                                               distance_p),
                                                          0),
                                                   digits=2L,background.format = "min.is.green"))),
    if (!is.null(old_bn)) {
      shiny::tags$tr(shiny::tags$th("Revisions (hf changes)",
                                    format_table_row(vapply(presets_list,
                                                            function(x) distance(in_revisions(x,
                                                                                              old_bn,
                                                                                              type="changes"),
                                                                                 distance_p),
                                                            0),
                                                     digits=2L,background.format = "min.is.green")))
    },
    shiny::tags$tr(shiny::tags$th("Portmanteau",rowspan=2),
                   shiny::tags$th("Statistic"),
                   format_table_row(vapply(summ,
                                           function(x) x$pm[if (x$rho == 0) "residuals"
                                                            else "residuals.decorrelated",
                                                            "statistic"],0),
                                    digits=2L)),
    shiny::tags$tr(shiny::tags$th("p-value"),
                   format_table_row(vapply(summ,function(x) x$pm[if (x$rho == 0) "residuals"
                                                                 else "residuals.decorrelated",
                                                                 "p.value"],0),
                                    digits=3L,signif.stars = TRUE,
                                    background.format = "success.is.red")),
    shiny::tags$tr(shiny::tags$th("Constant",rowspan=2),
                   shiny::tags$th("Value"),
                   format_table_row(vapply(summ,
                                           function(x) x$coefficients["constant","Estimate"],0),
                                    digits=2L,hide=c(2L,5L,6L))),
    shiny::tags$tr(shiny::tags$th("p-value"),
                   format_table_row(vapply(summ,
                                           function(x) x$coefficients["constant","p.value"],0),
                                    digits=3L,hide=c(2L,5L,6L),signif.stars = TRUE,
                                    background.format = "fail.is.red")),
    lapply(rownames(summ[[1]]$coefficients)[rownames(summ[[1]]$coefficients) != "constant"],function(n){
      shiny::tags$tr(
        shiny::tags$tr(shiny::tags$th(n,rowspan=2),
                       shiny::tags$th("Value"),
                       format_table_row(vapply(summ,
                                               function(x) x$coefficients[n,"Estimate"],0),
                                        digits=2L)),
        shiny::tags$tr(shiny::tags$th("p-value"),
                       format_table_row(vapply(summ,
                                               function(x) x$coefficients[n,"p.value"],0),
                                        digits=3L,signif.stars = TRUE,
                                        background.format = "fail.is.red"))
      )}
    ),
    shiny::tags$tr(shiny::tags$th("Rho",colspan=2),
                   format_table_row(vapply(summ,function(x) x$rho,0),
                                    digits=2L,hide=c(1L,2L,3L,5L))),
    width = "100%",border = 1
  )
}

reView_server_tab1_switch <- function(input,output,session,presets_list,old_bn,selected_preset_tab2,compare) {
  
  ns <- session$ns
  
  switch(input$firsttab_choice,
         "In-sample changes" = {
           shiny::fluidRow(
             shiny::column(6,
                           shiny::div(shiny::plotOutput(ns("model1_plot"),click=ns("model1_plotclick"),height = "33%"),
                                      shiny::plotOutput(ns("model3_plot"),click=ns("model3_plotclick"),height = "33%"),
                                      shiny::plotOutput(ns("model5_plot"),click=ns("model5_plotclick"),height = "33%"),class=ns("presetplot"))),
             shiny::column(6,
                           shiny::div(shiny::plotOutput(ns("model2_plot"),click=ns("model2_plotclick"),height = "33%"),
                                      shiny::plotOutput(ns("model4_plot"),click=ns("model4_plotclick"),height = "33%"),
                                      shiny::plotOutput(ns("model6_plot"),click=ns("model6_plotclick"),height = "33%"),class=ns("presetplot"))))
         },
         "Summary table" = {
           
           shiny::div(
             shiny::isolate(shiny::renderUI({ # for the table to be independent of the input distance
               
               summary_table_html(presets_list(),
                                  old_bn(),
                                  if (!shiny::isTruthy(input$distance_p)) 2L
                                  else switch(input$distance_p,Manhattan=1L,Euclidean=2L,Max=Inf),
                                  session$ns,
                                  selected_preset_tab2(),
                                  compare())
               
             })),
             do.call(shiny::tags$table,
                     c(
                       list(shiny::tags$tr(shiny::tags$th("Models",colspan=2),shiny::tags$th("Distance"))),
                       Map(shiny::tags$tr,
                           lapply(1:length(presets$label),shiny::tags$th," : "),
                           lapply(presets$label,shiny::tags$td,style='padding-left: 3px'),
                           c(
                             list(shiny::tags$td(shiny::radioButtons(ns("distance_p"),NULL,
                                                                     choices = c("Manhattan","Euclidean","Max"),
                                                                     selected = "Euclidean"),
                                                 rowspan=length(presets$label))),
                             rep(list(NULL),length(presets$label)-1L))),
                       list(style='margin-top: 20px')
                     ))
           )
         }
  )
}

reView_server_tab1 <- function(id,old_bn,new_bn_ext_setter,selected_preset_tab2,compare) {
  shiny::moduleServer(id,
                      function(input,output,session) {
                        
                        presets_list <- shiny::reactive({
                          m <- model.list(new_bn_ext_setter())
                          presets_list_fun(hfserie_alone(new_bn_ext_setter()),
                                           lfserie(new_bn_ext_setter()),
                                           start.coeff.calc=m$start.coeff.calc,
                                           end.coeff.calc=m$end.coeff.calc,
                                           start.benchmark=m$start.benchmark,
                                           end.benchmark=m$end.benchmark,
                                           start.domain=m$start.domain,
                                           end.domain=m$end.domain,
                                           outliers = outliers(new_bn_ext_setter()))
                        })
                        
                        output$firstTabOutput <- shiny::renderUI({
                          reView_server_tab1_switch(input,output,session,presets_list,old_bn,selected_preset_tab2,
                                                    compare)
                        })
                        
                        selected_preset_tab1 <- shiny::reactiveVal(NULL)
                        
                        lapply(1L:6L, function(n) {
                          output[[paste0("model",n,"_plot")]] <- shiny::renderPlot({
                            plot(in_sample(presets_list()[[n]]),
                                 main = paste0("Model ",n," (",presets$label[n],")"),
                                 col.main = if (isTRUE(selected_preset_tab2() == n)) "red"
                                   else "black",
                                 font.main = if (isTRUE(get_preset(old_bn()) == n) && compare()) 4
                                 else 2)
                          })
                        })
                        
                        lapply(1L:6L,function(type) {
                          shiny::observeEvent(
                            input[[paste0("model",type,"_plotclick")]],
                            {
                              selected_preset_tab1(NULL)
                              selected_preset_tab1(type)
                            })
                          shiny::observeEvent(
                            input[[paste0("model",type,"_actionlink")]],
                            {
                              selected_preset_tab1(NULL)
                              selected_preset_tab1(type)
                            },ignoreInit = TRUE)
                        })
                        selected_preset_tab1
                      })
}

reView_server_tab2_switch_impl <- function(benchmark,mainout_choice,plotswin,output,old_or_new,ns,old_bn = NULL) {
  
  switch(old_or_new,
         old={
           title <- shiny::div("Before",class="section")
           outputclass <- ns("mainoutwithtitle")
         },
         new={
           title <- shiny::div("After",class="section")
           outputclass <- ns("mainoutwithtitle")
         },
         mono={
           title <- NULL
           outputclass <- ns("mainoutwithouttitle")
         })
  
  # The old_bn arg is only for revisions
  switch(mainout_choice,
         "Benchmark plot" = {
           shiny::fluidRow(
             shiny::column(12,
                           title,
                           shiny::div(
                             plotOutBrushAndRender(benchmark,
                                                   plotswin,
                                                   output,
                                                   paste0(old_or_new,"plot"),
                                                   ns,height="100%"),class=outputclass)))
         },
         "Scatter plot" = {
           shiny::fluidRow(
             shiny::column(12,
                           title,
                           shiny::div(
                             plotOutBrushAndRender(shiny::reactive(in_scatter(benchmark())),
                                                   plotswin,
                                                   output,
                                                   paste0(old_or_new,"plot"),
                                                   ns,is.brush=FALSE,height="100%",
                                                   xlab = "High-frequency serie",
                                                   ylab = "Low-frequency serie"),class=outputclass)))
         },
         "In-sample predictions" = {
           shiny::fluidRow(
             shiny::column(12,
                           title,
                           shiny::div(
                             plotOutBrushAndRender(shiny::reactive(in_sample(benchmark(),type="levels")),
                                                   plotswin,
                                                   output,
                                                   paste0(old_or_new,"plotlev"),
                                                   ns,
                                                   height="50%",
                                                   ylab = "Levels"),
                             plotOutBrushAndRender(shiny::reactive(in_sample(benchmark(),type="changes")),
                                                   plotswin,
                                                   output,
                                                   paste0(old_or_new,"plotcha"),
                                                   ns,
                                                   height="50%",
                                                   ylab = "Changes"),class=outputclass)
             )
           )
         },
         "Benchmark summary" = {
           output_name <- paste0(old_or_new,"verbat")
           output[[output_name]] <- shiny::renderPrint(print(summary(benchmark()),call=FALSE))
           shiny::fluidRow(shiny::column(12,title,
                                         shiny::div(shiny::verbatimTextOutput(ns(output_name)),
                                                    class=outputclass)))
         },
         "Comparison benchmark/input" = {
           shiny::fluidRow(
             shiny::column(12,
                           title,
                           shiny::div(plotOutBrushAndRender(shiny::reactive(in_disaggr(benchmark(),
                                                                                       type="levels-rebased")),
                                                            plotswin,
                                                            output,
                                                            paste0(old_or_new,"plotlev"),
                                                            ns,
                                                            height="33%",
                                                            ylab = "Rebased levels"),
                                      plotOutBrushAndRender(shiny::reactive(in_disaggr(benchmark(),
                                                                                       type="changes")),
                                                            plotswin,
                                                            output,
                                                            paste0(old_or_new,"plotcha"),
                                                            ns,
                                                            height="33%",
                                                            ylab = "Changes"),
                                      plotOutBrushAndRender(shiny::reactive(in_disaggr(benchmark(),
                                                                                       type="contributions")),
                                                            plotswin,
                                                            output,
                                                            paste0(old_or_new,"plotctb"),
                                                            ns,
                                                            height="33%",
                                                            ylab = "Contributions"),class=outputclass)
             )
           )
         },
         "Revisions" = {
           shiny::fluidRow(
             shiny::column(12,
                           title,
                           shiny::div(plotOutBrushAndRender(shiny::reactive(in_revisions(benchmark(),old_bn(),
                                                                                         type="levels")),
                                                            plotswin,
                                                            output,
                                                            paste0(old_or_new,"plotlev"),
                                                            ns,
                                                            height= "33%",
                                                            ylab = "Levels"),
                                      plotOutBrushAndRender(shiny::reactive(in_revisions(benchmark(),old_bn(),
                                                                                         type="changes")),
                                                            plotswin,
                                                            output,
                                                            paste0(old_or_new,"plotcha"),
                                                            ns,
                                                            height="33%",
                                                            ylab = "Changes"),
                                      plotOutBrushAndRender(shiny::reactive(in_revisions(benchmark(),old_bn(),
                                                                                         type="contributions")),
                                                            plotswin,
                                                            output,
                                                            paste0(old_or_new,"plotctb"),
                                                            ns,
                                                            height="33%",
                                                            ylab = "Contributions"),class=outputclass)
             ))
         }
  )
}

reView_server_tab2_switch <- function(input,output,new_bn,old_bn,ns,compare) {
  
  plotswin <- shiny::reactive(c(input$plotswin[1L],
                                input$plotswin[2L]+
                                  deltat(lfserie(old_bn()))-
                                  deltat(hfserie_alone(old_bn()))))
  
  mainout_choice <- input$mainout_choice
  
  if (compare && !(mainout_choice %in% c("Comparison benchmark/input","Revisions"))) {
    shiny::fluidRow(
      shiny::column(6,shiny::fluidRow(shiny::column(12,reView_server_tab2_switch_impl(old_bn,mainout_choice,plotswin,output,"old",ns),
                                                    style=boxstyle),style=lrmargins)),
      shiny::column(6,shiny::fluidRow(shiny::column(12,
                                                    reView_server_tab2_switch_impl(new_bn,mainout_choice,plotswin,output,"new",ns),
                                                    style=boxstyle),style="margin-right: 6px"))
    )
  }
  else shiny::fluidRow(shiny::column(12,reView_server_tab2_switch_impl(new_bn,mainout_choice,plotswin,output,"mono",ns,old_bn),
                                     style=boxstyle),style=lrmargins)
}

reView_server_tab2 <- function(id,hfserie_name,lfserie_name,
                               old_bn,new_bn_external_setter,compare,
                               selected_preset_tab1,reset) {
  shiny::moduleServer(id,
                      function(input,output,session) {
                        
                        new_bn <- shiny::reactive(get_new_bn(input,hfserie_name,lfserie_name,new_bn_external_setter))
                        
                        shiny::exportTestValues(new_bn = new_bn())
                        
                        # Inputs initializers
                        
                        shiny::observeEvent(c(reset(),new_bn_external_setter(),old_bn()),
                                            set_inputs_to_default(session,new_bn_external_setter),
                                            priority = 2L)
                        
                        shiny::observeEvent(selected_preset_tab1(),set_preset(session,selected_preset_tab1),
                                            ignoreNULL = TRUE, priority = 2L)
                        
                        # Input modifiers
                        
                        shiny::observeEvent(input$brush,
                                            set_plots_window_with_brush(session,input,old_bn),
                                            ignoreNULL = TRUE, priority = 1L)
                        
                        shiny::observeEvent(input$click,
                                            shiny::updateSliderInput(session, "plotswin",value = get_maxwin(old_bn())),
                                            ignoreNULL = TRUE,priority = 1L)
                        
                        shiny::observeEvent(compare(),{
                          shiny::updateRadioButtons(session,"mainout_choice", NULL,
                                                    choices = c("Scatter plot","In-sample predictions","Comparison benchmark/input",
                                                                if (compare()) "Revisions","Benchmark plot","Benchmark summary"),
                                                    selected = "Scatter plot",
                                                    inline = TRUE)
                        },priority = 1L)
                        
                        # Outputs
                        
                        shiny::observeEvent(input$infobtn,
                                            info_dialog(session,input$mainout_choice))
                        
                        output$mainOutput <- shiny::renderUI({
                          reView_server_tab2_switch(input,output,
                                                    new_bn,old_bn,
                                                    session$ns,compare())
                        })
                        
                        new_bn})
}

reView_server_tab3 <- function(id,old_bn,new_bn,hfserie_name,lfserie_name,compare) {
  shiny::moduleServer(id,
                      function(input,output,session) {
                        new_call_text <- shiny::reactive(get_benchmark_call(new_bn(),hfserie_name(),lfserie_name()))
                        old_call_text <- shiny::reactive(if (compare()) get_benchmark_call(old_bn(),hfserie_name(),lfserie_name())
                                                         else "No model to compare.")
                        output$oldcall <- shiny::renderText(old_call_text())
                        output$newcall <- shiny::renderText(new_call_text())
                        
                        output$Export <- shiny::downloadHandler(
                          filename = paste0("benchmark-",deparse(hfserie_name()),"-",deparse(lfserie_name()),".rds"),
                          content = function(file) saveRDS(reViewOutput(new_bn(),old_bn(),compare()),file)
                        )
                        
                        session$onSessionEnded(function() {
                          if (Sys.getenv('SHINY_PORT') == "") shiny::isolate(shiny::stopApp(reViewOutput(new_bn(),old_bn(),compare())))
                        })
                        
                        shiny::observeEvent(input$Quit,{
                          session$sendCustomMessage(session$ns("closewindow"), "anymessage")
                          if (Sys.getenv('SHINY_PORT') == "") shiny::stopApp(reViewOutput(new_bn(),old_bn(),compare()))
                        })
                        
                        shiny::observeEvent(input$Copy,{
                          session$sendCustomMessage(session$ns("copy"), new_call_text())
                        })
                        
                        shiny::observeEvent(input$Copymade,{
                          shiny::showModal(
                            shiny::modalDialog(title = "reView",
                                               if (input$Copymade) "New model copied in the clipboard !"
                                               else "Sorry but the browser blocked the keyboard !",
                                               easyClose = TRUE,
                                               footer = NULL),
                            session)
                        })
                        
                        shiny::reactive(input$Reset)
                      })
}

reView_server_module <- function(id,old_bn,new_bn_external_setter,hfserie_name,lfserie_name,compare,
                                 disable_update_navbar) {
  shiny::moduleServer(id,function(input, output, session) {
    
    output$titlenavbar <- shiny::renderText(paste("reView:", deparse(hfserie_name()),"on", deparse(lfserie_name())))
    
    # tab 1 : Presets
    
    selected_preset_tab1 <- reView_server_tab1("reViewtab1",old_bn,new_bn_external_setter,selected_preset_tab2,
                                               compare)
    
    # tab 2 : Modify
    
    new_bn <- reView_server_tab2("reViewtab2",
                                 hfserie_name,lfserie_name,
                                 old_bn,new_bn_external_setter,compare,
                                 selected_preset_tab1,reset)
    selected_preset_tab2 <- shiny::reactive(get_preset(new_bn()))
    
    # tab3 : Export
    
    reset <- reView_server_tab3("reViewtab3",old_bn,new_bn,
                                hfserie_name,lfserie_name,
                                compare)
    
    shiny::observeEvent(c(selected_preset_tab1(),reset()),
                        if (!disable_update_navbar()) shiny::updateNavbarPage(session,"menu","Modify"),
                        ignoreInit = TRUE,
                        priority = 3L)
    
    new_bn
  })
}
reView_server <- function(old_bn,hfserie_name,lfserie_name,compare) {
  function(input,output,session) {
    reView_server_module("reView",
                         old_bn = shiny::reactive(old_bn),
                         new_bn_external_setter = shiny::reactive(old_bn),
                         hfserie_name = shiny::reactive(hfserie_name),
                         lfserie_name = shiny::reactive(lfserie_name),
                         compare = shiny::reactive(compare),
                         disable_update_navbar = shiny::reactive(FALSE))
  }
}

#### runner ####

#' @importFrom utils packageVersion
runapp_reView <- function(old_bn,hfserie_name,lfserie_name,compare) {
  if (!requireNamespace("shiny", quietly = TRUE) ||
      packageVersion("shiny") < 1.5) {
    stop("In order to use the reView shiny app, shiny >= 1.5.0 is needed.\n",
         "Please run install.packages(\"shiny\")",
         call. = FALSE)
  }
  shinyreturn <- shiny::runApp(
    shiny::shinyApp(ui = reView_ui(),
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
#' The app is made of \pkg{shiny} modules in order to make it easy to integrate
#' it into a wider application. In the module part, every input are defined as
#' reactive variables.
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
#' @seealso \link{rePort}
#'
#' @examples
#' \dontrun{
#' reView(twoStepsBenchmark(turnover,construction))
#' }
#' 
#' @export
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
                             compare = TRUE) {
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
         hfserie_name = hfserie_name %||% object$hfserie_name,
         lfserie_name = lfserie_name %||% object$lfserie_name,
         compare = compare)
}

warning_reviewoutput <- warning_news_factory("The order of the reViewOutput object produced by reView has been reversed. See NEWS. This warning is displayed once in each R session.")

#' @export
reView.twoStepsBenchmark <- function(object,
                                     hfserie_name = NULL,
                                     lfserie_name = NULL,
                                     compare = TRUE) {
  if (NCOL(neither_outlier_nor_constant(object)) > 1) stop("This reviewing application is only for univariate benchmarks.", call. = FALSE)
  warning_reviewoutput()
  runapp_reView(object,
                reViewName(hfserie_name %||% object$call$hfserie),
                reViewName(lfserie_name %||% object$call$lfserie),
                compare=compare)
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
#' @param launch.browser `TRUE` or `FALSE`. If TRUE, the output is opened in the
#' browser. Defaults to TRUE if output_file is NULL.
#' @param hfserie_name a character of length 1. The name of the hfserie.
#' @param lfserie_name a character of length 1. The name of the lfserie.
#' @param \dots other arguments passed to rmarkdown::render
#' 
#' @seealso reView
#' 
#' @export
rePort <- function(object, output_file = NULL,
                   launch.browser = if (is.null(output_file)) TRUE else FALSE,
                   hfserie_name = NULL,
                   lfserie_name = NULL,
                   ...) UseMethod("rePort")

#' @export
rePort.character <- function(object, output_file = NULL,
                             launch.browser = if (is.null(output_file)) TRUE else FALSE,
                             hfserie_name = NULL,
                             lfserie_name = NULL,
                             ...)
  rePort(readRDS(object), output_file, launch.browser, hfserie_name, lfserie_name,...)

#' @export
rePort.connection <- function(object,
                              output_file = NULL,
                              launch.browser = if (is.null(output_file)) TRUE else FALSE,
                              hfserie_name = NULL,
                              lfserie_name = NULL,
                              ...)
  rePort(readRDS(object), output_file, launch.browser, hfserie_name, lfserie_name, ...)

#' @export
rePort.twoStepsBenchmark <- function(object, output_file = NULL,
                                     launch.browser = if (is.null(output_file)) TRUE else FALSE,
                                     hfserie_name = NULL,
                                     lfserie_name = NULL,
                                     ...) {
  if (NCOL(neither_outlier_nor_constant(object)) > 1) stop("This reporting function is only for univariate benchmarks.", call. = FALSE)
  rePort(reViewOutput(object,
                      benchmark_old=NULL,
                      compare=FALSE),
         output_file,launch.browser,
         hfserie_name=hfserie_name,
         lfserie_name=lfserie_name,
         ...)
}

#' @importFrom utils browseURL packageVersion
#' @export
rePort.reViewOutput <- function(object, output_file = NULL,
                                launch.browser = if (is.null(output_file)) TRUE else FALSE,
                                hfserie_name = NULL,
                                lfserie_name = NULL,
                                ...) {
  if (!requireNamespace("rmarkdown", quietly = TRUE) ||
      packageVersion("rmarkdown") < 2.0) {
    stop("In order to use the rePort function, rmarkdown >= 2.0.0 is needed.\n",
         "Please run install.packages(\"rmarkdown\")",
         call. = FALSE)
  }
  temp_dir <- tempdir()
  temp_rmd <- file.path(temp_dir, "report.Rmd")
  temp_html <- tempfile("report",temp_dir,".html")
  
  if (object$compare) file.copy(system.file("rmd/report.Rmd", package = "disaggR"), temp_rmd, overwrite = TRUE)
  else file.copy(system.file("rmd/report_nocompare.Rmd", package = "disaggR"), temp_rmd, overwrite = TRUE)
  rmarkdown::render(temp_rmd,output_file=temp_html,
                    params = list(new_bn=object$benchmark,
                                  old_bn=object$benchmark_old,
                                  hfserie_name=reViewName(hfserie_name %||% object$hfserie_name),
                                  lfserie_name=reViewName(lfserie_name %||% object$lfserie_name)),
                    envir = new.env(parent = globalenv()),
                    output_format = rmarkdown::html_document(css=system.file("rmd/report.css", package = "disaggR"),
                                                             theme=NULL),
                    quiet = TRUE,
                    ...)
  if (is.null(output_file))  {
    if (launch.browser) browseURL(temp_html)
    invisible(temp_html)
  }
  else {
    file.copy(temp_html, output_file, overwrite = TRUE)
    if (launch.browser) browseURL(output_file)
    invisible(output_file)
  }
}

#' @export
print.reViewOutput <- function(x,
                               hfserie_name = NULL,
                               lfserie_name = NULL,
                               ...) rePort(x,
                                           output_file=NULL,
                                           hfserie_name=hfserie_name,
                                           lfserie_name=lfserie_name,
                                           ...)
