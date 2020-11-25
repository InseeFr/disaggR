type_label <- function(object) {
  switch(attr(object,"type"),
         levels="Levels",
         `levels-rebased`="Rebased levels",
         changes="Changes",
         contributions="Contributions"
  )
}

#' Default color palette
#' 
#' The default color palette for the graphics, imported from the package.
#' \pkg{scales}. The chosen palette function depends on the input object.
#' 
#' @keywords internal
#' @export
#' @importFrom scales brewer_pal div_gradient_pal
default_col_pal <- function(object) {
  if (identical(attr(object,"type"),"contributions")) brewer_pal(type = "qual",palette = 7L)
  else if (identical(attr(object,"func"),"in_scatter")) function(n) div_gradient_pal()(seq(0, 1, length.out = n))
  else brewer_pal(type = "qual",palette = 6L)
}

#' Default color palette
#' 
#' The default palette for the graphics, imported from the package
#' \pkg{scales}.
#' 
#' @keywords internal
#' @export
#' @importFrom scales linetype_pal
default_lty_pal <- function() linetype_pal()

default_margins <- function(main, xlab, ylab) {
  c(
    if (is.null(xlab)) 1 else 2,
    if (is.null(ylab)) 1.3 else 2.3,
    if (is.null(main)) 0 else 1,
    0
  )
}

#### Base plots

plot_init <- function(xmin,xmax,ymin,ymax,xlab,ylab,
                      extend.x,extend.y,abline.x,
                      main, ...) {
  
  if (is.null(xlab)) xlab <- ""
  if (is.null(ylab)) ylab <- ""
  
  sizey <- ymax-ymin
  plot(x = c(xmin,xmax), y = c(ymin,ymax),
       xlim = c(xmin,xmax) + if (extend.x) 0.02 * (xmax-xmin) * c(-1,1) else c(0,0),
       ylim = c(ymin,ymax) + if (extend.y) 0.02 * (ymax-ymin) * c(-1,1) else c(0,0),
       type = "n",
       xaxs = "i", xaxt = "n",
       yaxs = "i", yaxt = "n",
       cex.main = 0.8,
       main = main, ...)
  
  title(xlab = xlab, line= 0.8, cex.lab=0.8)
  
  title(ylab = ylab, line= 1.3, cex.lab=0.8)
  
  if (abline.x) {
    grid(nx = NA,ny=NULL,col = "grey")
    abline(v = (floor(xmin)+1L):(ceiling(xmax)-1L),lty="dotted",lwd=1,col="grey")   
  }
  else grid(nx = NULL,ny=NULL,col = "grey")
}

plot_init_x <- function(x, xlab, ylab, main, ...) {
  tspx <- tsp(x)
  plot_init(xmin = tspx[1L],xmax = tspx[2L]+deltat(x),
            # That x window is set to be able to translate x values
            # of deltat(x)/2 on the right
            ymin = min(x,na.rm = TRUE), ymax = max(x,na.rm = TRUE),
            xlab = xlab, ylab = ylab,
            extend.x = FALSE, extend.y = TRUE, abline.x = TRUE,
            main = main, ...)
}

barplot_mts <- function (height,xlab,ylab,col,main, ...) {
  
  timeh <- time(height)
  
  pser <- height * (height > 0)
  nser <- height * (height < 0)
  ppser <- ts_from_tsp(rowSums(pser),tsp(pser))
  nnser <- ts_from_tsp(rowSums(nser),tsp(nser))
  
  tsph <- tsp(height)
  
  plot_init_x(cbind(nnser,ppser),xlab = xlab, ylab = ylab, main = main, ...)
  
  d <- deltat(height)
  
  cc <- cycle(pser)
  xleft <- floor(timeh) + (cc - 1) * d
  xright <- xleft + d
  
  for (i in 1L:ncol(height)) {
    rect(xleft = xleft, xright = xright,
         ybottom = nnser, ytop = ppser,
         col = col[i],border=FALSE)
    nnser <- nnser - nser[, i]
    ppser <- ppser - pser[, i]
  }
  
  invisible()
}

arrows_heads <- function(x0,y0,x1,y1,col) {
  x0i <- grconvertX(x0, from = "user", to = "inches")
  y0i <- grconvertY(y0, from = "user", to = "inches")
  x1i <- grconvertX(x1, from = "user", to = "inches")
  y1i <- grconvertY(y1, from = "user", to = "inches")
  
  ratios <- 0.1/sqrt((x1i-x0i)^2+(y1i-y0i)^2)
  proportions <- tan(15/180*pi)
  
  xlefts   <- grconvertX(x1i + ratios * ((x0i-x1i)-(y0i-y1i) * proportions), from = "inches", to = "user")
  xrights  <- grconvertX(x1i + ratios * ((x0i-x1i)+(y0i-y1i) * proportions), from = "inches", to = "user")
  
  ylefts   <- grconvertY(y1i + ratios * ((y0i-y1i)+(x0i-x1i) * proportions), from = "inches", to = "user")
  yrights  <- grconvertY(y1i + ratios * ((y0i-y1i)-(x0i-x1i) * proportions), from = "inches", to = "user")
  
  # It's okay if the distance is zero because then these points are NaN
  # And 2 NaN in a triangle doesn't draw anything
  
  Map(polygon,
      x=Map(c,x1,xlefts,xrights),
      y=Map(c,y1,ylefts,yrights),
      col=col,
      border=col)
  
  invisible()
}

break_arrows <- function(arrows_time) {
  res <- pretty(arrows_time,n=4,eps.correct = 2)
  res <- res[-c(1L,length(res))]
  
  res <- c(if (arrows_time[1L] != res[1L]) arrows_time[1L],
           res,
           if (arrows_time[length(arrows_time)] != res[length(res)]) arrows_time[length(arrows_time)])
  
  res
}

scatterplot_ts <- function(x,col) {
  n <- nrow(x)
  x0 <- x[-n,2L]
  y0 <- x[-n,1L]
  x1 <- x[-1L,2L]
  y1 <- x[-1L,1L]
  segments(x0,y0,x1,y1,col = col)
  arrows_heads(x0,y0,x1,y1,col = col)
}

draw_axes <- function(timex) {
  
  axis(side = 2L, labels=NA, tick = TRUE)
  axis(side = 2L, tick = FALSE, line=-0.5, cex.axis=0.7)
  
  if (is.null(timex)) {
    axis(side = 1L, labels=NA, tick = TRUE, tck=-0.011)
    axis(side = 1L, tick = FALSE, line=-1.1, cex.axis=0.7)
  }
  else {
    year <- floor(timex)
    axis(side = 1L, at = c(year,year[length(year)]+1L), labels = NA, tick = TRUE)
    axis(side = 1L, at = year + 0.5, labels = year, tick = FALSE, line = -1.1, cex.axis=0.7)
    
  }
}

window_default <- function(x,start,end) {
  timex <- time(x)
  
  non_na_vals <- {
    if (NCOL(x) == 1L) !is.na(x)
    else apply(x,1L,function(x) !all(is.na(x)))
  }
  
  start <- if (is.null(start)) floor(min(timex[non_na_vals])) else start
  end <- if (is.null(end)) floor(max(timex[non_na_vals])) + 1 - deltat(x) else end
  
  window(x,start=start,end=end,extend=TRUE)
}

eval_function_if_it_is_one <- function(f,arg) if (is.function(f)) f(arg) else f

plotts <-function(x,show.legend,col,lty,
                  series_names,type="line",
                  start,end,
                  xlab,ylab, main,
                  ...) {
  
  coefficients <- attr(x,"coefficients") # Only for the scatter plot
  
  x <- window_default(x,start,end)
  
  timex_win <- as.vector(time(x)) + deltat(x)/2
  
  col <- eval_function_if_it_is_one(col,if (type == "scatter") nrow(x)-1L else NCOL(x))
  lty <- eval_function_if_it_is_one(lty,NCOL(x))
  
  switch(type,
         line = {
           plot_init_x(x,xlab = xlab, ylab = ylab, main = main, ...)
           
           if (NCOL(x) == 1L) lines.default(x = timex_win, y = x,
                                            col = col, lty = lty)
           else Map(
             graphics::lines.default,
             x = list(timex_win),
             y = lapply(1:NCOL(x),function(i) x[,i]),
             # can be replaced by apply(x,2,identity,simplify = FALSE) in r 4.1
             col = col,
             lty = lty
           )
           
           if (show.legend) legend("bottomleft",legend=series_names,
                                   col=col,lty=lty,horiz=TRUE,bty="n",cex=0.8)
         },
         bar = {
           barplot_mts(x, col=col, xlab = xlab, ylab = ylab, main = main, ...)
           # barplot are initialized in the subfunction because their height
           # is unknown before having separated the positive and negative part
           lines(x = timex_win, y = as.vector(rowSums(x)), lty = lty)
           
           if (show.legend) legend("bottomleft",legend=series_names,
                                   fill=col,horiz=TRUE,bty="n",cex=0.8)
         },
         segment = {
           plot_init_x(x,xlab = xlab, ylab = ylab, main = main, ...)
           
           lines.default(x = timex_win, y = x,
                         col = col,type = "h")
           
           if (show.legend) legend("bottomleft", legend = series_names,
                                   col=col,lty="solid",horiz=TRUE,bty="n",cex=0.8)
         },
         scatter = {
           plot_init(xmin = min(x[,2L], na.rm = TRUE),
                     xmax = max(x[,2L], na.rm = TRUE),
                     ymin = min(x[,1L],na.rm = TRUE),
                     ymax = max(x[,1L],na.rm = TRUE),
                     xlab = xlab, ylab = ylab,
                     extend.x = TRUE, extend.y = TRUE,
                     abline.x=FALSE, main = main, ...)
           
           abline(a = coefficients["constant"],
                  b = coefficients[names(coefficients) != "constant"],
                  col = "red",
                  lwd = 2)
           
           scatterplot_ts(x, col=col)
           
           if (show.legend) {
             arrows_time <- (timex_win + deltat(x)/2)
             arrows_time <- arrows_time[-length(arrows_time)]
             
             selected_time <- break_arrows(arrows_time)
             
             i <- round((selected_time-arrows_time[1L])*frequency(x))+1
             
             legend("bottom", legend = round(selected_time,2),
                    fill = col[i], horiz=TRUE,bty="n",cex=0.8)
           } 
         }
  )
  invisible()
}

#' @export
plot.twoStepsBenchmark <- function(x, xlab = NULL, ylab = NULL,
                                   start = NULL, end = NULL,
                                   col = default_col_pal(x),
                                   lty = default_lty_pal(),
                                   show.legend = TRUE,
                                   main=NULL,
                                   mar = default_margins(main, xlab, ylab),
                                   ...) {
  
  mar_save <- par("mar")
  on.exit(par(mar=mar_save))
  par(mar=mar)
  
  model <- model.list(x)
  x <- as.ts(x)
  
  col <- eval_function_if_it_is_one(col,2L)
  lty <- eval_function_if_it_is_one(lty,2L)
  
  plotts(x = x,show.legend = FALSE,
         col = col[1L],lty = lty[1L],
         series_names = "Benchmark",type = "line",
         start = start, end = end,
         xlab = xlab, ylab = ylab, main = main,
         ...)
  
  Map(
    function(time,value) lines(ts_expand(ts(value,
                                            start=time,
                                            frequency = frequency(model$lfserie)),
                                         nfrequency=frequency(model$hfserie)),
                               col = col[2L],
                               lty = lty[2L]),
    time(model$lfserie)+deltat(model$hfserie)/2,
    model$lfserie
  )
  
  if (show.legend) legend("bottomleft",legend=c("Benchmark", "Low-Frequency serie"),
                          col=col,lty=lty,horiz=TRUE,bty="n",cex=0.8)
  
  draw_axes(time(x))
  
  invisible()
}

#' @export
plot.tscomparison <- function(x, xlab = NULL, ylab = NULL, start = NULL, end = NULL,
                              col = default_col_pal(x),
                              lty = default_lty_pal(),
                              show.legend = TRUE,
                              main = NULL,
                              mar = default_margins(main, xlab, ylab),
                              ...) {
  
  mar_save <- par("mar")
  on.exit(par(mar=mar_save))
  par(mar=mar)
  
  type_label <- type_label(x)
  
  if (type_label == "Contributions") plotts(x = x, show.legend = show.legend,
                                            col = col, lty = lty,
                                            series_names = colnames(x),type = "bar",
                                            start = start, end = end,
                                            xlab = xlab, ylab = ylab, main = main,
                                            ...)
  else {
    switch(attr(x,"func"),
           in_revisions = plotts(x = x, show.legend = show.legend,
                                 col = col, lty = lty,
                                 series_names = colnames(x),type = "segment",
                                 start = start, end = end,
                                 xlab = xlab, ylab = ylab, main = main,
                                 ...),
           in_scatter = plotts(x = x, show.legend = show.legend,
                               col = col, lty = lty,
                               series_names = colnames(x),type = "scatter",
                               start = start, end = end,
                               xlab = xlab, ylab = ylab, main = main,
                               ...),
           plotts(x = x, show.legend = show.legend,
                  col = col, lty = lty,
                  series_names = colnames(x), type = "line",
                  start = start, end = end, 
                  xlab = xlab, ylab = ylab, main = main,
                  ...)
    )
  }
  
  draw_axes(if (identical(attr(x,"func"),"in_scatter")) NULL else time(x))
  
  invisible()
}

#### ggplot2 plots

#' Default ggplot theme
#' 
#' This is the default theme for the ggplot graphics produced with autoplot
#' 
#' @keywords internal
#' @export
default_theme_ggplot <- function(show.legend,xlab,ylab,mar) {
  classic <- theme_classic()
  classic %+replace%
    theme(axis.title.x = if (is.null(xlab)) element_blank() else classic$axis.title.x,
          axis.title.y = if (is.null(ylab)) element_blank() else classic$axis.title.y,
          plot.margin =  if (is.null(mar)) classic$plot.margin else margin(mar[3L],mar[4L],
                                                                           mar[1L],mar[2L],
                                                                           unit="pt"),
          panel.grid.major = element_line(colour = "#cccccc"),
          legend.position = if (show.legend) "bottom" else "none"
    )
}

gglims <- function(object) c(tsp(object)[1L],tsp(object)[2L] + deltat(object))

dftsforggplot <- function(object,series_names) {
  data.frame(
    Date = as.numeric(time(object)+deltat(object)/2),
    Values = as.numeric(object),
    Variables = factor(do.call(c,lapply(series_names,rep.int,times=NROW(object))),
                       levels=series_names)
  )
}

ggplotts <- function(object,show.legend, series_names,theme,type,
                     start, end, xlab,ylab,...) {
  
  object <- window_default(object,start = start, end = end)
  
  df <- dftsforggplot(object,series_names)
  
  df <- df[!is.na(df$Values),]
  
  lims <- gglims(object)
  
  # That x window is set to be able to translate x values
  # of deltat(x)/2 on the right like for base plot init
  g <- ggplot(df,aes(x=Date,y=Values),show.legend = show.legend,...) +
    xlab(xlab) + ylab(ylab)
  switch(type,
         line = g + geom_line(aes(colour=Variables,linetype=Variables,group=Variables)),
         bar = g + geom_bar(aes(fill=Variables,group=Variables),stat="identity") +
           stat_summary(fun = sum, geom="line", colour = "black",
                        size = 0.5, alpha=1,na.rm = TRUE),
         segment = g + geom_segment(aes(xend=Date,colour=Variables,group=Variables),
                                    yend=0)
  ) +
    scale_x_continuous(
      limits = lims,
      breaks = (floor(lims[1L])+1L):(ceiling(lims[2L])-1L),
      minor_breaks = numeric(),
      expand=c(0,0)
    ) + 
    theme
}

ggscatter <- function(object,show.legend, theme, start, end, xlab,ylab, col, ...) {
  coefficients <- attr(object,"coefficients")
  
  object <- window_default(object,start = start, end = end)
  
  lims <- gglims(object)
  
  df <- data.frame(Time=as.numeric(time(object)),
                   object,check.names = FALSE)
  ggplot(df,aes(x=`High-frequency serie`,y=`Low-frequency serie`),
         show.legend = show.legend, ...) + xlab(xlab) + ylab(ylab) +
    geom_abline(intercept = coefficients["constant"],
                slope = coefficients[names(coefficients) != "constant"],
                linetype = "solid", colour = "red",size = 1) +
    geom_path(aes(colour=Time,group=1),arrow=arrow(angle = 15,
                                                   ends = "last",
                                                   type = "closed",
                                                   length = unit(0.1,"inches")),
              na.rm = TRUE) +
    continuous_scale("colour","gradient",function(x) col(length(x)),
                     limits = lims,
                     breaks = break_arrows(df$Time[-1]),
                     minor_breaks = numeric(),
                     expand=c(0,0)) +
    theme +
    guides(colour=guide_legend(override.aes = list(arrow = NULL))) 
}

function_if_it_isnt_one <- function(f) {
  if (is.function(f)) f else {
    if (length(f) == 1L) eval(bquote(function(n) rep(.(f),n)))
    else eval(bquote(function(n) .(f)[1L:n]))
  }
}

#' @export
ggplot2::autoplot

#' @export 
autoplot.twoStepsBenchmark <- function(object, xlab = NULL, ylab = NULL,
                                       start=NULL,end=NULL,
                                       col = default_col_pal(object),
                                       lty = default_lty_pal(),
                                       show.legend = TRUE,
                                       main = NULL,
                                       mar = NULL,
                                       theme = default_theme_ggplot(show.legend,
                                                                    xlab, ylab,
                                                                    mar),
                                       ...) {
  model <- model.list(object)
  
  col <- function_if_it_isnt_one(col)
  lty <- function_if_it_isnt_one(lty)
  
  lfdf <- dftsforggplot(ts_expand(model$lfserie,nfrequency = frequency(model$hfserie)),
                        series_names = "Low-Frequency serie")
  lfdf[,"Low-Frequency Periods"] <- rep(time(model$lfserie) + deltat(model$hfserie),
                                        each=frequency(model$hfserie)/frequency(model$lfserie))
  
  ggplotts(object = as.ts(object),show.legend = show.legend,
           series_names = "Benchmark", theme = theme, type = "line",
           start = start, end = end,
           xlab = xlab, ylab = ylab, ...) +
    geom_line(aes(x=Date,y=Values,colour=Variables,linetype=Variables,
                  group=`Low-Frequency Periods`),lfdf,
              na.rm = TRUE) +
    discrete_scale("colour","hue",col,na.translate = FALSE) +
    discrete_scale("linetype","hue",lty,na.translate = FALSE) +
    ggtitle(main)
}

#' @export 
autoplot.tscomparison <- function(object, xlab = NULL, ylab = NULL,
                                  start=NULL,end=NULL,
                                  col = default_col_pal(object),
                                  lty = default_lty_pal(),
                                  show.legend = TRUE,
                                  main = NULL,
                                  mar = NULL,
                                  theme = default_theme_ggplot(show.legend,
                                                               xlab, ylab,mar),
                                  ...) {
  
  col <- function_if_it_isnt_one(col)
  lty <- function_if_it_isnt_one(lty)
  
  type_label <- type_label(object)
  
  if (type_label == "Contributions") {
    ggplotts(object, show.legend = show.legend,
             type = "bar", series_names = colnames(object), theme = theme,
             start = start, end = end,
             xlab = xlab, ylab = ylab, ...) +
      labs(fill=type_label) +
      discrete_scale("fill","hue",col,na.translate = FALSE) +
      ggtitle(main)
  }
  else switch(attr(object,"func"),
              in_revisions = {
                ggplotts(object = object, show.legend = show.legend,
                         type="segment", series_names = colnames(object),theme = theme,
                         start = start, end = end,
                         xlab = xlab, ylab = ylab, ...) +
                  labs(colour=type_label) +
                  discrete_scale("colour","hue",col,na.translate = FALSE) +
                  ggtitle(main)
              },
              in_scatter = {
                ggscatter(object = object, show.legend = show.legend,
                          theme = theme, start = start, end = end,
                          xlab = xlab, ylab = ylab,
                          col = col,...) +
                  ggtitle(main)
              },
              ggplotts(object = object, show.legend = show.legend,
                       type="line",series_names = colnames(object), theme = theme,
                       start = start, end = end,
                       xlab = xlab, ylab = ylab, ...) +
                labs(linetype=type_label) +
                labs(colour=type_label) +
                discrete_scale("colour","hue",col,na.translate = FALSE) +
                discrete_scale("linetype", "linetype_d", lty,na.translate = FALSE) +
                ggtitle(main)
  )
}