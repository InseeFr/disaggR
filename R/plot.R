type_label <- function(object) {
  switch(attr(object,"type"),
         levels="Levels",
         `levels-rebased`="Rebased levels",
         changes="Changes",
         contributions="Contributions",
         differences="Differences"
  )
}

#' Default color palette
#' 
#' The default color palette for the graphics, imported from the package
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
#' The default palette for the graphics. `linetype_pal` is imported from the
#' package \pkg{scales}.
#' 
#' @keywords internal
#' @export
#' @importFrom scales linetype_pal
default_lty_pal <- function(object) {
  if (identical(attr(object,"func"),"in_scatter")) seq_len
  else linetype_pal()
}

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
  pser <- height * (height > 0)
  nser <- height * (height < 0)
  ppser <- ts_from_tsp(rowSums(pser),tsp(pser))
  nnser <- ts_from_tsp(rowSums(nser),tsp(nser))
  
  plot_init_x(cbind(nnser,ppser),xlab = xlab, ylab = ylab, main = main, ...)
  
  xleft <- time(height)
  xright <- xleft + deltat(height)
  
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
  
  # It's okay if the distance is zero because then left and right are NaN
  # And 2 NaN points in a triangle doesn't draw anything
  
  Map(polygon,
      x=Map(c,x1,xlefts,xrights),
      y=Map(c,y1,ylefts,yrights),
      col=col,
      border=col)
  
  invisible()
}

break_arrows <- function(arrows_time) {
  res <- pretty(arrows_time,n=4,eps.correct = 2)
  res <- c(arrows_time[1L],
           res[-c(1L,length(res))],
           arrows_time[length(arrows_time)])
  
  freq <- 1/(arrows_time[2L]-arrows_time[1L])
  res <- unique(round(res*freq)/freq)
  
  res
}

scatterplot_ts <- function(x,col,lty) {
  n <- nrow(x)
  x0 <- x[-n,2L]
  y0 <- x[-n,1L]
  x1 <- x[-1L,2L]
  y1 <- x[-1L,1L]
  segments(x0,y0,x1,y1,col = col, lty = lty)
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
  
  res <- window(x,start=start,end=end,extend=TRUE)
  
  attr(res,"coefficients") <- attr(x,"coefficients")
  attr(res,"func") <- attr(x,"func")
  
  res
}

eval_function_if_it_is_one <- function(f,arg) if (is.function(f)) f(arg) else f

plotts <- function(x,show.legend,col,lty,
                   series_names,type="line",
                   start,end,
                   xlab,ylab, main,
                   ...) {
  
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
           plot_init(xmin = min(x[,-1L], na.rm = TRUE),
                     xmax = max(x[,-1L], na.rm = TRUE),
                     ymin = min(x[,1L],na.rm = TRUE),
                     ymax = max(x[,1L],na.rm = TRUE),
                     xlab = xlab, ylab = ylab,
                     extend.x = TRUE, extend.y = TRUE,
                     abline.x=FALSE, main = main, ...)
           
           if (!is.null(attr(x,"coefficients"))) {
             abline(a = attr(x,"coefficients")["constant"],
                    b = attr(x,"coefficients")[names(attr(x,"coefficients")) != "constant"],
                    col = "red",
                    lwd = 2)
           }
           
           scatterplot_ts(x[,c(1L,2L)], col=col, lty = lty[1L])
           
           if (ncol(x) == 3L) {
             is_value_reg <- which(!is.na(x[,2L]))
             
             if (is_value_reg[1L] != 1L) {
               x_temp <- x[,c(1L,3L)]
               x_temp[(is_value_reg[1L] + 1L):nrow(x),2L] <- NA
               scatterplot_ts(x_temp,col = col, lty = lty[2L])
             }
             
             if (is_value_reg[length(is_value_reg)] != nrow(x)) {
               x_temp <- x[,c(1L,3L)]
               x_temp[1L:(is_value_reg[length(is_value_reg)]-1L),2L] <- NA
               scatterplot_ts(x_temp,col = col, lty = lty[2L])
             }
             
           }
           
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
  
  draw_axes(if (identical(attr(x,"func"),"in_scatter")) NULL else timex_win)
  
  invisible()
}

plot_with_lf <- function(x, xlab, ylab,
                         start, end,
                         col, lty,
                         show.legend, main,
                         mar, serie_name,
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
         series_names = serie_name,type = "line",
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
  
  if (show.legend) legend("bottomleft",legend=c(serie_name, "Low-frequency serie"),
                          col=col,lty=lty,horiz=TRUE,bty="n",cex=0.8)
  
  invisible()
}

#' @export
#' @rdname plot.tscomparison
plot.twoStepsBenchmark <- function(x, xlab = NULL, ylab = NULL,
                                   start = NULL, end = NULL,
                                   col = default_col_pal(x),
                                   lty = default_lty_pal(x),
                                   show.legend = TRUE,
                                   main=NULL,
                                   mar = default_margins(main, xlab, ylab),
                                   ...) {
  plot_with_lf(x,xlab,ylab,
               start,end,col,lty,
               show.legend,main,
               mar,"Benchmark",...)
}

#' @export
#' @rdname plot.tscomparison
plot.threeRuleSmooth <- function(x, xlab = NULL, ylab = NULL,
                                 start = NULL, end = NULL,
                                 col = default_col_pal(x),
                                 lty = default_lty_pal(x),
                                 show.legend = TRUE,
                                 main=NULL,
                                 mar = default_margins(main, xlab, ylab),
                                 ...) {
  plot_with_lf(x,xlab,ylab,
               start,end,col,lty,
               show.legend,main,
               mar,"Smooth",...)
}

#' @title Plotting disaggR objects
#' 
#' @description 
#' Plot methods for objects of class `"tscomparison"`, \link{threeRuleSmooth}
#' and \link{twoStepsBenchmark}. :
#' 
#' * \code{plot} draws a plot with base graphics
#' * \code{autoplot} produces a ggplot object
#' 
#' Objects of class `tscomparison` can be produced with the functions
#' \link{in_sample}, \link{in_scatter}, \link{in_revisions}, \link{in_disaggr}.
#' 
#' @param x (for the plot method) a tscomparison, a twoStepsBenchmark or a
#' threeRuleSmooth.
#' @param object (for the autoplot method) a tscomparison, a twoStepsBenchmark
#' or a threeRuleSmooth.
#' @param xlab the title for the x axis
#' @param ylab the title for the y axis
#' @param start a numeric of length 1 or 2. The start of the plot.
#' @param end a numeric of length 1 or 2. The end of the plot.
#' @param col the color scale applied on the plot. Could be a vector of colors,
#' or a function from n to colors.
#' @param lty the linetype scales applied on the plot. Could be a vector of
#' linetypes, or a function from n to linetypes.
#' @param show.legend `TRUE` or `FALSE`. Should an automatic legend be added to
#' the plot.
#' @param main a character of length 1, the title of the plot
#' @param mar a numeric of length 4, the margins of the plot specified in the
#' form `c(bottom, left, top, right)`.
#' @param theme a ggplot theme object to replace the default one (only for
#' autoplot methods)
#' @param ... other arguments passed either to ggplot or plot
#' @return `NULL` for the plot methods, the ggplot object for the autoplot
#' methods
#' @examples
#' benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
#' plot(benchmark)
#' plot(in_sample(benchmark))
#' autoplot(in_disaggr(benchmark,type="changes"),start=c(2015,1),end=c(2020,12))
#' plot(in_scatter(benchmark),xlab="title x",ylab="title y")
#' @export
plot.tscomparison <- function(x, xlab = NULL, ylab = NULL, start = NULL, end = NULL,
                              col = default_col_pal(x),
                              lty = default_lty_pal(x),
                              show.legend = TRUE,
                              main = NULL,
                              mar = default_margins(main, xlab, ylab),
                              ...) {
  
  mar_save <- par("mar")
  on.exit(par(mar=mar_save))
  par(mar=mar)
  
  type_label <- type_label(x)
  
  if (type_label == "Contributions") {
    if (all(x[,"Trend"] == 0,na.rm = TRUE)) {
      force(col);force(lty)
      x <- x[,colnames(x) != "Trend", drop = FALSE]
    }
    plotts(x = x, show.legend = show.legend,
           col = col, lty = lty,
           series_names = colnames(x),type = "bar",
           start = start, end = end,
           xlab = xlab, ylab = ylab, main = main,
           ...)
  }
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
                       levels=series_names,
                       ordered = TRUE)
  )
}

ggplotts <- function(object,...) UseMethod("ggplotts")

ggplotts.data.frame <- function(object,show.legend, theme,type,
                                xlab,ylab, group,lims,
                                ...) {
  
  object <- object[!is.na(object$Values),]
  
  group <- enquo(group)
  
  g <- ggplot(object,aes(x=Date,y=Values),show.legend = show.legend,...) +
    xlab(xlab) + ylab(ylab)
  switch(type,
         line = g + geom_line(aes(colour=Variables,linetype=Variables,group=!!group),
                              na.rm = TRUE),
         bar = g + geom_bar(aes(fill=Variables,group=!!group),stat="identity") +
           stat_summary(fun = sum, geom="line", colour = "black",
                        size = 0.5, alpha=1,na.rm = TRUE),
         segment = g + geom_segment(aes(xend=Date,colour=Variables,group=!!group),
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

ggplotts.ts <- function(object,show.legend, series_names,theme,type,
                        start, end, xlab,ylab, ...) {
  
  object <- window_default(object,start = start, end = end)
  
  df <- dftsforggplot(object,series_names)
  
  lims <- gglims(object)
  # That x window is set to be able to translate x values
  # of deltat(x)/2 on the right like for base plot init
  
  ggplotts(df,show.legend, theme,type,
           xlab,ylab, group = Variables,lims,
           ...)
}

geom_path_scatter <- function(object,i,lty) {
  
  n <- nrow(object)
  
  df <- data.frame(Time=as.numeric(time(object))[-1L],
                   `High-frequency serie` = object[-n,2L],
                   `Low-frequency serie` = object[-n,1L],
                   xend = object[-1L,2L],
                   yend = object[-1L,1L], check.names = FALSE)
  
  geom_segment(data=df,
               aes(x = `High-frequency serie`, y = `Low-frequency serie`,
                   xend = xend, yend = yend,
                   colour = Time, group = i),
               linetype = lty,
               arrow=arrow(angle = 15,
                           ends = "last",
                           type = "closed",
                           length = unit(0.1,"inches")),
               na.rm = TRUE)
}

ggscatter <- function(object,show.legend, theme, start, end, xlab,ylab,
                      col, lty,...) {
  
  object <- window_default(object,start = start, end = end)
  
  lims <- gglims(object)
  
  lty <- lty(ncol(object)-1L)
  
  g <- ggplot(show.legend = show.legend, ...) + xlab(xlab) + ylab(ylab)
  
  if (!is.null(attr(object,"coefficients"))) {
    g <- g + geom_abline(intercept = attr(object,"coefficients")["constant"],
                         slope = attr(object,"coefficients")[names(attr(object,"coefficients")) != "constant"],
                         lty = "solid", colour = "red", size = 1)
  }
  
  g <- g + geom_path_scatter(object[,c(1L,2L)],1L,lty[1L])
  
  if (ncol(object) == 3L) {
    is_value_reg <- which(!is.na(object[,2L]))
    
    if (is_value_reg[1L] != 1L) {
      object_temp <- object[,c(1L,3L)]
      object_temp[(is_value_reg[1L] + 1L):nrow(object),2L] <- NA
      g <- g + geom_path_scatter(object_temp,2L,lty[2L])
    }
    
    if (is_value_reg[length(is_value_reg)] != nrow(object)) {
      object_temp <- object[,c(1L,3L)]
      object_temp[1L:(is_value_reg[length(is_value_reg)]-1L),2L] <- NA
      g <- g + geom_path_scatter(object_temp,3L,lty[2L]) 
    }
    
    # These are the parts before and after the coefficients calc window
  }
  g <- g + continuous_scale("colour","gradient",function(x) col(length(x)),
                            limits = lims,
                            breaks = break_arrows(as.numeric(time(object)[-1])),
                            minor_breaks = numeric(),
                            expand=c(0,0)) +
    theme +
    guides(colour=guide_legend(override.aes = list(arrow = NULL)))
  g
}

function_if_it_isnt_one <- function(f) {
  if (is.function(f)) f else {
    if (length(f) == 1L) eval(bquote(function(n) rep(.(f),n)))
    else eval(bquote(function(n) .(f)[1L:n]))
  }
}

#' @export
ggplot2::autoplot

autoplot_with_lf <- function(object, xlab, ylab,
                             start, end, col, lty,
                             show.legend, main,
                             mar, theme,
                             serie_name,
                             ...) {
  model <- model.list(object)
  
  col <- function_if_it_isnt_one(col)
  lty <- function_if_it_isnt_one(lty)
  
  hfbench <- window_default(as.ts(object),start = start, end = end)
  
  hfdf <- dftsforggplot(hfbench,
                        series_names = serie_name)
  hfdf[,"group"] <- 1L
  lfdf <- dftsforggplot(ts_expand(model$lfserie,nfrequency = frequency(model$hfserie)),
                        series_names = "Low-frequency serie")
  lfdf[,"group"] <- rep((1L:length(model$lfserie))+1L,
                        each=frequency(model$hfserie)/frequency(model$lfserie))
  
  ggplotts(object = rbind(hfdf,lfdf),show.legend = show.legend,
           theme = theme, type = "line",
           xlab = xlab, ylab = ylab, group = group,
           lims = gglims(hfbench),...) +
    discrete_scale("colour","hue",col,na.translate = FALSE) +
    discrete_scale("linetype","hue",lty,na.translate = FALSE) +
    ggtitle(main)
}

#' @export
#' @rdname plot.tscomparison
autoplot.twoStepsBenchmark <- function(object, xlab = NULL, ylab = NULL,
                                       start=NULL,end=NULL,
                                       col = default_col_pal(object),
                                       lty = default_lty_pal(object),
                                       show.legend = TRUE,
                                       main = NULL,
                                       mar = NULL,
                                       theme = default_theme_ggplot(show.legend,
                                                                    xlab, ylab,
                                                                    mar),
                                       ...) {
  autoplot_with_lf(object, xlab, ylab,
                   start, end, col,
                   lty,show.legend,
                   main,mar,theme, "Benchmark",
                   ...)
}

#' @export
#' @rdname plot.tscomparison
autoplot.threeRuleSmooth <- function(object, xlab = NULL, ylab = NULL,
                                     start=NULL,end=NULL,
                                     col = default_col_pal(object),
                                     lty = default_lty_pal(object),
                                     show.legend = TRUE,
                                     main = NULL,
                                     mar = NULL,
                                     theme = default_theme_ggplot(show.legend,
                                                                  xlab, ylab,
                                                                  mar),
                                     ...) {
  autoplot_with_lf(object, xlab, ylab,
                   start, end, col,
                   lty,show.legend,
                   main,mar,theme, "Smooth",
                   ...)
}

#' @export
#' @rdname plot.tscomparison
autoplot.tscomparison <- function(object, xlab = NULL, ylab = NULL,
                                  start=NULL,end=NULL,
                                  col = default_col_pal(object),
                                  lty = default_lty_pal(object),
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
    if (all(object[,"Trend"] == 0,na.rm = TRUE)) object <- object[,colnames(object) != "Trend", drop = FALSE]
    
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
                          col = col, lty = lty,...) +
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