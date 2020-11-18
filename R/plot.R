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
#' @importFrom scales brewer_pal hue_pal
default_col_pal <- function(object) {
  if (identical(attr(object,"type"),"contributions")) brewer_pal(type = "qual",palette = 7L)
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

#### Base plots

plot_init <- function(xmin,xmax,ymin,ymax,xlab,ylab,mar,...) {
  
  if (is.null(xlab)) xlab <- ""
  if (is.null(ylab)) ylab <- ""
  
  sizey <- ymax-ymin
  plot(x= c(xmin,xmax), y = c(ymin,ymax),
       xlim = c(xmin,xmax),ylim= c(ymin-0.02*sizey,ymax+0.02*sizey),
       type = "n",
       xaxs = "i", xaxt = "n",
       yaxs = "i", yaxt = "n",
       xlab = xlab, ylab = ylab,...)
  grid(nx = NA,ny=NULL,col = "grey")
  abline(v = (floor(xmin)+1L):(ceiling(xmax)-1L),lty="dotted",lwd=1,col="grey")
}

plot_init_x <- function(x, xlab, ylab, ...) {
  tspx <- tsp(x)
  plot_init(xmin = tspx[1L],xmax = tspx[2L]+deltat(x),
            # That x window is set to be able to translate x values
            # of deltat(x)/2 on the right
            ymin = min(x,na.rm = TRUE), ymax = max(x,na.rm = TRUE),
            xlab = xlab, ylab = ylab, ...)
}

barplot_mts <- function (height,xlab,ylab,col,space = c(0, 1L, 0), ...) {
  
  timeh <- time(height)
  
  pser <- height * (height > 0)
  nser <- height * (height < 0)
  ppser <- ts_from_tsp(rowSums(pser),tsp(pser))
  nnser <- ts_from_tsp(rowSums(nser),tsp(nser))
  
  tsph <- tsp(height)
  
  plot_init_x(cbind(nnser,ppser),xlab = xlab, ylab = ylab, ...)
  
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

draw_axes <- function(timex) {
  axis(side = 2L, labels=NA, tick = TRUE)
  axis(side = 2L, tick = FALSE, line=-0.5, cex.axis=0.7)
  year <- floor(timex)
  axis(side = 1L, at = c(year,year[length(year)]+1L), labels = NA, tick = TRUE)
  axis(side = 1L, at = year + 0.5, labels = year, tick = FALSE, line = -1.1, cex.axis=0.7)
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
                  xlab,ylab,...) {
  
  col <- eval_function_if_it_is_one(col,NCOL(x))
  lty <- eval_function_if_it_is_one(lty,NCOL(x))
  
  x <- window_default(x,start,end)
  
  timex_win <- as.vector(time(x)) + deltat(x)/2
  
  switch(type,
         line = {
           plot_init_x(x,xlab = xlab, ylab = ylab, ...)
           
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
           barplot_mts(x, col=col, xlab = xlab, ylab = ylab, ...)
           lines(x = timex_win, y = as.vector(rowSums(x)), lty = lty)
           
           if (show.legend) legend("bottomleft",legend=series_names,
                                   fill=col,horiz=TRUE,bty="n",cex=0.8)
         },
         segment = {
           plot_init_x(x,xlab = xlab, ylab = ylab, ...)
           
           lines.default(x = timex_win, y = x,
                         col = col,type = "h")
           
           if (show.legend) legend("bottomleft",legend=series_names,
                                   col=col,lty="solid",horiz=TRUE,bty="n",cex=0.8)
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
                                   ...) {
  
  mar <- par("mar")
  on.exit(par(mar=mar))
  par(mar=c(1,1.5,0,0))
  
  model <- model.list(x)
  x <- as.ts(x)
  
  col <- eval_function_if_it_is_one(col,2L)
  lty <- eval_function_if_it_is_one(lty,2L)
  
  plotts(x = x,show.legend = FALSE,
         col = col[1L],lty = lty[1L],
         series_names = "Benchmark",type = "line",
         start = start, end = end,
         xlab = xlab, ylab = ylab,
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
plot.tscomparison <- function(x, xlab="", ylab="", start = NULL, end = NULL,
                              col=default_col_pal(x),
                              lty=default_lty_pal(),
                              show.legend=TRUE,
                              ...) {
  
  mar <- par("mar")
  on.exit(par(mar=mar))
  par(mar=c(1,1.5,0,0))
  
  type_label <- type_label(x)
  func <- attr(x,"func")
  
  if (type_label == "Contributions") plotts(x = x, show.legend = show.legend,
                                            col = col, lty = lty,
                                            series_names = colnames(x),type = "bar",
                                            start = start, end = end,
                                            xlab = xlab, ylab = ylab,
                                            ...)
  else {
    if (func == "in_revisions") plotts(x = x, show.legend = show.legend,
                                       col = col, lty = lty,
                                       series_names = "Benchmark",type = "segment",
                                       start = start, end = end,
                                       xlab = xlab, ylab = ylab,
                                       ...)
    else plotts(x = x, show.legend = show.legend,
                col = col, lty = lty,
                series_names = colnames(x), type = "line",
                start = start, end = end, 
                xlab = xlab, ylab = ylab,
                ...)
  }
  
  draw_axes(time(x))
  
  invisible()
}

#### ggplot2 plots

#' Default ggplot theme
#' 
#' This is the default theme for the ggplot graphics produced with autoplot
#' 
#' @keywords internal
#' @export
default_theme_ggplot <- function(show.legend,xlab,ylab) theme_classic() %+replace%
  theme(axis.title.x = if (is.null(xlab)) element_blank(),
        axis.title.y = if (is.null(ylab)) element_blank(),
        panel.grid.major = element_line(colour = "#cccccc"),
        legend.position = if (show.legend) "bottom" else "none"
  )

dftsforggplot <- function(object,series_names=NULL) {
  if (is.null(series_names)) series_names <- colnames(object)
  if (is.null(series_names)) series_names <- paste("Serie",1:NCOL(object))
  data.frame(
    Date = as.numeric(time(object)+deltat(object)/2),
    Values = as.numeric(object),
    Variables = factor(do.call(c,lapply(series_names,rep.int,times=NROW(object))),
                       levels=series_names)
  )
}

ggplotts <- function(object,show.legend,variable_aes,
                     series_names,theme,type,
                     start = start, end = end,
                     xlab,ylab,...) {
  
  object <- window_default(object,start = start, end = end)
  
  exprs <- structure(lapply(variable_aes, function(x) quote(Variables)),
                     names=variable_aes)
  
  df <- dftsforggplot(object,series_names)
  
  df <- df[!is.na(df$Values),]
  
  lims <- c(tsp(object)[1L],tsp(object)[2L] + deltat(object))
    # That x window is set to be able to translate x values
    # of deltat(x)/2 on the right like for base plot init
  g <- ggplot(df,aes(x=Date,y=Values),show.legend = show.legend,...) +
    xlab(xlab) + ylab(ylab)
  switch(type,
         line = g + geom_line(aes(,,!!!exprs,group=Variables)),
         bar = g + geom_bar(aes(,,!!!exprs,group=Variables),stat="identity") +
           stat_summary(fun = sum, geom="line", colour = "black",
                        size = 0.5, alpha=1,na.rm = TRUE),
         segment = g + geom_segment(aes(xend=Date,,,!!!exprs,group=Variables),
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
                                       theme = default_theme_ggplot(show.legend, xlab, ylab),
                                       ...) {
  model <- model.list(object)
  
  col <- function_if_it_isnt_one(col)
  lty <- function_if_it_isnt_one(lty)
  
  lfdf <- dftsforggplot(ts_expand(model$lfserie,nfrequency = frequency(model$hfserie)),
                        series_names = "Low-Frequency serie")
  lfdf[,"Low-Frequency Periods"] <- rep(time(model$lfserie) + deltat(model$hfserie),
                                        each=frequency(model$hfserie)/frequency(model$lfserie))
  
  ggplotts(object = as.ts(object),show.legend = show.legend,
           variable_aes = c("colour","linetype"),series_names = "Benchmark",
           theme = theme, type = "line",
           start = start, end = end,
           xlab = xlab, ylab = ylab, ...) +
    geom_line(aes(x=Date,y=Values,colour=Variables,linetype=Variables,
                  group=`Low-Frequency Periods`),lfdf,
              na.rm = TRUE) +
    discrete_scale("colour","hue",col,na.translate = FALSE) +
    discrete_scale("linetype","hue",lty,na.translate = FALSE)
}

#' @export 
autoplot.tscomparison <- function(object, xlab = NULL, ylab = NULL,
                                  start=NULL,end=NULL,
                                  col = default_col_pal(object),
                                  lty = default_lty_pal(),
                                  show.legend = TRUE,
                                  theme = default_theme_ggplot(show.legend, xlab, ylab),
                                  ...) {
  
  col <- function_if_it_isnt_one(col)
  lty <- function_if_it_isnt_one(lty)
  
  type_label <- type_label(object)
  func <- attr(object,"func")
  
  if (type_label == "Contributions") {
    ggplotts(object, show.legend = show.legend,
             variable_aes = "fill", type = "bar",
             series_names = colnames(object), theme = theme,
             start = start, end = end,
             xlab = xlab, ylab = ylab, ...) +
      labs(fill=type_label) +
      discrete_scale("fill","hue",col,na.translate = FALSE)
  }
  else if (func == "in_revisions") {
    ggplotts(object = object, show.legend = show.legend,
             variable_aes = "colour",type="segment",
             series_names = "Benchmark",theme = theme,
             start = start, end = end,
             xlab = xlab, ylab = ylab, ...) +
      labs(colour=type_label) +
      discrete_scale("colour","hue",col,na.translate = FALSE)
  }
  else ggplotts(object = object, show.legend = show.legend,
                variable_aes = c("colour","linetype"),type="line",
                series_names = colnames(object), theme = theme,
                start = start, end = end,
                xlab = xlab, ylab = ylab, ...) +
    labs(linetype=type_label) +
    labs(colour=type_label) +
    discrete_scale("colour","hue",col,na.translate = FALSE) +
    discrete_scale("linetype", "linetype_d", lty,na.translate = FALSE)
}