type_label <- function(object) {
  switch(attr(object,"type"),
         levels="Levels",
         `levels-rebased`="Rebased levels",
         changes="Changes",
         contributions="Contributions"
  )
}

plot_init <- function(xmin,xmax,ymin,ymax,xlab,ylab,...) {
  sizey <- ymax-ymin
  plot(x= c(xmin,xmax), y = c(ymin,ymax),
       xlim = c(xmin,xmax),ylim= c(ymin-0.1*sizey,ymax+0.1*sizey),
       type = "n",
       xaxs = "i", xaxt = "n",
       yaxs = "i", yaxt = "n",
       xlab = xlab, ylab = ylab,...)
  grid(nx = NA,ny=NULL,col = "grey")
  abline(v = (floor(xmin)+1L):(ceiling(xmax)-1L),lty="dotted",lwd=1,col="grey")
}

#' @export
#' @importFrom scales brewer_pal hue_pal
default_col_pal <- function(object) {
  if (identical(attr(object,"type"),"contributions")) brewer_pal(type = "qual",palette = 7L)
  else brewer_pal(type = "qual",palette = 6L)
}

#' @importFrom scales linetype_pal
#' @importFrom graphics plot points
#' @export
plot.twoStepsBenchmark <- function(x, xlab="", ylab="",
                                   start = NULL, end = NULL,
                                   col=default_col_pal(x),
                                   lty=linetype_pal(),
                                   ...) {
  
  model <- model.list(x)
  x <- as.ts(x)
  
  col <- eval_function_if_is_one(col,2L)
  lty <- eval_function_if_is_one(lty,2L)
  
  timex <- time(x)
  start <- if (is.null(start)) floor(min(timex[!(is.na(x))])) else start
  end <- if (is.null(end)) ceiling(max(timex[!(is.na(x))])) - deltat(x) else end
  
  x <- window(x,start=start,end=end,extend=TRUE)
  
  timex_win <- as.vector(time(x)) + deltat(x)/2
  
  plot_init(xmin = timex_win[1L]-deltat(x)/2,xmax = timex_win[length(timex_win)]+deltat(x)/2,
            ymin = min(x,na.rm = TRUE), ymax = max(x,na.rm = TRUE),
            xlab = xlab, ylab = ylab,
            ...)
  
  graphics::lines.default(timex_win,x,col = col[1L],lty = lty[1L])
  Map(
    function(time,value) lines(ts_expand(ts(value,
                                            start=time,
                                            frequency = frequency(model$lfserie)),
                                         nfrequency=frequency(model$hfserie)),
                               col = col[2L],
                               lty = lty[2L]),
    time(model$lfserie)+deltat(model$hfserie),
    model$lfserie
  )
  
  axis(side = 2L)
  year <- floor(timex)
  axis(side = 1, at = c(year,year[length(year)]+1L), labels = NA, tick = TRUE)
  axis(side = 1, at = year + 0.5, labels = year, tick = FALSE, 
       line = -1)
  invisible()
}

barplot_mts_add <- function (height,xlab,ylab,col,space = c(1L, 3L, 1L),
                         ...) {
  
  timeh <- time(height)
  freq <- frequency(height)
  
  pser <- height * (height > 0)
  nser <- height * (height < 0)
  ppser <- ts_from_tsp(rowSums(pser),tsp(pser))
  nnser <- ts_from_tsp(rowSums(nser),tsp(nser))
  
  tsph <- tsp(height)
  
  plot_init(xmin = timeh[1L],xmax = timeh[length(timeh)],
            ymin = min(nnser,na.rm = TRUE), ymax = max(ppser,na.rm = TRUE),
            xlab = xlab, ylab = ylab,
            ...)
  
  xxx <- 1/sum(c(2, freq, freq - 1) * space[1:3])
  
  cc <- cycle(pser)
  xleft <- floor(round(timeh, 4)) +
    (space[1] + (cc - 1) * space[2] +(cc - 1) * space[3]) * xxx
  xright <- xleft + space[2] * xxx
  
  for (i in ncol(height):1L) {
    rect(xleft = xleft, xright = xright,
         ybottom = nnser, ytop = ppser,
         col = col[i],border=FALSE)
    nnser <- nnser - nser[, i]
    ppser <- ppser - pser[, i]
  }
  invisible()
}

eval_function_if_is_one <- function(f,arg) {
  if (is.function(f)) f(arg) else f
}

#' @importFrom scales linetype_pal
#' @importFrom graphics lines.default points.default
#' @export
plot.tscomparison <- function(x, xlab="", ylab="", start = NULL, end = NULL,
                              col=default_col_pal(x),
                              lty=linetype_pal(),
                              ...) {
  
  col <- eval_function_if_is_one(col,NCOL(x))
  lty <- eval_function_if_is_one(lty,NCOL(x))
  
  type_label <- type_label(x)
  func <- attr(x,"func")
  
  timex <- time(x)
  start <- if (is.null(start)) floor(min(timex[apply(x,1L,function(x) !all(is.na(x)))])) else start
  end <- if (is.null(end)) ceiling(max(timex[apply(x,1L,function(x) !all(is.na(x)))])) - deltat(x) else end
  
  x <- window(x,start=start,end=end,extend=TRUE)
  
  timex_win <- as.vector(time(x)) + deltat(x)/2
  
  if (type_label == "Contributions") {
    barplot_mts_add(x,
                    col=col,
                    xlab = xlab, ylab = ylab,
                    ...)
    lines(x = timex_win, y = as.vector(rowSums(x)), lty = lty)
  }
  else {
    plot_init(xmin = timex_win[1L]-deltat(x)/2,xmax = timex_win[length(timex_win)]+deltat(x)/2,
              ymin = min(x,na.rm = TRUE), ymax = max(x,na.rm = TRUE),
              xlab = xlab, ylab = ylab,
              ...)
    if (func == "in_revisions") {
      graphics::lines.default(x=timex_win, y=x, col=col,type="h")
    }
    else {
      Map(
        graphics::lines.default,
        x = list(timex_win),
        y = lapply(1:NCOL(x),function(i) x[,i]),
          # can be replaced by apply(x,2,identity,simplify = FALSE) in r 4.1
        col = col,
        lty = lty
      )
    }
  }
  axis(side = 2L)
  year <- floor(timex)
  axis(side = 1, at = c(year,year[length(year)]+1L), labels = NA, tick = TRUE)
  axis(side = 1, at = year + 0.5, labels = year, tick = FALSE, 
       line = -1)
  invisible()
}

#' @importFrom ggplot2 %+replace%
ggthemets <- function() ggplot2::theme_classic() %+replace%
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_line(colour = "#cccccc"),
                 legend.position = "bottom",
                 plot.tag.position = "top",
                 plot.tag = element_text(size = 10,colour="red")
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

#' @importFrom ggplot2 geom_line geom_bar stat_summary aes element_text
ggplotts <- function(object,show.legend = !is.null(dim(object)),variable_aes="colour",
                     series_names=NULL,theme=ggthemets(),type="line",do.sum=FALSE,
                     start=NULL,end=NULL,...) {
  exprs <- structure(lapply(variable_aes, function(x) quote(Variables)),
                     names=variable_aes)
  
  df <- dftsforggplot(object,series_names)
  isnaval <- is.na(df$Values)
  
  start <- {
    if (is.null(start)) floor(min(df$Date[!isnaval]))
    else switch(length(start),
                start,
                start[1L] +(start[2L] - 1)/frequency(object),
                stop("bad value for 'start'"))
  }
  
  end <- {
    if (is.null(end)) ceiling(max(df$Date[!isnaval]))
    else switch(length(end),
                end,
                end[1L] + (end[2L] - 1)/frequency(object), 
                stop("bad value for 'end'"))
  }
  
  df <- df[df$Date >= start & df$Date<= end & !isnaval,]
  
  lims <- c(start,end)
  
  g <- ggplot2::ggplot(df,ggplot2::aes(x=Date,y=Values),
                       show.legend = show.legend,...)
  switch(type,
         line=g + ggplot2::geom_line(ggplot2::aes(,,!!!exprs,group=Variables)),
         bar=g+ ggplot2::geom_bar(ggplot2::aes(,,!!!exprs,group=Variables),stat="identity"),
         segments=g +ggplot2::geom_segment(aes(xend=Date,,,!!!exprs,group=Variables),yend=0)
  ) +
    ggplot2::scale_x_continuous(
      limits = lims,
      breaks = (floor(lims[1L])+1L):(ceiling(lims[2L])-1L),
      minor_breaks = numeric(),
      expand=c(0,0)
    ) + 
    theme +
    if (do.sum) ggplot2::stat_summary(fun = sum, geom="line", colour = "black", size = 0.5, alpha=1,na.rm = TRUE)
}

#' @importFrom ggplot2 autoplot labs
#' @export 
autoplot.twoStepsBenchmark <- function(object, xlab = "", ylab = "",start=NULL,end=NULL,
                                       col = default_col_pal(object),
                                       lty = linetype_pal(),
                                       ...) {
  model <- model.list(object)
  
  col <- make_function_if_it_isnt(col)
  lty <- make_function_if_it_isnt(lty)
  
  object <- window(as.ts(object),start=start,end=end,extend=TRUE)
  
  lfdf <- dftsforggplot(ts_expand(model$lfserie,nfrequency = frequency(model$hfserie)),
                        series_names = "Low-Frequency serie")
  lfdf[,"Low-Frequency Periods"] <- rep(time(model$lfserie) + deltat(model$hfserie),
                                        each=frequency(model$hfserie)/frequency(model$lfserie))
  
  ggplotts(as.ts(object),show.legend = TRUE,series_names = "Benchmark",variable_aes = c("colour","linetype"),
           start=start,end=end) +
    ggplot2::geom_line(ggplot2::aes(x=Date,y=Values,colour=Variables,linetype=Variables,
                                    group=`Low-Frequency Periods`),lfdf,
                       na.rm = TRUE) +
    discrete_scale("colour","hue",col,na.translate = FALSE) +
    discrete_scale("linetype","hue",lty,na.translate = FALSE)
}

make_function_if_it_isnt <- function(f) {
  if (is.function(f)) f else {
    if (length(f) == 1) eval(bquote(function(n) rep(.(f),n)))
    else eval(bquote(function(n) .(f)))
  }
}

#' @importFrom ggplot2 discrete_scale
#' @export 
autoplot.tscomparison <- function(object, xlab = "", ylab = "",start=NULL,end=NULL,
                                  col = default_col_pal(object),
                                  lty = linetype_pal(),
                                  ...) {
  
  col <- make_function_if_it_isnt(col)
  lty <- make_function_if_it_isnt(lty)
  
  type_label <- type_label(object)
  func <- attr(object,"func")
  
  object <- window(object,start=start,end=end,extend=TRUE)
  
  if (type_label == "Contributions") {
    ggplotts(object,
             variable_aes = "fill",type = "bar",do.sum=TRUE,
             start=start,end=end) +
      ggplot2::labs(fill=type_label) +
      discrete_scale("fill","hue",col,na.translate = FALSE)
  }
  else if (func == "in_revisions") {
    ggplotts(object,
             variable_aes = "colour",type="segments",
             start=start,end=end) +
      ggplot2::labs(colour=type_label) +
      discrete_scale("colour","hue",col,na.translate = FALSE)
  }
  else ggplotts(object,
                variable_aes = c("colour","linetype"),type="line",
                start=start,end=end) +
    ggplot2::labs(linetype=type_label) +
    ggplot2::labs(colour=type_label) +
    discrete_scale("colour","hue",col,na.translate = FALSE) +
    discrete_scale("linetype", "linetype_d", lty,na.translate = FALSE)
}