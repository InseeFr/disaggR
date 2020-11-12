#' @importFrom graphics plot points
#' @export
plot.twoStepsBenchmark <- function(x, xlab = "", ylab = "",
                                   start=NULL,end=NULL, ...) {
  model <- model.list(x)
  tsbench <- as.ts(x)
  
  lims <- c(if (is.null(start)) floor(min(time(tsbench)[!is.na(tsbench)])) else start,
            if (is.null(end)) ceiling(max(time(tsbench)[!is.na(tsbench)])) else end)
  
  plot(window(tsbench,start=lims[1L],end=lims[2L],extend=TRUE)
       , xlab = xlab, ylab = ylab, ...)
  points(window(ts_expand(model$lfserie,nfrequency = frequency(model$hfserie)),
                start=lims[1L],end=lims[2L],extend=TRUE),cex=0.25,pch=20)
  invisible()
}

type_label <- function(object) {
  switch(attr(object,"type"),
         levels="Levels",
         `levels-rebased`="Rebased levels",
         changes="Changes",
         contributions="Contributions"
  )
}

plot.indicator <- function(object,xlab="",start=NULL,end=NULL) NextMethod()
plot.insample <- function(object,xlab="",start=NULL,end=NULL) NextMethod()
plot.inrevisions <- function(object,xlab="",start=NULL,end=NULL) NextMethod()

barplot.ts <- function (height,start,end,space = c(1L, 3L, 1L),col=hue_pal(c(0, 360) + 15,100,65,1)(NCOL(height)),
                        main=NULL,
                        ...) 
{
  start <-  switch(length(start),
                   start,
                   start[1L] +(start[2L] - 1)/frequency(height),
                   stop("bad value for 'start'"))
  end <- switch(length(end),
                end,
                end[1L] + (end[2L] - 1)/frequency(height), 
                stop("bad value for 'end'"))
  
  heigth <- window(height,start=start,end=end,extend=TRUE)
  time <- time(height)
  year <- floor(time)
  freq <- frequency(height)
  nheight <- NCOL(height)
  
  pser <- height * (height > 0)
  nser <- height * (height < 0)
  ppser <- ts_from_tsp(rowSums(pser),tsp(pser))
  nnser <- ts_from_tsp(rowSums(nser),tsp(nser))
  
  plot(x= c(start,end), y = c(min(nnser,na.rm = TRUE),max(ppser,na.rm=TRUE)), type = "n", main = main, xaxs = "i", 
       xaxt = "n", xlab = "", ylab = "")
  grid(nx = NA,ny=NULL,col = "grey")
  abline(v = (floor(start)+1L):(ceiling(end)-1L),lty="dotted",lwd=par("lwd"),col="grey")
  
  xxx <- 1/sum(c(2, freq, freq - 1) * space[1:3])
  cc <- cycle(pser)
  xleft <- floor(round(time, 4)) + (space[1] + (cc - 
                                                  1) * space[2] + (cc - 1) * space[3]) * xxx
  xright <- xleft + space[2] * xxx
  
  for (i in nheight:1) {
    rect(xleft = xleft, xright = xright, ybottom = nnser, 
         ytop = ppser, col = col[i],border=FALSE)
    if (nheight > 1) {
      nnser <- nnser - nser[, i]
      ppser <- ppser - pser[, i]
    }
  }
  axis(side = 1, at = year, labels = NA, tick = TRUE)
  axis(side = 1, at = year + 0.5, labels = year, tick = FALSE, 
       line = -1)
  invisible()
}

#' @export
plot.tscomparison <- function(x, xlab="", ylab="", start = NULL, end = NULL, ...) {
  
  type_label <- type_label(x)
  
  start <- if (is.null(start)) floor(min(time(x)[apply(x,1L,function(x) !all(is.na(x)))])) else start
  end <- if (is.null(end)) ceiling(max(time(x)[apply(x,1L,function(x) !all(is.na(x)))])) else end
  
  x <- window(x,start=start,end=end,extend=TRUE)
  
  if (type_label == "Contributions") {
    barplot.ts(x[,colnames(x) != "Benchmark",drop=FALSE],start=start,end=end)
    lines(x = as.vector(time(x)) + 0.5/frequency(x), y = as.vector(x[,"Benchmark"]))
  }
  else if (attr(.Class,"previous")[1L] == "inrevisions") {
    df <- dftsforggplot(x[,"Benchmark",drop=FALSE])
    plot(df$Date, df$Values, type="h", xlab = xlab, ylab = ylab,
         ...)
    points(df$Date,df$Values,pch = 20L)
  }
  else {
    stats::plot.ts(x, plot.type="single", lty=c(1L,2L), xlab = xlab, ylab = ylab, ...)
  }

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
         line=g + ggplot2::geom_line(aes(,,!!!exprs,group=Variables)),
         bar=g+ ggplot2::geom_bar(stat="identity",aes(,,!!!exprs,group=Variables)),
         lollipop=g +ggplot2::geom_point(alpha=0.8,size=1.5,aes(,,!!!exprs,group=Variables))+
           geom_bar(stat="identity",position="dodge",width=0.015,colour="black")
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
autoplot.twoStepsBenchmark <- function(object,start=NULL,end=NULL) {
  model <- model.list(object)
  
  lfdf <- dftsforggplot(ts_expand(model$lfserie,nfrequency = frequency(model$hfserie)),
                        series_names = "Low-Frequency serie")
  lfdf[,"Low-Frequency Periods"] <- rep(time(model$lfserie),
                                        each=frequency(model$hfserie)/frequency(model$lfserie))
  
  ggplotts(as.ts(object),show.legend = TRUE,series_names = "Benchmark",variable_aes = "linetype",
           start=start,end=end) +
    ggplot2::geom_line(ggplot2::aes(x=Date,y=Values,linetype=Variables,group=`Low-Frequency Periods`),lfdf,
                       na.rm = TRUE) +
    ggplot2::labs(linetype=ggplot2::element_blank())
}

autoplot.indicator <- function(object,start=NULL,end=NULL) NextMethod()
autoplot.insample <- function(object,start=NULL,end=NULL) NextMethod()
autoplot.inrevisions <- function(object,start=NULL,end=NULL) NextMethod()

#' @importFrom ggplot2 autoplot
#' @export
autoplot.tscomparison <- function(object,start=NULL,end=NULL) {
  
  type_label <- type_label(object)
  
  object <- window(object,start=start,end=end,extend=TRUE)
  
  if (type_label == "Contributions") {
    ggplotts(object[,(colnames(object) != "Benchmark"),drop=FALSE],
             variable_aes = "fill",type = "bar",do.sum=TRUE,
             start=start,end=end) +
      ggplot2::labs(fill=type_label)
  }
  else if (attr(.Class,"previous")[1L] == "inrevisions") {
    ggplotts(object[,"Benchmark",drop=FALSE],
             variable_aes = "shape",type="lollipop",
             start=start,end=end) +
      ggplot2::labs(shape=type_label) +
      if (sum(abs(object[,(colnames(object) != "Benchmark"),drop=FALSE]),na.rm = TRUE) > 1e-7) {
        labs(tag="Warning : The indicators contain revisions!")
      }
  }
  else ggplotts(object,variable_aes = "linetype",
                start=start,end=end) +
    ggplot2::labs(linetype=type_label)
}