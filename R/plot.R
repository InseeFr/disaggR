#' @importFrom graphics plot points
#' @export
plot.twoStepsBenchmark <- function(x, xlab = "", ylab = "", ...) {
  model <- model.list(x)
  tsbench <- as.ts(x)
  lims <- c(floor(min(time(tsbench)[!is.na(tsbench)])),
            ceiling(max(time(tsbench)[!is.na(tsbench)])))
  plot(window(tsbench,start=lims[1],end=lims[2],extend=TRUE)
       , xlab = xlab, ylab = ylab, ...)
  points(ts_expand(model$lfserie,nfrequency = frequency(model$hfserie)),cex=0.25,pch=20)
  invisible()
}

#' @export
plot.comparison <- function(x, xlab="", ylab="", ...) {
  class(x) <- class(x)[-1]
  lims <- c(floor(min(time(x)[!is.na(x[,1])|!is.na(x[,2])])),
            ceiling(max(time(x)[!is.na(x[,1])|!is.na(x[,2])])))
  plot(window(x,start=lims[1],end=lims[2],extend=TRUE), plot.type="single", lty=c(1L,2L), xlab = xlab, ylab = ylab,
       main = paste0("In-sample predictions (", attr(x,"type"),")"), ...)
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
  )[!is.na(as.numeric(object)),]
}

#' @importFrom ggplot2 geom_line geom_bar stat_summary aes element_text
ggplotts <- function(object,show.legend = !is.null(dim(object)),variable_aes="colour",series_names=NULL,theme=ggthemets(),type="line",do.sum=FALSE,...) {
  exprs <- structure(lapply(variable_aes, function(x) quote(Variables)),
                     names=variable_aes)
  df <- dftsforggplot(object,series_names)
  lims <- c(floor(min(df$Date)),
            ceiling(max(df$Date)))
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
      breaks = lims[1]:(lims[2]-1),
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
  x <- window(as.ts(object),start=start,end=end,extend=TRUE)
  lfdf <- dftsforggplot(ts_expand(model$lfserie,nfrequency = frequency(model$hfserie)),series_names = "Low-Frequency serie")
  lfdf[,"Low-Frequency Periods"] <- rep(time(model$lfserie),each=frequency(model$hfserie)/frequency(model$lfserie))
  lfdf <- lfdf[!is.na(lfdf$Values),]
  ggplotts(x,show.legend = TRUE,series_names = "Benchmark",variable_aes = "linetype") +
    ggplot2::geom_line(ggplot2::aes(x=Date,y=Values,linetype=Variables,group=`Low-Frequency Periods`),lfdf) +
    ggplot2::labs(linetype=ggplot2::element_blank())
}

autoplot.indicator <- function(object,start=NULL,end=NULL) NextMethod()
autoplot.insample <- function(object,start=NULL,end=NULL) NextMethod()
autoplot.inrevisions <- function(object,start=NULL,end=NULL) NextMethod()

#' @importFrom ggplot2 autoplot
#' @export
autoplot.tscomparison <- function(object,start=NULL,end=NULL) {
  
  type_label <- switch(attr(object,"type"),
                       levels="Levels",
                       `levels-rebased`="Rebased levels",
                       changes="Changes",
                       contributions="Contributions"
                       )
  
  object <- window(object,start=start,end=end,extend=TRUE)
  
  if (type_label == "Contributions") {
    ggplotts(object[,(colnames(object) != "Benchmark"),drop=FALSE],
             variable_aes = "fill",type = "bar",
             do.sum=TRUE) +
      ggplot2::labs(fill=type_label)
  }
  else if (attr(.Class,"previous")[1L] == "inrevisions") {
    ggplotts(object[,(colnames(object) == "Benchmark"),drop=FALSE],variable_aes = "shape",type="lollipop") +
      ggplot2::labs(shape=type_label) +
      if (sum(abs(object[,(colnames(object) != "Benchmark"),drop=FALSE]),na.rm = TRUE) > 1e-7) {
        labs(tag="Warning : The indicators contain revisions!")
      }
  }
  else ggplotts(object,variable_aes = "linetype") +
    ggplot2::labs(linetype=type_label)
}