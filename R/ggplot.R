ggthemets <- function() ggplot2::theme_classic() %+replace% ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                                                                           axis.title.y = ggplot2::element_blank(),
                                                                           panel.grid.major = ggplot2::element_line(colour = "#cccccc"),
                                                                           legend.position = "bottom")

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

ggplotts <- function(object,show.legend = !is.null(dim(object)),variable_aes="colour",series_names=NULL,theme=ggthemets(),...) {
  exprs <- structure(lapply(variable_aes, function(x) quote(Variables)),
                     names=variable_aes)
  df <- dftsforggplot(object,series_names)
  lims <- c(floor(min(df$Date[!is.na(df$Values)])),
            ceiling(max(df$Date[!is.na(df$Values)])))
  ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=Date,y=Values,group=Variables,,,!!!exprs),
                       df,
                       show.legend = show.legend,na.rm = TRUE,...) +
    ggplot2::scale_x_continuous(
      limits = lims,
      breaks = lims[1]:(lims[2]-1),
      minor_breaks = numeric(),
      expand=c(0,0)
    ) + 
    theme
}

#' @export
gginsampleplot <- function(object) UseMethod("gginsampleplot")
#' @export
gginsampleplot.praislm <- function(object,type="changes") {
  m <- model.list(object)
  y <- model.list(object)$y
  switch(type,
         changes={
           y_lagged <- stats::lag(y,k=-1)
           y_changes <- (y/y_lagged-1)*100
           fitted_diff <- if (m$include.differenciation) object$fitted.values else diff(object$fitted.values)
           fitted_changes <- (fitted_diff/y_lagged)*100
           series <- cbind(y_changes,fitted_changes)
           colnames(series) <- c("Response","Predicted value")
           ggplotts(series,variable_aes = "linetype") + labs(linetype="Percentage changes")
         })
}
#' @export
gginsampleplot.twoStepsBenchmark <- function(object) {
  gginsampleplot.praislm(object$regression)
}

#' @importFrom ggplot2 autoplot
#' @export 
autoplot.twoStepsBenchmark <- function(object) {
  model <- model.list(object)
  x <- as.ts(object)
  lfdf <- dftsforggplot(tsExpand(model$lfserie,nfrequency = frequency(model$hfserie)),series_names = "Low-Frequency serie")
  lfdf[,"Low-Frequency Periods"] <- rep(time(model$lfserie),each=frequency(model$hfserie)/frequency(model$lfserie))
  ggplotts(x,show.legend = TRUE,series_names = "Benchmark",variable_aes = "linetype") +
    geom_line(ggplot2::aes(x=Date,y=Values,linetype=Variables,group=`Low-Frequency Periods`),lfdf) +
    labs(linetype=element_blank())
}
