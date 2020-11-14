#### In-sample

#' Producing the in sample predictions of a prais-lm regression
#' 
#' The function `tscomparison` returns in-sample predictions from
#' a \link{praislm} or a \link{twoStepsBenchmark} object.
#' 
#' The predicted values are different from the fitted values :
#' 
#' * they are eventually reintegrated
#' * the autocorrelated part of the residuals is added.
#' 
#' Besides, changes are relative to the latest benchmark value,
#' not the latest predicted value.
#' 
#' @param object an object of class `praislm` or `twoStepsBenchmark`
#' @param type "changes" or "levels". The results are either returned
#' in changes or in levels.
#' @return
#' a named matrix time-serie of two columns, one for the
#' response and the other for the predicted value.
#' A `insample` class is added to the object. Then, the functions
#' `plot` and `autoplot` (the latter requires to load \pkg{ggplot2})
#' can be used to produce graphics.
#' @examples
#' benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
#' in_sample(benchmark)
#' @export
in_sample <- function(object,type="changes") UseMethod("in_sample")

#' @export
in_sample.praislm <- function(object,type="changes") {
  autocor <- rho(object)*lag(residuals(object),-1)
  m <- model.list(object)
  y <- m$y
  y_lagged <- stats::lag(y,k=-1)
  predicted_diff <- if (m$include.differenciation) fitted(object) + autocor else fitted(object)+autocor-y_lagged
  
  series <- switch(type,
                   levels={
                     predicted <- y_lagged+predicted_diff
                     cbind(y,predicted)
                   },
                   changes={
                     y_changes <- (y/y_lagged-1)*100
                     predicted_changes <- (predicted_diff/y_lagged)*100
                     cbind(y_changes,predicted_changes)
                   },
                   stop("The type argument of in_sample should be either \"levels\" or  \"changes\".",call. = FALSE))

  structure(series,
            type=type,
            func="in_sample",
            class=c("tscomparison",class(series)),
            dimnames=list(NULL,c("Benchmark","Predicted value")))
}

#' @export
in_sample.twoStepsBenchmark <- function(object,type="changes") {
  in_sample(prais(object),type=type)
}

#' @export
in_dicator <- function(object,type="changes") UseMethod("in_dicator")

#' @export
in_dicator.twoStepsBenchmark <- function(object,type="changes") {
  hfserie <- model.list(object)$hfserie
  hfserie <- hfserie[,colnames(hfserie) != "constant"]
  
  benchmark <- na.omit(as.ts(object))
  
  series <- cbind(benchmark,hfserie)
  
  series <- switch(type,
                   levels = series,
                   "levels-rebased" = ts(t(t(series)/series[1L,]) * 100,start=start(series),frequency=frequency(series)),
                   changes = (series/stats::lag(series,-1)-1)*100,
                   contributions = {
                     series_with_smoothed_part <- cbind(series[,-1L,drop=FALSE],smoothed.part(object))
                     diff(ts(t(t(series_with_smoothed_part) * c(coef(object)[names(coef(object)) != "constant"],1)),
                             start = start(series),
                             frequency = frequency(series)))/stats::lag(series[,1L],-1) * 100
                   },
                   stop("The type argument of in_sample should be either \"levels\", \"levels-rebased\" or \"changes\".",call. = FALSE)
  )
  
  structure(window(series,start=start(benchmark),end=end(benchmark),extend=TRUE),
            type=type,
            func="in_dicator",
            class=c("tscomparison",class(series)),
            dimnames=list(NULL,
                          c(if (type != "contributions") "Benchmark",
                            if (is.null(colnames(hfserie))) "High-frequency serie" else colnames(hfserie),
                            if (type == "contributions") "Smoothed part"
                          ))
  )
}

#' @export
in_revisions <- function(object,object_old,type="changes") UseMethod("in_revisions")

#' @export
in_revisions.twoStepsBenchmark <- function(object,object_old,type="changes") {
  if (!inherits(object_old,"twoStepsBenchmark")) stop("old_object must be a twoStepsBenchmark", call. = FALSE)
  series <- in_dicator(object,type)-in_dicator(object_old,type)
  class(series) <- c("tscomparison",class(series))
  if (type != "contributions") {
    if (sum(abs(series[,colnames(series) != "Benchmark",drop=FALSE]),na.rm = TRUE) > 1e-7) {
      warning("Warning : The indicators contain revisions!")
    }
    series <- structure(series[,"Benchmark",drop = FALSE],
                        type=attr(series,"type"),
                        func="in_revisions")
  }
  class(series) <- c("tscomparison",class(series))
  series
}

#' @export
print.tscomparison <- function(x, digits = max(3L, getOption("digits") - 3L),...) {
  label <- switch(attr(x,"func")[1L],
                  in_dicator="Comparison with indicators",
                  in_sample="In-sample predictions",
                  in_revisions="Comparison between two benchmarks")
  cat(label, " (", attr(x,"type"),"):\n", sep = "")
  attr(x,"type") <- NULL
  attr(x,"func") <- NULL
  print(.preformat.ts(x, any(frequency(x) == c(4, 12)) && length(start(x)) == 2L, ...),
        quote = FALSE, right = TRUE,digits = digits,
        ...)
  invisible(x)
}