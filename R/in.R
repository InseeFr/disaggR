#' Producing the in sample predictions of a prais-lm regression
#' 
#' The function `in_sample` returns in-sample predictions from
#' a \link{praislm} or a \link{twoStepsBenchmark} object.
#' 
#' The functions `plot` and `autoplot` can be used on this object to produce
#' graphics.
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
#' A `tscomparison` class is added to the object.
#' @seealso in_dicator in_revisions
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

#' Comparing a benchmark with its indicator
#' 
#' The function `in_dicator`takes a \link{twoStepsBenchmark} object
#' as an input. It produces a comparison between the benchmarked time-serie and the
#' input.
#' 
#' The functions `plot` and `autoplot` can be used on this object to produce
#' graphics.
#' 
#' @param object an object of class twoStepsBenchmark`
#' @param type "levels","levels-rebased", "changes" or "contributions". This
#' defines the type of output.
#' @return
#' a named matrix time-serie of two columns, one for the
#' response and the other for the predicted value.
#' A `tscomparison` class is added to the object.
#' @seealso in_sample in_revisions
#' @examples
#' benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
#' in_dicator(benchmark)
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
                             start = start(series_with_smoothed_part),
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

#' Comparing two benchmarks together
#' 
#' The function `in_revisions`takes two \link{twoStepsBenchmark} objects
#' as inputs, and produces a comparison between those.
#' 
#' The functions `plot` and `autoplot` can be used on this object to produce
#' graphics.
#' 
#' @param object an object of class twoStepsBenchmark`
#' @param object_old an object of class twoStepsBenchmark`
#' @param type "levels","levels-rebased", "changes" or "contributions". This
#' defines the type of output.
#' @return
#' a named matrix time-serie of two columns, one for the
#' response and the other for the predicted value.
#' A `tscomparison` class is added to the object.
#' @seealso in_sample in_dicator
#' @examples
#' benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
#' in_dicator(benchmark)
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