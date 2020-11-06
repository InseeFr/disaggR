#### In-sample

#' Producing the in sample predictions of a prais-lm regression
#' 
#' The function `in_sample` returns in-sample predictions from
#' a \link{praislm} or a \link{twoStepsBenchmark} object.
#' 
#' The predicted values are different from the fitted values :
#' * they are eventually reintegrated
#' * the autocorrelated part of the residuals is added
#' Besides, changes are relative to the latest response value,
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
            class=c("insample","comparison",class(series)),
            dimnames=list(NULL,c("Benchmark","Predicted value")))
}

#' @export
in_sample.twoStepsBenchmark <- function(object,type="changes") {
  in_sample(prais(object),type=type)
}

#' @export
compare_to_hfserie <- function(object,type="changes") UseMethod("compare_to_hfserie")

#' @export
compare_to_hfserie.twoStepsBenchmark <- function(object,type="changes") {
  hfserie <- model.list(object)$hfserie
  hfserie <- hfserie[,colnames(hfserie) != "constant"]
  
  benchmark <- na.omit(as.ts(object))
  
  series <- window(cbind(benchmark,hfserie),start = start(benchmark),end = end(benchmark))
  
  series <- switch(type,
                   levels = series,
                   "levels-rebased" = ts(t(t(series)/series[1L,]) * 100,start=start(series),frequency=frequency(series)),
                   changes = (series/stats::lag(series,-1)-1)*100,
                   contributions = {
                     diff(ts(t(t(series) * c(1,coef(object)[names(coef(object)) != "constant"])),
                             start = start(series),
                             frequency = frequency(series)))/stats::lag(series[,1L],-1) * 100}
                   ,
                   stop("The type argument of in_sample should be either \"levels\", \"levels-rebased\" or \"changes\".",call. = FALSE)
  )
  
  structure(series,
            type=type,
            class=c("comparetohfserie","comparison",class(series)),
            dimnames=list(NULL,
                          c("Benchmark",
                            if (is.null(colnames(hfserie))) "High-Frequency serie" else colnames(hfserie)
                          ))
  )
}

#' @export
print.comparetohfserie <- function(x, digits = max(3L, getOption("digits") - 3L),...) {
  cat("Comparison (", attr(x,"type"),"):\n", sep = "")
  print.comparison(x,digits,...)
}

#' @export
print.insample <- function(x, digits = max(3L, getOption("digits") - 3L),...) {
  cat("In-sample predictions (", attr(x,"type"),"):\n", sep = "")
  print.comparison(x,digits,...)
}

print.comparison <- function(x, digits = max(3L, getOption("digits") - 3L),...) {
  attr(x,"type") <- NULL
  print(.preformat.ts(x, any(frequency(x) == c(4, 12)) && length(start(x)) == 2L, ...),
        quote = FALSE, right = TRUE,digits = digits,
        ...)
  invisible(x)
}