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
  switch(type,
         changes={
           y_changes <- (y/y_lagged-1)*100
           predicted_changes <- (predicted_diff/y_lagged)*100
           series <- cbind(y_changes,predicted_changes)
           colnames(series) <- c("Response","Predicted value")
           structure(series,type=type,class=c("insample",class(series)))
         },
         levels={
           predicted <- y_lagged+predicted_diff
           series <- cbind(y,predicted)
           colnames(series) <- c("Response","Predicted value")
           structure(series,type=type,class=c("insample",class(series)))
         })
}

#' @export
in_sample.twoStepsBenchmark <- function(object,type="changes") {
  in_sample(prais(object),type=type)
}

#' @export
print.insample <- function(x, digits = max(3L, getOption("digits") - 3L),...) {
  cat("In-sample predictions (", attr(x,"type"),"):\n", sep = "")
  attr(x,"type") <- NULL
  print(.preformat.ts(x, any(frequency(x) == c(4, 12)) && length(start(x)) == 2L, ...),
        quote = FALSE, right = TRUE,digits = digits,
        ...)
  invisible(x)
}