#' Producing the in sample predictions of a prais-lm regression
#' 
#' The function \link{in_sample} returns in-sample predictions from
#' a \link{praislm} or a \link{twoStepsBenchmark} object.
#' 
#' The predicted values are different from the fitted values :
#' * they are eventually reintegrated
#' * the autocorrelated part of the residuals is added
#' Besides, changes are relative to the latest response value,
#' not the latest predicted value.
#' 
#' @param x an object of class `praislm` or `twoStepsBenchmark`
#' @return
#' a named matrix time-serie of two columns, one for the
#' response and the other for the predicted value.
#' A `insample` class is added to the object. Then, the functions
#' `plot` and `autoplot` (the latter requires to load \pkg{ggplot2})
#' can be used to produce graphics.
#'
#' @export
in_sample <- function(object,type="changes") UseMethod("in_sample")

in_sample_impl <- function(y,y_lagged,predicted_diff,type) {
  y_lagged <- stats::lag(y,k=-1)
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
in_sample.praislm <- function(object,type="changes") {
  autocor <- rho(object)*lag(residuals(object),-1)
  in_sample_impl(y = model.list(object)$y,
                 y_lagged = stats::lag(y,k=-1),
                 predicted_diff =  if (m$include.differenciation) fitted(object) + autocor else fitted(object)+autocor-y_lagged,
                 type = type
 )
}

#' @export
in_sample.twoStepsBenchmark <- function(object,type="changes") {
  m <- model.list(object)
  y <- model.list(object)$lfserie
  y_lagged <- stats::lag(y, k = -1)
  f <- aggregate(window(object$fitted.values,start=tsp(y)[1]),nfrequency = frequency(y))
  autocor <- rho(object) * lag(residuals(object), -1)
  
  in_sample_impl(y = model.list(object)$lfserie,
                 y_lagged = stats::lag(y,k=-1),
                 predicted_diff =  if (m$include.differenciation) diff(f) + autocor else f+autocor-y_lagged,
                 type = type
  )
}