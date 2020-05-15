#' @importFrom stats coef
#' @export
coef.praislm <- function(x) x$coefficients

#' @importFrom stats residuals
#' @export
residuals.praislm <- function(x) x$residuals

#' @export
rho <- function(x) UseMethod("rho")
#' @export
rho.praislm <- function(x) x$rho

#' @export
model.list.praislm <- function(x) x$model.list

#' @export
se <- function(x) UseMethod("se")
#' @export
se.praislm <- function(x) x$se

#' @importFrom stats vcov
#' @export
vcov.praislm <- function(object) {
  resc <- residuals(object)-mean(residuals(object))
  n <- length(resc)
  rho <- rho(object)
  epsilon <- c(
    resc[1L]*sqrt(1L-rho^2),
    resc[-1L]-rho*resc[-n]
  )
  epsilon_variance <- as.numeric(crossprod(epsilon)/(df.residual(object)))
  omega_inv <- diag(n)
  diag(omega_inv)[-c(1L,n)] <- 1+rho^2
  diag(omega_inv[1L:(n-1L),2L:n]) <- -rho
  diag(omega_inv[2L:n,1L:(n-1L)]) <- -rho
  
  m <- model.list(object)
  X <- m$X[,!(colnames(m$X) %in% names(m$set.coefficients))]
  if (m$include.differenciation) X <- diff(X)

  return(epsilon_variance*solve(crossprod(X,omega_inv)%*%X))
}

#' @export
print.praislm <- function(x, digits = max(3L, getOption("digits") - 3L)) {
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  cat("Coefficients:\n")
  print.default(format(coef(x), digits = digits), print.gap = 2L,quote = FALSE)
  cat("\n")
  invisible(x)
}

#' @importFrom stats fitted
#' @export
fitted.praislm <- function(x) x$fitted.values

#' @export
summary.praislm <- function (object, ...) {
  serr <- se(object)
  tval <- coef(object)/serr
  TAB <- cbind(Estimate = coef(object), StdErr = serr, t.value = tval, 
               p.value = 2 * suppressWarnings(pt(-abs(tval), df = object$df)))
  rownames(TAB) <- names(object$coefficients)
  colnames(TAB) <- c("Estimate", "StdErr", "t.value", "p.value")
  mss <- sum(object$fitted.values.decorrelated^2)
  rss <- sum(object$residuals.decorrelated^2)
  r.squared <- mss/(mss + rss)
  n <- length(object$fitted.values.decorrelated)
  rdf <- object$df.residual
  adj.r.squared <- 1 - (1 - r.squared) * n/rdf
  rho <- object$rho
  
  pm <- rbind(residuals=do.call(c,Box.test(object$residuals, lag = 1, type = "Ljung-Box")[c("statistic","p.value")]),
              if (rho != 0) residuals.decorrelated=do.call(c,Box.test(object$residuals.decorrelated, lag = 1, type = "Ljung-Box")[c("statistic","p.value")]))
  colnames(pm) <- c("statistic","p.value")
  res <- list(call = object$call, coefficients = TAB, r.squared = r.squared, 
              adj.r.squared = adj.r.squared, sigma = sqrt(sum((object$residuals)^2)/rdf), 
              df = object$df, residSum = summary.default(object$residuals,digits = 5)[-4],
              rho=rho,pm=pm)
  class(res) <- "summary.praislm"
  res
}

#' @export
print.summary.praislm <- function (x, digits=max(3, getOption("digits") - 3L),
                                   signif.stars = getOption("show.signif.stars"),...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nResiduals:\n")
  print(x$residSum,digits = digits)
  cat("\n")
  printCoefmat(x$coefficients, P.values = TRUE, has.Pvalue = TRUE,digits=digits,signif.stars=signif.stars)
  cat("\nResidual standard error: ", formatC(x$sigma, digits = digits), 
      " on ", formatC(x$df), " degrees of freedom\n", sep = "")
  cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits), 
      ",\tAdjusted R-squared: ", formatC(x$adj.r.squared, digits = digits), 
      "\n", sep = "")
  
  cat("\nPortmanteau:\n")
  pm <- x$pm
  if (x$rho==0) {
    rownames(pm) <- "u"
    mes <- "Where Y=X*C+u"
  } else {
    rownames(pm) <- c("u","epsilon")
    mes <- c("Where Y=X*C+u","Where u=rho*lag(u)+epsilon")
  }

  pm <- formatC(pm,digits=digits)
  if (signif.stars) {
    Signif <- symnum(x$pm[,"p.value"], corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                     symbols = c("***", "**", "*", ".", " "))
    pm <- cbind(pm, format(Signif))
  }
  pm <- cbind(pm,"||",format(mes))
  print.default(pm, quote = FALSE, right = TRUE,...)
}

#' Producing the in sample predictions of a prais-lm regression
#' 
#' The function \link{insample} returns in-sample predictions from
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
#' A `insamplepraislm` class is added to the object. Then, the functions
#' `plot` and `autoplot` (the latter requires to load \pkg{ggplot2})
#' can be used to produce graphics.
#'
#' @export
insample <- function(object,type="changes") UseMethod("insample")

#' @export
insample.praislm <- function(object,type="changes") {
  m <- model.list(object)
  y <- model.list(object)$y
  y_lagged <- stats::lag(y,k=-1)
  autocor <- rho(object)*lag(residuals(object),-1)
  predicted_diff <- if (m$include.differenciation) object$fitted.values + autocor else object$fitted.values+autocor-y_lagged
  
  switch(type,
         changes={
           y_changes <- (y/y_lagged-1)*100
          predicted_changes <- (predicted_diff/y_lagged)*100
           series <- cbind(y_changes,predicted_changes)
           colnames(series) <- c("Response","Predicted value")
           structure(series,type=type,class=c("insamplepraislm",class(series)))
         },
         levels={
           predicted <- y_lagged+predicted_diff
           series <- cbind(y,predicted)
           colnames(series) <- c("Response","Predicted value")
           structure(series,type=type,class=c("insamplepraislm",class(series)))
         })
}