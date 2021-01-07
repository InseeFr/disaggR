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
#' * they are eventually reintegrated.
#' * they contain the autocorrelated part of the residuals.
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
#' @seealso in_dicator in_revisions in_scatter plot.tscomparison
#' @examples
#' benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
#' plot(in_sample(benchmark))
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
#' @seealso in_sample in_revisions in_scatter plot.tscomparison
#' @examples
#' benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
#' plot(in_dicator(benchmark))
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
                     trend <- 
                       series_with_smoothed_part <- cbind(series[,-1L,drop=FALSE],
                                                          smoothed.part(object),
                                                          if (model.list(object)$include.differenciation && coef(object)["constant"] != 0) {
                                                            coef(object)["constant"] * model.list(object)$hfserie[,"constant"]
                                                          } else 0)
                     diff(ts(t(t(series_with_smoothed_part) * c(coef(object)[names(coef(object)) != "constant"],1,1)),
                             start = start(series_with_smoothed_part),
                             frequency = frequency(series)))/stats::lag(series[,1L],-1) * 100
                   },
                   stop("The type argument of in_dicator should be either \"levels\", \"levels-rebased\" or \"changes\".",call. = FALSE)
  )
  
  structure(window(series,start=start(benchmark),end=end(benchmark),extend=TRUE),
            type=type,
            func="in_dicator",
            class=c("tscomparison",class(series)),
            dimnames=list(NULL,
                          c(if (type != "contributions") "Benchmark",
                            if (is.null(colnames(hfserie))) "High-frequency serie" else colnames(hfserie),
                            if (type == "contributions") c("Smoothed part","Trend")
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
#' @seealso in_sample in_dicator in_scatter plot.tscomparison
#' @examples
#' benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
#' benchmark2 <- twoStepsBenchmark(turnover,construction,include.differenciation = TRUE)
#' plot(in_revisions(benchmark,benchmark2))
#' @export
in_revisions <- function(object,object_old,type="changes") UseMethod("in_revisions")

#' @export
in_revisions.twoStepsBenchmark <- function(object,object_old,type="changes") {
  if (!inherits(object_old,"twoStepsBenchmark")) stop("old_object must be a twoStepsBenchmark", call. = FALSE)
  series <- tryCatch(in_dicator(object,type)-in_dicator(object_old,type),
                     error=function(e) stop(gsub("in_dicator","in_revisions",e$message), call.=FALSE))
  class(series) <- c("tscomparison",class(series))
  if (type != "contributions") {
    if (sum(abs(series[,colnames(series) != "Benchmark",drop=FALSE]),na.rm = TRUE) > 1e-7) {
      warning("The indicators contain revisions!", .call = FALSE)
    }
    series <- structure(series[,"Benchmark",drop = FALSE],
                        type=attr(series,"type"),
                        func="in_revisions")
  }
  class(series) <- c("tscomparison",class(series))
  series
}

#' Comparing the inputs of a praislm regression
#' 
#' The function `in_scatter` returns comparison of the inputs from
#' a \link{praislm} or the inner regression of a \link{twoStepsBenchmark} object.
#' 
#' The functions `plot` and `autoplot` can be used on this object to produce
#' graphics.
#' @param object an object of class `praislm` or `twoStepsBenchmark`
#' @return
#' a named matrix time-serie of two columns, one for the low-frequency serie
#' and the other for the high-frequency-serie (eventually differencied if
#' `include.differenciation` is `TRUE`).
#' A `tscomparison` class is added to the object.
#' @seealso in_sample in_dicator in_revisions plot.tscomparison
#' @examples
#' benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
#' plot(in_scatter(benchmark))
#' @export
#' @export
in_scatter <- function(object) UseMethod("in_scatter")

#' @export
in_scatter.praislm <- function(object) {
  m <- model.list(object)
  
  X <- m$X[,colnames(m$X) != "constant",drop = FALSE]
  
  if (ncol(X) != 1L) stop("This in_scatter method is only for univariate benchmarks.", call. = FALSE)
  
  series <- cbind(m$y,X)
  
  if  (m$include.differenciation) series <- diff(series)
  
  structure(series,
            type=if (m$include.differenciation) "differences" else "levels",
            func="in_scatter",
            class=c("tscomparison",class(series)),
            dimnames=list(NULL,c("Low-frequency serie", "High-frequency serie (regression)")),
            coefficients=coefficients(object))
}

#' @export
in_scatter.twoStepsBenchmark <- function(object) {
  
  m <- model.list(object)
  
  coeff_clean_win <- switch_window(m$start.coeff.calc,
                                   m$end.coeff.calc,
                                   tsp(m$lfserie))
  
  benchmark_clean_win <- switch_window(m$start.benchmark,
                                       m$end.benchmark,
                                       tsp(m$lfserie))
  
  y <- window(m$lfserie,
              min(coeff_clean_win[1L],benchmark_clean_win[1L]),
              max(coeff_clean_win[2L],benchmark_clean_win[2L]))
  
  X <- m$hfserie[,colnames(m$hfserie) != "constant",drop = FALSE]
  
  if (ncol(X) != 1L) stop("This in_scatter method is only for univariate benchmarks.", call. = FALSE)
  
  series <- cbind(y,
                  window(
                    aggregate_and_crop_hf_to_lf(X,
                                                y),
                    coeff_clean_win[1L],
                    coeff_clean_win[2L]),
                  window(
                    aggregate_and_crop_hf_to_lf(X,
                                                y),
                    benchmark_clean_win[1L],
                    benchmark_clean_win[2L])
  )
  
  if  (m$include.differenciation) series <- diff(series)
  
  structure(series,
            type=if (m$include.differenciation) "differences" else "levels",
            func="in_scatter",
            class=c("tscomparison",class(series)),
            dimnames=list(NULL,c("Low-frequency serie",
                                 "High-frequency serie (regression)",
                                 "High-frequency serie (benchmark)")),
            coefficients=coefficients(object))
}

#' @export
print.tscomparison <- function(x, digits = max(3L, getOption("digits") - 3L),...) {
  label <- switch(attr(x,"func")[1L],
                  in_dicator="Comparison with indicator",
                  in_sample="In-sample predictions",
                  in_revisions="Comparison between two benchmarks",
                  in_scatter="Comparison between the inputs")
  cat(label, " (", attr(x,"type"),"):\n", sep = "")
  
  attr(x,"type")         <- NULL
  attr(x,"func")         <- NULL
  attr(x,"coefficients") <- NULL
  
  print(.preformat.ts(x, any(frequency(x) == c(4, 12)) && length(start(x)) == 2L, ...),
        quote = FALSE, right = TRUE,digits = digits,
        ...)
  invisible(x)
}

#' Distance computation for benchmarks
#' 
#' This function `distance` computes the Minkowski distance of exponent p,
#' related to a tscomparison object, produced with `in_sample`, `in_dicator` or
#' `in_sample` 
#' 
#' The meaning depends on the tscomparison function :
#' 
#' * `in_sample` will produce the low-frequency distance between the predicted
#' value and the response, on the coefficient calculation window.
#' * `in_dicator` will produce the high-frequency distance between the inputs
#' (eventually, the sum of its contributions) and the benchmarked serie.
#' * `in_revisions` will produce the high-frequency distance between the two
#' benchmarked series (eventually, between the two contributions sums of the inputs).
#' 
#' @param x an object of class `tscomparison`
#' @param p an integer greater than 1L, or Inf.
#' 
#' @return
#' a numeric of length 1, the distance.
#' @seealso in_sample in_dicator in_revisions
#' @examples
#' benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
#' distance(in_sample(benchmark,type="changes"))
#' distance(in_dicator(benchmark,type="contributions"),p=1L)
#' distance(in_dicator(benchmark,type="changes"),p=Inf)
#' @export
distance <- function(x, p = 2) UseMethod("distance")

#' @export
distance.tscomparison <- function(x, p = 2) {
  if (p < 1) stop("p should be greater than 1", call. = FALSE)
  
  res <- switch(attr(x,"func"),
                in_sample = x[,"Benchmark"] - x[,"Predicted value"],
                in_scatter = stop("This function doesn't work with in_scatter", call. = FALSE),
                in_dicator = {
                  if (identical(attr(x,"type"),"contributions")) x[,"Smoothed part"] + x[,"Trend"]
                  else x[,"Benchmark"] - ts_from_tsp(rowSums(x[,colnames(x) != "Benchmark",drop = FALSE]),
                                                     tsp(x))
                },
                in_revisions = {
                  if (identical(attr(x,"type"),"contributions")) {
                    ts_from_tsp(rowSums(x[,!(colnames(x) %in% c("Smoothed part","Trend")),drop = FALSE]),
                                tsp(x))
                  } else x[,"Benchmark"]
                })
  
  if (p == Inf) max(abs(res),na.rm = TRUE)
  else mean(abs(res)^p,na.rm = TRUE)^(1/p)
}