#' Producing the in sample predictions of a prais-lm regression
#' 
#' The function `in_sample` returns in-sample predictions from a \link{praislm}
#' or a \link{twoStepsBenchmark} object.
#' 
#' The functions `plot` and `autoplot` can be used on this object to produce
#' graphics.
#' 
#' The predicted values are different from the fitted values :
#' 
#' * they are eventually reintegrated.
#' * they contain the autocorrelated part of the residuals.
#' 
#' Besides, changes are relative to the latest benchmark value, not the latest
#' predicted value.
#' 
#' @param object an object of class `"praislm"` or `"twoStepsBenchmark"`.
#' @param type `"changes"` or `"levels"`. The results are either returned
#' in changes or in levels.
#' @return
#' a named matrix time-serie of two columns, one for the response and the other
#' for the predicted value.
#' A `"tscomparison"` class is added to the object.
#' @seealso \link{in_disaggr} \link{in_revisions} \link{in_scatter}
#' \link{plot.tscomparison}
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
  predicted_diff <- {
    if (m$include.differenciation) fitted(object) + autocor
    else fitted(object)+autocor-y_lagged
  }
  
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
                   stop("The type argument of in_sample should be either \"levels\" or  \"changes\"",call. = FALSE))
  
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
in_sample.threeRuleSmooth <- function(object,type="changes") {
  stop("The in_sample method needs a regression hence isn't applicable on a threeRuleSmooth object",call. = FALSE)
}

#' Comparing a disaggregation with the high-frequency input
#' 
#' The function `in_disaggr` takes a \link{twoStepsBenchmark} or a 
#' \link{threeRuleSmooth} object as an input. It produces a comparison between
#' the benchmarked time-serie and the high-frequency input.
#' 
#' The functions `plot` and `autoplot` can be used on this object to produce
#' graphics.
#' 
#' @param object an object of class `"twoStepsBenchmark"` or `"threeRuleSmooth"`.
#' @param type `"levels"`,`"levels-rebased"`, `"changes"` or `"contributions"`.
#' This defines the type of output.
#' @return
#' a named matrix time-serie of two columns, one for the response and the other
#' for the input.
#' A `tscomparison` class is added to the object.
#' @seealso \link{in_sample} \link{in_revisions} \link{in_scatter}
#' \link{plot.tscomparison}
#' @examples
#' benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
#' plot(in_disaggr(benchmark))
#' @export
in_disaggr <- function(object,type="changes") UseMethod("in_disaggr")

#' @export
in_disaggr.twoStepsBenchmark <- function(object,type="changes") {
  
  hfserie <- hfserie(object)
  
  benchmark <- na.omit(as.ts(object))
  
  series <- cbind(benchmark,hfserie)
  
  series <- switch(type,
                   levels = series,
                   "levels-rebased" =
                     ts(
                       t(t(series) /
                           series[which(apply(!(series == 0) & !(is.na(series)),1L, all))[1L],]
                         ) * 100,
                       start=start(series),
                       frequency=frequency(series)),
                   changes = (series/stats::lag(series,-1)-1)*100,
                   contributions = {
                     series_with_smoothed_part <-
                       cbind(series[,-1L,drop=FALSE],
                             smoothed.part(object),
                             if (model.list(object)$include.differenciation &&
                                 coef(object)["constant"] != 0) {
                               coef(object)["constant"] * model.list(object)$hfserie[,"constant"]
                             }
                             else 0)
                     diff(ts(t(t(series_with_smoothed_part) * c(coef(object)[names(coef(object)) != "constant"],1,1)),
                             start = start(series_with_smoothed_part),
                             frequency = frequency(series)))/stats::lag(series[,1L],-1) * 100
                   },
                   stop("The type argument of in_disaggr should be either \"levels\", \"levels-rebased\" or \"changes\".",call. = FALSE)
  )
  
  structure(window(series,start=start(benchmark),end=end(benchmark),extend=TRUE),
            type=type,
            func="in_disaggr",
            class=c("tscomparison",class(series)),
            dimnames=list(NULL,
                          c(if (type != "contributions") "Benchmark",
                            if (is.null(colnames(hfserie))) "High-frequency serie" else colnames(hfserie),
                            if (type == "contributions") c("Smoothed part","Trend")
                          ))
  )
}

#' @export
in_disaggr.threeRuleSmooth <- function(object,type="changes") {
  if (type == "contributions") {
    
    benchmark <- na.omit(as.ts(object))
    
    series <- cbind((benchmark/stats::lag(benchmark,-1)-1)*100,
                    0,
                    0)
    
    structure(window(series,start=start(benchmark),end=end(benchmark),extend=TRUE),
              type="contributions",
              func="in_disaggr",
              class=c("tscomparison",class(series)),
              dimnames=list(NULL,
                            c("High-frequency serie","Smoothed part","Trend")
              ))
  }
  else in_disaggr.twoStepsBenchmark(object,type)
}

#' Comparing two disaggregations together
#' 
#' The function `in_revisions`takes two inputs, \link{twoStepsBenchmark} or a 
#' \link{threeRuleSmooth}, and produces a comparison between those.
#' 
#' The functions `plot` and `autoplot` can be used on this object to produce
#' graphics.
#' 
#' @param object an object of class `"twoStepsBenchmark"` or `"threeRuleSmooth"`.
#' @param object_old an object of class `"twoStepsBenchmark"` or `"threeRuleSmooth"`.
#' @param type `"levels"`,`"levels-rebased"`, `"changes"` or `"contributions"`.
#' This defines the type of output.
#' @return
#' a named matrix time-serie of two columns, one for the response and the other
#' for the predicted value.
#' A `tscomparison` class is added to the object.
#' @seealso \link{in_sample} \link{in_disaggr} \link{in_scatter}
#' \link{plot.tscomparison}
#' @examples
#' benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
#' benchmark2 <- twoStepsBenchmark(turnover,construction,include.differenciation = TRUE)
#' plot(in_revisions(benchmark,benchmark2))
#' @export
in_revisions <- function(object,object_old,type="changes") UseMethod("in_revisions")

#' @export
in_revisions.twoStepsBenchmark <- function(object,object_old,type="changes") {
  if (!inherits(object_old,"twoStepsBenchmark") &&
      !inherits(object_old,"threeRuleSmooth")) stop("old_object must be a twoStepsBenchmark or a threeRuleSmooth", call. = FALSE)
  
  tryCatch({
    indic_old <- in_disaggr(object_old,type)
    indic_new <- in_disaggr(object,type)
    series <- indic_new-indic_old
    colnames(series) <- colnames(indic_new)
  },
  error=function(e) stop(gsub("in_disaggr","in_revisions",e$message), call.=FALSE))
  
  if (type != "contributions") {
    if (mean(abs(series[,colnames(series) != "Benchmark",drop=FALSE]),na.rm = TRUE) > 1e-7) {
      warning("The high-frequency inputs contain revisions!", .call = FALSE)
    }
    series <- series[,"Benchmark",drop = FALSE]
  }
  
  attr(series,"type") <- type
  attr(series,"func") <- "in_revisions"
  class(series) <- c("tscomparison",class(series))
  
  series
}

#' @export
in_revisions.threeRuleSmooth <- in_revisions.twoStepsBenchmark

#' Comparing the inputs of a praislm regression
#' 
#' The function `in_scatter` returns low-frequency comparisons of the inputs from
#' a \link{praislm}, a \link{twoStepsBenchmark} or \link{threeRuleSmooth}.
#' 
#' The functions `plot` and `autoplot` can be used on this object to produce
#' graphics.
#' @param object an object of class `"praislm"`, `"twoStepsBenchmark"`
#' or `"threeRuleSmooth"`.
#' @return
#' a named matrix time-serie of two columns, one for the low-frequency serie
#' and the other for the high-frequency-serie (eventually differencied if
#' `include.differenciation` is `TRUE`).
#' A `tscomparison` class is added to the object.
#' @seealso \link{in_sample} \link{in_disaggr} \link{in_revisions}
#' \link{plot.tscomparison}
#' @examples
#' benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
#' plot(in_scatter(benchmark))
#' @export
in_scatter <- function(object) UseMethod("in_scatter")

#' @export
in_scatter.praislm <- function(object) {
  m <- model.list(object)
  
  X <- m$X[,!grepl(outliers_pattern,colnames(m$X)) &
                   colnames(m$X) != "constant",
                 drop = FALSE]
  
  if (ncol(X) != 1L) stop("This in_scatter method only supports univariate benchmarks", call. = FALSE)
  
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
              max(coeff_clean_win[2L],benchmark_clean_win[2L]),
              extend = TRUE)
  
  X <- m$hfserie[,!grepl(outliers_pattern,colnames(m$hfserie)) &
                   colnames(m$hfserie) != "constant",
                 drop = FALSE]
  
  if (ncol(X) != 1L) stop("This in_scatter method only supports univariate benchmarks", call. = FALSE)
  
  lfx <- aggregate_and_crop_hf_to_lf(X,y)
  
  series <- cbind(y,
                  window(
                    lfx,
                    coeff_clean_win[1L],
                    coeff_clean_win[2L],
                    extend = TRUE),
                  window(
                    lfx,
                    benchmark_clean_win[1L],
                    benchmark_clean_win[2L],
                    extend = TRUE)
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
in_scatter.threeRuleSmooth <- function(object) {
  m <- model.list(object)
  
  benchmark_clean_win <- switch_window(m$start.benchmark,
                                       m$end.benchmark,
                                       tsp(m$lfserie))
  
  y <- window(m$lfserie,
              benchmark_clean_win[1L],
              benchmark_clean_win[2L],
              extend = TRUE)
  
  X <- m$hfserie
  
  series <- cbind(y,
                  window(
                    aggregate_and_crop_hf_to_lf(X,
                                                y),
                    benchmark_clean_win[1L],
                    benchmark_clean_win[2L],
                    extend = TRUE)
  )
  
  structure(series,
            type= "levels",
            func="in_scatter",
            class=c("tscomparison",class(series)),
            dimnames=list(NULL,c("Low-frequency serie",
                                 "High-frequency serie (benchmark)")),
            coefficients=coefficients(object))
}

#' @export
print.tscomparison <- function(x, digits = max(3L, getOption("digits") - 3L),...) {
  label <- switch(attr(x,"func")[1L],
                  in_disaggr="Comparison between the benchmark and the input",
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

#' Distance computation for disaggregations
#' 
#' This function `distance` computes the Minkowski distance of exponent p,
#' related to a tscomparison object, produced with `in_sample`, `in_disaggr` or
#' `in_revisions` 
#' 
#' The meaning depends on the tscomparison function :
#' 
#' * `in_sample` will produce the low-frequency distance between the predicted
#' value and the response, on the coefficient calculation window.
#' * `in_disaggr` will produce the high-frequency distance between the inputs
#' (eventually, the sum of its contributions) and the benchmarked serie.
#' * `in_revisions` will produce the high-frequency distance between the two
#' benchmarked series (contributions distance isn't permitted).
#' 
#' @param x an object of class `tscomparison`
#' @param p an integer greater than 1L, or Inf.
#' 
#' @return
#' a numeric of length 1, the distance.
#' @seealso in_sample in_disaggr in_revisions
#' @examples
#' benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
#' distance(in_sample(benchmark,type="changes"))
#' distance(in_disaggr(benchmark,type="contributions"),p=1L)
#' distance(in_disaggr(benchmark,type="changes"),p=Inf)
#' @export
distance <- function(x, p = 2) UseMethod("distance")

#' @export
distance.tscomparison <- function(x, p = 2) {
  if (p < 1) stop("p should be greater than 1", call. = FALSE)
  
  res <- switch(attr(x,"func"),
                in_sample = x[,"Benchmark"] - x[,"Predicted value"],
                in_scatter = stop("The distance method doesn't support in_scatter results", call. = FALSE),
                in_disaggr = {
                  if (identical(attr(x,"type"),"contributions")) x[,"Smoothed part"] + x[,"Trend"]
                  else x[,"Benchmark"] - ts_from_tsp(rowSums(x[,colnames(x) != "Benchmark",drop = FALSE]),
                                                     tsp(x))
                },
                in_revisions = {
                  if (identical(attr(x,"type"),"contributions")) stop("The distance method doesn't support revisions of contributions", call. = FALSE)
                  else x[,"Benchmark"]
                })
  
  if (p == Inf) max(abs(res),na.rm = TRUE)
  else mean(abs(res)^p,na.rm = TRUE)^(1/p)
}