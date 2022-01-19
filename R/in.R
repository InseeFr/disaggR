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

#' @importFrom stats lag
#' @export
in_sample.praislm <- function(object,type="changes") {
  autocor <- rho(object)*lag(residuals(object),-1)
  m <- model.list(object)
  y <- m$y
  y_lagged <- lag(y,k=-1)
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

replace_colnames_with_labels <- function(x) {
  cln <- colnames(x)
  cln[cln == "constant"] <- "Trend"
  cln[cln == "hfserie"] <- "High-frequency serie"
  colnames(x) <- cln
  x
}

trend_is_last_col <- function(x) {
  string <- colnames(x)
  constant_which <- which(string == "Trend")
  if (length(constant_which) == 0L) x
  else x[,c(setdiff(seq_len(length(string)),constant_which),constant_which),
         drop = FALSE]
}

#' @importFrom stats lag
in_disaggr_notctb <- function(object,type) {
  
  hfserie <- neither_outlier_nor_constant(object)
  
  hfserie <- replace_colnames_with_labels(hfserie)
  
  benchmark <- na.omit(as.ts(object))
  
  series <- cbind(benchmark,
                  hfserie)
  
  series <- switch(type,
                   levels = series,
                   "levels-rebased" =
                     ts(
                       t(t(series) /
                           series[which(apply(!(series == 0) & !(is.na(series)),1L, all))[1L],]
                       ) * 100,
                       start=start(series),
                       frequency=frequency(series)),
                   changes = (series/lag(series,-1)-1)*100,
                   stop("The type argument of in_disaggr should be either \"contributions\", \"levels\", \"levels-rebased\" or \"changes\".",call. = FALSE)
  )
  
  structure(window(series,start=start(benchmark),end=end(benchmark),extend=TRUE),
            dimnames=list(NULL,
                          c("Benchmark",colnames(hfserie))
            ),
            type=type,
            func="in_disaggr",
            class=c("tscomparison",class(series))
  )
  
}

in_disaggr_ctb <- function(object,type) UseMethod("in_disaggr_ctb")

in_disaggr_ctb.threeRuleSmooth <- function(object) {
  benchmark <- na.omit(as.ts(object))
  
  series <- cbind((benchmark/lag(benchmark,-1)-1)*100,
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

in_disaggr_ctb.twoStepsBenchmark <- function(object) {
  
  hfserie <- model.list(object)$hfserie
  
  hfserie <- replace_colnames_with_labels(hfserie)
  
  benchmark <- na.omit(as.ts(object))
  
  series <- cbind(hfserie,smoothed.part(object))
  
  series <- diff(ts(t(t(series) * c(coef(object),1)),
                    start = start(series),
                    frequency = frequency(series)))/lag(benchmark,-1) * 100
  
  structure(
    trend_is_last_col(
      structure(window(series,start=start(benchmark),end=end(benchmark),extend=TRUE),
                dimnames=list(NULL,
                              c(colnames(hfserie),
                                "Smoothed part")
                ))),
    type="contributions",
    func="in_disaggr",
    class=c("tscomparison",class(series))
  )
}

#' @export
in_disaggr.twoStepsBenchmark <- function(object,type="changes") {
  if (type == "contributions") in_disaggr_ctb(object)
  else in_disaggr_notctb(object,type)
}

#' @export
in_disaggr.threeRuleSmooth <- function(object,type="changes") {
  if (type == "contributions") in_disaggr_ctb(object)
  else in_disaggr_notctb(object,type)
}

safe_difference <- function(x,y) {
  cnx <- colnames(x)
  cny <- colnames(y)
  
  cnxlack <- setdiff(cny,cnx)
  cnylack <- setdiff(cnx,cny)
  
  cnres <- c(intersect(cnx,cny),cnxlack,cnylack)
  
  x <- structure(cbind(x,
                       if (!length(cnxlack) == 0L) {
                         ts_from_tsp(matrix(0,
                                            nrow = nrow(x),
                                            ncol = length(cnxlack)),
                                     tsp(x))
                       }),
                 dimnames = list(NULL,
                                 c(cnx,cnxlack)))[,
                                                  cnres,
                                                  drop = FALSE]
  
  y <- structure(cbind(y,
                       if (!length(cnylack) == 0L) {
                         ts_from_tsp(matrix(0,
                                            nrow = nrow(y),
                                            ncol = length(cnylack)),
                                     tsp(y))
                       }),
                 dimnames = list(NULL,
                                 c(cny,cnylack)))[,
                                                  cnres,
                                                  drop = FALSE]
  
  res <- x-y
  colnames(res) <- cnres
  
  res
  
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
    series <- 
      trend_is_last_col(
        safe_difference(in_disaggr(object,type),
                        in_disaggr(object_old,type))
      )
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
#' @param type `"levels"` or `"changes"`.
#' This defines the type of output. A differencied model can't have a scatterplot
#' in levels.
#' @return
#' a named matrix time-serie of two columns, one for the low-frequency serie
#' and the other for the high-frequency-serie (eventually differencied if
#' `include.differenciation` is `TRUE`).
#' A `tscomparison` class is added to the object.
#'  
#' For a `twoStepsBenchmark` object, if outlier effects are estimated, 
#' the contributions of the outliers are substracted from the low-frequency serie.

#' 
#' For a `twoStepsBenchmark` object, the matrix has three columns, for the low-frequency serie, 
#' the high-frequency on the regression span and the high-frequency serie on the benchmark span.
#' 
#' @seealso \link{in_sample} \link{in_disaggr} \link{in_revisions}
#' \link{plot.tscomparison}
#' @examples
#' benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
#' plot(in_scatter(benchmark))
#' @include utils.R
#' @export
in_scatter <- function(object,
                       type = if (model.list(object)$include.differenciation) "changes" else "levels") UseMethod("in_scatter")

warning_news_outlier_in_scatter <- warning_news_factory("The in_scatter function now substracts the outlier contributions from the low-frequency serie. See NEWS. This warning is displayed once in each R session.")

outliers_ctb <- function(object) {
  outliers <- outliers(object, as.ts = TRUE)
  if (is.null(outliers)) NULL
  else {
    warning_news_outlier_in_scatter()
    ts_from_tsp(outliers %*%
                     coefficients(object)[colnames(outliers)],
                   tsp(outliers))
  }
}

has_any_ls <- function(object) any(substr(names(outliers(object)),1,2) == "LS")

clean_wins <- function(m) {
  list(
    coeff     = switch_window(m$start.coeff.calc,
                              m$end.coeff.calc,
                              tsp(m$lfserie)),
    benchmark = switch_window(m$start.benchmark,
                              m$end.benchmark,
                              tsp(m$lfserie))
  )
}

get_y_in_scatter <- function(m, is.twoStepsBenchmark) {
  if (is.twoStepsBenchmark) {
    clean_wins <- clean_wins(m)
    
    window(m$lfserie,
           min(clean_wins$coeff[1L],clean_wins$benchmark[1L]),
           max(clean_wins$coeff[2L],clean_wins$benchmark[2L]),
           extend = TRUE)
  }
  else m$y
}

get_series_in_scatter <- function(X, m, is.twoStepsBenchmark, ctb_outliers_to_sub) {
  
  y <- get_y_in_scatter(m, is.twoStepsBenchmark)
  
  if (!is.null(ctb_outliers_to_sub)) {
    y <- y - aggregate_and_crop_hf_to_lf(ctb_outliers_to_sub,y)
  }
  
  if (is.twoStepsBenchmark) {
    clean_wins <- clean_wins(m)
    lfx <- aggregate_and_crop_hf_to_lf(X,y)
    `colnames<-`(cbind(y,
                       window(
                         lfx,
                         clean_wins$coeff[1L],
                         clean_wins$coeff[2L],
                         extend = TRUE),
                       window(
                         lfx,
                         clean_wins$benchmark[1L],
                         clean_wins$benchmark[2L],
                         extend = TRUE)),
                 c("Low-frequency serie",
                   "High-frequency serie (regression)",
                   "High-frequency serie (benchmark)"))
  }
  else `colnames<-`(cbind(y,X),c("Low-frequency serie",
                                 "High-frequency serie (regression)"))
}

in_scatter_impl <- function(object, type) {
  
  is.twoStepsBenchmark <- class(object) == "twoStepsBenchmark"
  
  m <- model.list(object)
  
  if (! type %in% c("levels", "changes")) stop("The type argument of in_scatter should be either \"levels\" or \"changes\".",call. = FALSE)
  if (type == "levels" && m$include.differenciation) stop("The model is differencied hence the scatterplot should be in changes",
                                                          call. = FALSE)
  
  X <- neither_outlier_nor_constant(object)
  
  if (ncol(X) != 1L) stop("This in_scatter method only supports univariate benchmarks", call. = FALSE)
  
  series <- get_series_in_scatter(X, m, is.twoStepsBenchmark,outliers_ctb(object))
  
  if  (type == "changes") series <- diff(series)
  
  structure(series,
            type=type,
            func="in_scatter",
            class=c("tscomparison",class(series)),
            abline=
              c(
                constant = {
                  if (! m$include.differenciation && type == "changes") 0
                  else  as.numeric(coefficients(object)["constant"])
                },
                slope=as.numeric(coefficients(object)[colnames(X)])
              ))
}

#' @export
in_scatter.praislm <- function(object,
                               type = if (model.list(object)$include.differenciation) "changes" else "levels") {
  in_scatter_impl(object, type)
}

#' @export
in_scatter.twoStepsBenchmark <- function(object,
                                         type = if (model.list(object)$include.differenciation) "changes" else "levels") {
  in_scatter_impl(object, type)
}

#' @export
in_scatter.threeRuleSmooth <- function(object, type = "levels") {
  
  if (! type %in% c("levels", "changes")) stop("The type argument of in_scatter should be either \"levels\" or \"changes\".",call. = FALSE)
  
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
  
  if  (type == "changes") series <- diff(series)
  
  structure(series,
            type= type,
            func="in_scatter",
            class=c("tscomparison",class(series)),
            dimnames=list(NULL,c("Low-frequency serie",
                                 "High-frequency serie (benchmark)")))
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
  attr(x,"abline") <- NULL
  
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
