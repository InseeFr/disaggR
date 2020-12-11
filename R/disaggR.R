#' @title Two-Steps Benchmarks for Time Series Disaggregation
#' 
#' @description 
#'
#' The twoStepsBenchmark() function and its wrappers allow you to disaggregate a low frequency time serie with time series of higher frequency, using the French National Accounts methodology.
#' The aggregated sum of the resulting time-serie is strictly equal to the low-frequency serie within the benchmarking window.
#' Typically, the low frequency serie is an annual one, unknown for the last year, and the high frequency is either quarterly or mensual.
#' See "Methodology of quarterly national accounts", Insee Méthodes N°126, by Insee (2012, ISBN:978-2-11-068613-8).
#'
#' @import stats graphics ggplot2 shiny
#' @keywords internal
#' @useDynLib disaggR, .registration=TRUE
#' @importFrom Rcpp sourceCpp
"_PACKAGE"

globalVariables(c("Date","Values","Low-Frequency Periods","Variables",
                  "High-frequency serie","Low-frequency serie",
                  "Low-frequency periods","Time","xend","yend"))