#' @title Two-Steps Benchmarks for Time Series Disaggregation
#' 
#' @description 
#'
#' The `twoStepsBenchmark()` and `threeRuleSmooth()` functions allow you to 
#' disaggregate a low-frequency time-serie with higher frequency time-series, 
#' using the French National Accounts methodology. The aggregated sum of the 
#' resulting time-serie is strictly equal to the low-frequency serie within the 
#' benchmarking window. Typically, the low-frequency serie is an annual one, 
#' unknown for the last year, and the high frequency one is either quarterly or 
#' monthly.
#' 
#' See "Methodology of quarterly national accounts", Insee Méthodes 
#' N°126, by Insee (2012, ISBN:978-2-11-068613-8,
#' https://www.insee.fr/en/information/2579410).
#'
#' @import stats graphics
#' @keywords internal
"_PACKAGE"

globalVariables(c("Date","Values","Low-Frequency Periods","Variables",
                  "High-frequency serie","Low-frequency serie",
                  "Low-frequency periods","Time","xend","yend",
                  "group"))
