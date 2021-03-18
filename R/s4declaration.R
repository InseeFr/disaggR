# disaggR classes are registered as minimal S4 classes in order to allow
# Ops group generic double dispatch with ts. Through, most of the package uses
# S3 methods for simplicity

#' Virtual Class "disaggR" Class of disaggregations
#'
#' The `disaggR` class is a class contained by the actual disaggregation classes
#' in the disaggR package. It is a “virtual” class.  disaggR classes are
#' minimalist S4 classes, containing disaggR and list. This allows Ops group generic
#' double dispatch but most of the package rather uses S3 methods.
#' 
#' @seealso
#' the functions \link{twoStepsBenchmark} and \link{threeRuleSmooth}.
#' 
#' @aliases 
#' Ops,disaggR,vector-method Ops,vector,disaggR-method
#' Ops,disaggR,ts-method Ops,ts,disaggR-method
#' Ops,disaggR,disaggR-method
#' Math2,disaggR-method
#' show,disaggR-method
setClass("disaggR", contains = "VIRTUAL")

#' @import methods
#' @export
setClass("threeRuleSmooth",contains = c("disaggR","list"))

#' @import methods
#' @export
setClass("twoStepsBenchmark",contains = c("disaggR","list"))
