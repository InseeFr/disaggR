#' Virtual Class "disaggR" Class of disaggregations
#'
#' The `"disaggR"` class is a class contained by the actual disaggregation
#' classes in the disaggR package. It is a “virtual” class.  disaggR classes are
#' minimalist S4 classes, containing disaggR and list. This allows Ops group
#' generic double dispatch but most of the package rather uses S3 methods.
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
#' threeRuleSmooth-class
#' twoStepsBenchmark-class
#' @keywords internal
#' @export
setClass("disaggR", contains = "VIRTUAL")

#' @import methods
#' @keywords internal
#' @export
setClass("threeRuleSmooth",contains = c("disaggR","list"))

#' @import methods
#' @keywords internal
#' @export
setClass("twoStepsBenchmark",contains = c("disaggR","list"))

# S3 registering function for other package generics.
# Taken from the package vctrs under the permissive
# unlicense. Preferred to base S3method in order to keep disaggR compatible
# with R >= 3.4.0, not R >= 3.6.0.

s3_register <- function (generic, class, method = NULL) 
{
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]
  caller <- parent.frame()
  get_method_env <- function() {
    top <- topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    }
    else {
      caller
    }
  }
  get_method <- function(method, env) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    }
    else {
      method
    }
  }
  register <- function(...) {
    envir <- asNamespace(package)
    method_fn <- get_method(method)
    stopifnot(is.function(method_fn))
    if (exists(generic, envir)) {
      registerS3method(generic, class, method_fn, envir = envir)
    }
    else if (identical(Sys.getenv("NOT_CRAN"), "true")) {
      warning(sprintf("Can't find generic `%s` in package %s to register S3 method.", 
                      generic, package))
    }
  }
  setHook(packageEvent(package, "onLoad"), register)
  if (isNamespaceLoaded(package)) {
    register()
  }
  invisible()
}

.onLoad <- function(...) {
  s3_register("ggplot2::autoplot", "twoStepsBenchmark")
  s3_register("ggplot2::autoplot", "threeRuleSmooth")
  s3_register("ggplot2::autoplot", "tscomparison")
}
