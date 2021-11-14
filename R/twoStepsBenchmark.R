outliers_pattern <- "^(AO|LS)([0-9]+?)(?:T([0-9]+?))?$"

split_outlier_names <- function(outlier_strings) {
  
  str <- regmatches(outlier_strings,
                    regexec("^(AO|LS)([0-9]+?)(?:T([0-9]+?))?$",
                            outlier_strings))
  
  if (any(lengths(str) == 0L)) stop("The outlier names can't be interpreted (see ?twoStepsBenchmark)",
                                    call. = FALSE)
  
  structure(
    lapply(str, function(x) list(type=x[2L],
                                 year=as.integer(x[3L]),
                                 cycle=if (identical(x[4L],"")) 1L else as.integer(x[4L]))),
    names = outlier_strings)
  
}

minmax_tsp <- function(ts_list) {
  
  tsp_list <- vapply(ts_list,tsp,FUN.VALUE = c(0,0,0))
  
  c(min(tsp_list[1L,]),
    max(tsp_list[2L,]))
  
}

cbind_outliers <- function(outliers_ts_list,start,end) {
  
  minmax <- minmax_tsp(outliers_ts_list)
  minmax[1L] <- min(minmax[1L],start)
  minmax[2L] <- max(minmax[2L],end)
  
  hffreq <- frequency(outliers_ts_list[[1L]])
  
  res <- window(
    ts(
      vapply(
        outliers_ts_list,
        function(serie) {
          
          tspser <- tsp(serie)
          
          c(rep(0,round((tspser[1L]-minmax[1L])*hffreq)),
            as.numeric(serie),
            rep(switch(attr(serie,"type"),
                       AO = 0,
                       LS = serie[length(serie)]),
                round((minmax[2L]-tspser[2L])*hffreq)))
          
        },
        FUN.VALUE = rep(0,round((minmax[2L]-minmax[1L])*hffreq)+1L)
      ),
      start = minmax[1L],
      frequency = hffreq),
    start = start,
    end = end,
    extend = TRUE)
  
  res
}

interpret_outliers <- function(outliers,lffreq,hfserie) {
  
  if (is.null(outliers)) return()
  
  if (!inherits(outliers,"list")) stop("The outliers must be a named list (see ?twoStepsBenchmark)",
                                       call. = FALSE)
  
  if (is.null(names(outliers))) stop("The outliers list must have names (see ?twoStepsBenchmark)",
                                     call. = FALSE)
  
  tsphf <- tsp(hfserie)
  
  ratio <- frequency(hfserie)/lffreq
  
  outliers_ts_list <-
    Map(
      function(splitted_name,vect) {
        if (length(vect) %% ratio != 0L) stop("The outlier vector must be of length k * hf/lf",
                                              call. = FALSE)
        
        res <- structure(
          ts(as.numeric(vect),
             start = splitted_name$year + (splitted_name$cycle - 1L) / lffreq,
             frequency = tsphf[3L]),
          type = splitted_name$type
        )
        
        if (is.ts(vect) && !tsp_equal(tsp(vect),tsp(res)))
          stop("The outlier list contains time-series whose windows or frequencies are inconsistent",
               call. = FALSE)
        
        res
        
      },
      split_outlier_names(names(outliers)),
      outliers)
  
  res <- cbind_outliers(
    outliers_ts_list,
    start = tsphf[1L],
    end = tsphf[2L]
  )
  
  res
  
}

residuals_extrap_sequence <- function(u0,u1,rho,n,include.differenciation) {
  if (include.differenciation) {
    if (rho == 1) u1 + (u1-u0) * (1:n)
    else (u1-u0)*(1-rho^(2:(n+1)))/(1-rho)+u0
  }
  else u1*(rho^(1:n))
}

#' Extrapolation function for the residuals in a twoStepsBenchmark
#' 
#' This function is the rule to extrapolate the low-frequency residuals.
#' If include.differenciation is `TRUE`, u(n+1)-u(n) = rho*(u(n)-u(n-1))
#' Else u(n+1) = rho * u(n)
#'
#' @param lfresiduals the residuals to extrapolate
#' @param rho the autocorrelation parameter of the regression
#' @param n an integer, how many extrapolations to do.
#' @param include.differenciation a boolean, the same as submitted
#' to twoStepsBenchmark
#'
#' @return a numeric, the extrapolated sequence of residuals, to replace the NA of
#' the residuals
#' @keywords internal
#' @export
residuals_extrap <- function(lfresiduals,rho,include.differenciation) {
  valplaces <- which(!is.na(lfresiduals))
  if (length(valplaces) != 0) {
    firstval <- valplaces[1L]
    lastval <- valplaces[length(valplaces)]
    if (rho==0) rhoinverse <- 0 else rhoinverse <- 1/rho
    if (lastval != length(lfresiduals)) {
      lfresiduals[(lastval+1L):length(lfresiduals)] <-
        residuals_extrap_sequence(lfresiduals[lastval-1L],
                                  lfresiduals[lastval],
                                  rho,
                                  length(lfresiduals)-lastval,
                                  include.differenciation)
    }
    if (firstval != 1L) {
      lfresiduals[(firstval-1L):1L] <-
        residuals_extrap_sequence(lfresiduals[firstval+1L],
                                  lfresiduals[firstval],
                                  rhoinverse,
                                  firstval-1L,
                                  include.differenciation)
    }
  }
  lfresiduals
}

regression_estimation <- function(hfserie,lfserie,
                                  include.differenciation,include.rho,
                                  start.coeff.calc,end.coeff.calc,
                                  set_coefficients,cl) {
  y <- window(lfserie,start=start.coeff.calc,end=end.coeff.calc,extend = TRUE)
  
  x <- aggregate_and_crop_hf_to_lf(hfserie,y)
  
  praislm(x,y,include.rho,include.differenciation,set_coefficients,cl)
}

coefficients_application <- function(hfserie,lfserie,regcoefs) {
  
  tsp_extended <- extend_tsp(tsp(hfserie),frequency(lfserie))
  
  hfserie_win <- window(hfserie,start=tsp_extended[1L],end=tsp_extended[2L],extend = TRUE)
  
  ts_from_tsp(as.numeric(hfserie_win %*% regcoefs),tsp(hfserie_win))
  
}

eval_smoothed_part <- function(hfserie_fitted,lfserie,include.differenciation,rho,set.smoothed.part) {
  if (is.null(set.smoothed.part)) {
    hfserie_fitted_aggreg <- fast_aggregate(hfserie_fitted,nfrequency = frequency(lfserie))
    lfresiduals <- fast_op_on_x(
      hfserie_fitted_aggreg,
      lfserie,
      function(e1,e2) e2-e1)
    lfresiduals <- residuals_extrap(lfresiduals,rho,include.differenciation)
    bflSmooth(lfresiduals,frequency(hfserie_fitted))
  }
  else {
    if (!is.ts(set.smoothed.part) || is.mts(set.smoothed.part)) stop("set.smoothed part must be an univariate time-serie", call. = FALSE)
    set.smoothed.part
  }
}

#' @include s4register.R
twoStepsBenchmark_impl <- function(hfserie,lfserie,
                                   include.differenciation,include.rho,
                                   set_coefficients,
                                   start.coeff.calc,end.coeff.calc,
                                   start.benchmark,end.benchmark,
                                   start.domain,end.domain,
                                   maincl,cl=NULL,set.smoothed.part=NULL) {
  if (is.null(cl)) cl <- maincl
  
  regresults     <- regression_estimation(hfserie,lfserie,
                                          include.differenciation,include.rho,
                                          start.coeff.calc,end.coeff.calc,
                                          set_coefficients,cl)
  
  hfserie_cropped <- window(hfserie,start=start.domain,end=end.domain,extend=TRUE)
  
  lfserie_cropped <- window(lfserie,start=start.benchmark,end=end.benchmark,extend=TRUE)
  
  hfserie_fitted  <- coefficients_application(hfserie_cropped,lfserie_cropped,regresults$coefficients)
  
  smoothed_part   <- eval_smoothed_part(hfserie_fitted,lfserie_cropped,include.differenciation,regresults$rho,set.smoothed.part)
  
  rests <- fast_op_on_x(hfserie_fitted,
                        smoothed_part,
                        `+`)
  
  tsp_cropped <- tsp(hfserie_cropped)
  res <- list(benchmarked.serie = window(rests,start=tsp_cropped[1L],end=tsp_cropped[2L],extend = TRUE),
              fitted.values = window(hfserie_fitted,start=tsp_cropped[1L],end=tsp_cropped[2L],extend = TRUE),
              regression = regresults,
              smoothed.part = smoothed_part,
              model.list = c(list(hfserie = hfserie,
                                  lfserie = lfserie,
                                  include.rho = include.rho,
                                  include.differenciation = include.differenciation,
                                  set.coefficients = set_coefficients,
                                  start.coeff.calc = start.coeff.calc,
                                  end.coeff.calc = end.coeff.calc,
                                  start.benchmark = start.benchmark,
                                  end.benchmark = end.benchmark,
                                  start.domain = start.domain,
                                  end.domain = end.domain),
                             if (!is.null(set.smoothed.part)) list(set.smoothed.part=set.smoothed.part)),
              call = cl)
  
  new("twoStepsBenchmark",res)
}

#' @title Regress and bends a time-serie with a lower frequency one
#' 
#' @description twoStepsBenchmark bends a time-serie with a time-serie of a lower frequency.
#' The procedure involved is a Prais-Winsten regression, then an additive
#' Denton benchmark.
#' 
#' Therefore, the resulting time-serie is the sum of a regression fit and of a
#' smoothed part. The smoothed part minimizes the sum of squares of its
#' differences.
#' 
#' The resulting time-serie is equal to the low-frequency serie after aggregation
#' within the benchmark window.
#'
#' @details annualBenchmark is a wrapper of the main function, that applies more specifically
#' to annual series, and changes the default window parameters to the ones
#' that are commonly used by quarterly national accounts.
#'   
#' @aliases annualBenchmark
#' @usage
#' twoStepsBenchmark(hfserie,lfserie,include.differenciation=FALSE,include.rho=FALSE,
#'                   set.coeff=NULL,set.const=NULL,
#'                   start.coeff.calc=NULL,end.coeff.calc=NULL,
#'                   start.benchmark=NULL,end.benchmark=NULL,
#'                   start.domain=NULL,end.domain=NULL,outliers=NULL,
#'                   ...)
#'
#'
#' annualBenchmark(hfserie,lfserie,
#'                 include.differenciation=FALSE,include.rho=FALSE,
#'                 set.coeff=NULL,set.const=NULL,
#'                 start.coeff.calc=start(lfserie)[1L],
#'                 end.coeff.calc=end(lfserie)[1L],
#'                 start.benchmark=start(lfserie)[1L],
#'                 end.benchmark=end.coeff.calc[1L]+1L,
#'                 start.domain=start(hfserie),
#'                 end.domain=c(end.benchmark[1L]+2L,frequency(hfserie)),
#'                 outliers=NULL)
#' 
#' @param hfserie the bended time-serie. It can be a matrix time-serie.
#' @param lfserie a time-serie whose frequency divides the frequency of `hfserie`.
#' @param include.differenciation a boolean of length 1. If `TRUE`, `lfserie` and
#' `hfserie` are differenced before the estimation of the regression.
#' @param include.rho a boolean of length 1. If `TRUE`, the regression includes
#' an autocorrelation parameter for the residuals. The applied procedure is a
#' Prais-Winsten estimation.
#' @param set.coeff an optional numeric, that allows the user to set the
#' regression coefficients instead of evaluating them.
#' If hfserie is not a matrix, set.coeff can be an unnamed numeric of length 1.
#' Otherwise, `set.coeff` has to be a named numeric, which will set the
#' corresponding coefficients instead of evaluating them.
#' Each column name of hfserie and each outlier set with the `outlier` arg
#' initialize a coefficient with the same name, that can be set through set.coeff.
#' The default name for a non-matrix time-serie is then `"hfserie"`,
#' By example, a LS2003 and the time-serie can be set using
#' `set.coeff=c(hfserie=3,LS2003=1)`.
#' @param set.const an optional numeric of length 1, that sets the regression
#' constant.
#' The constant is actually an automatically added column to `hfserie`. Using
#' `set.constant=3` is equivalent to using `set.coeff=c(constant=3)`.
#' @param start.coeff.calc an optional start for the estimation of the
#' coefficients of the regression.
#' Should be a numeric of length 1 or 2, like a window for `lfserie`. If NULL,
#' the start is defined by lfserie's window.
#' @param end.coeff.calc an optional end for the estimation of the coefficients
#' of the regression.
#' Should be a numeric of length 1 or 2, like a window for `lfserie`. If NULL,
#' the end is defined by lfserie's window.
#' @param start.benchmark an optional start for `lfserie` to bend `hfserie`.
#' Should be a numeric of length 1 or 2, like a window for `lfserie`. If NULL,
#' the start is defined by lfserie's window.
#' @param end.benchmark an optional end for `lfserie` to bend `hfserie`.
#' Should be a numeric of length 1 or 2, like a window for `lfserie`. If NULL,
#' the start is defined by lfserie's window.
#' @param start.domain an optional for the output high-frequency serie. It also
#' defines the smoothing window :
#' The low-frequency residuals will be extrapolated until they contain the
#' smallest low-frequency window that is around the high-frequency domain
#' window.
#' Should be a numeric of length 1 or 2, like a window for `hfserie`. If NULL,
#' the start is defined by hfserie's window.
#' @param end.domain an optional end for the output high-frequency serie. It
#' also defines the smoothing window :
#' The low-frequency residuals will be extrapolated until they contain the
#' smallest low-frequency window that is around the high-frequency domain
#' window.
#' Should be a numeric of length 1 or 2, like a window for `hfserie`. If NULL,
#' the start is defined by hfserie's window.
#' @param outliers an optional named list of numeric vectors, whose pattern is
#' like `list(AO2008T2=c(0,0,3,2),LS2002=c(0.1,0.1,0.1,0.1))` where :
#' 
#' * `"AO"` stands for additive outlier or `"LS"` for level shift
#' * The integer that follows stands for the outlier starting year
#' * an optional integer, preceded by the letter T, stands for the low-frequency
#' cycle of the outlier start.
#' * The numeric vector values stands for the disaggregated value of the outlier
#' and must be a multiple of hf / lf
#' 
#' The outliers coefficients are evaluated though the regression process, like
#' any coefficient. Therefore, if any outlier is outside of the coefficient
#' calculation window, it should be fixed using `set.coeff`.
#' 
#' @param \dots if the dots contain a cl item, its value overwrites the value of
#' the returned call. This feature allows to build wrappers.
#' @return
#' twoStepsBenchark returns an object of class "`twoStepsBenchmark`".
#' 
#' The function `summary` can be used to obtain and print a summary of the
#' regression used by the benchmark.
#' The functions `plot` and `autoplot` (the generic from \pkg{ggplot2}) produce
#' graphics of the benchmarked serie and the bending serie.
#' The functions \link{in_disaggr}, \link{in_revisions}, \link{in_scatter}
#' produce comparisons on which plot and autoplot can also be used.
#' 
#' The generic accessor functions `as.ts`, `prais`, `coefficients`, `residuals`,
#' `fitted.values`, `model.list`, `se`, `rho` extract various useful features of
#' the returned value.
#' 
#' An object of class "`twoStepsBenchmark`" is a list containing the following
#' components :
#'   \item{benchmarked.serie}{a time-serie, that is the result of the
#'   benchmark. It is equal to `fitted.values + smoothed.part`.}
#'   \item{fitted.values}{a time-serie, that is the high-frequency serie as it
#'   is after having applied the regression coefficients. Compared to the fitted
#'   values of the regression, which can be retrieved inside the regression
#'   component, it has a high-frequency time-serie and can eventually be
#'   integrated if `include.differenciation` is `TRUE`.}
#'   \item{regression}{an object of class praislm, it is the regression on which
#'   relies the benchmark. It can be extracted with the function \link{prais}}
#'   \item{smoothed.part}{the smoothed part of the two-steps benchmark. It is
#'   the smoothed difference between the `fitted.values` and lfserie.}
#'   \item{model.list}{a list containing all the arguments submitted to the
#'   function.}
#'   \item{call}{the matched call (either of twoStepsBenchmark or
#'   annualBenchmark)}
#'   
#' @examples
#' 
#' ## How to use annualBenchmark or twoStepsBenchark
#' 
#' benchmark <- twoStepsBenchmark(hfserie = turnover,
#'                                lfserie = construction,
#'                                include.differenciation = TRUE)
#' as.ts(benchmark)
#' coef(benchmark)
#' summary(benchmark)
#' library(ggplot2)
#' autoplot(in_sample(benchmark))
#' 
#' ## How to manually set the coefficient
#' 
#' benchmark2 <- twoStepsBenchmark(hfserie = turnover,
#'                                 lfserie = construction,
#'                                 include.differenciation = TRUE,
#'                                 set.coeff = 0.1)
#' coef(benchmark2)
#'
#' @export
twoStepsBenchmark <- function(hfserie,lfserie,
                              include.differenciation=FALSE,include.rho=FALSE,
                              set.coeff=NULL,set.const=NULL,
                              start.coeff.calc=NULL,end.coeff.calc=NULL,
                              start.benchmark=NULL,end.benchmark=NULL,
                              start.domain=NULL,end.domain=NULL,
                              outliers=NULL,...) {
  
  if ( !is.ts(lfserie) || !is.ts(hfserie) ) stop("Not a ts object",
                                                 call. = FALSE)
  
  if (is.matrix(hfserie) &&
      is.null(colnames(hfserie)) &&
      ncol(hfserie) != 1L) stop("The high-frequency mts must have column names", call. = FALSE)
  
  hfserie <- clean_tsp(hfserie)
  lfserie <- clean_tsp(lfserie)
  
  tsplf <- tsp(lfserie)
  tsphf <- tsp(hfserie)
  
  if (tsphf[3L] %% tsplf[3L] != 0L) stop("The low frequency should divide the higher one", call. = FALSE)
  if (!is.null(dim(lfserie)) && dim(lfserie)[2L] != 1) stop("The low frequency serie must be one-dimensional", call. = FALSE)
  
  maincl <- match.call()
  
  if (is.null(set.coeff)) set.coeff <- numeric()
  if (is.null(set.const)) set.const <- numeric()
  
  constant <- {
    if (include.differenciation) 1L:NROW(hfserie)*(frequency(lfserie)/frequency(hfserie))^2
    else                         rep(frequency(lfserie)/frequency(hfserie),NROW(hfserie))
  }
  
  if (length(set.const) > 1L) stop("set.const must be of a single value", call. = FALSE)
  if (length(set.const) == 1L) names(set.const) <- "constant"
  
  if ((NCOL(hfserie) == 1L) &&
      length(set.coeff) == 1L &&
      is.null(names(set.coeff)))
    names(set.coeff) <- "hfserie"
  
  outliers_mts <- interpret_outliers(outliers,frequency(lfserie),hfserie)
  
  hfserie <- ts(matrix(c(constant,hfserie,outliers_mts),
                       nrow = NROW(hfserie)),
                start = tsphf[1L],
                frequency = tsphf[3L],
                names = c("constant",
                          if (is.null(colnames(hfserie))) "hfserie" else colnames(hfserie),
                          colnames(outliers_mts)))
  
  if (anyDuplicated(colnames(hfserie))) stop("Invalid colnames for hfserie",
                                             call. = FALSE)
  
  res <- twoStepsBenchmark_impl(hfserie,lfserie,
                                include.differenciation,include.rho,
                                c(set.const,set.coeff),
                                start.coeff.calc,end.coeff.calc,
                                start.benchmark,end.benchmark,
                                start.domain,end.domain,maincl,...)
  if (!is.null(outliers)) {
    attr(res$model.list,"outliers") <- outliers
    attr(res$regression$model.list,"outliers") <- outliers
  }
  
  res
}

#' @export
annualBenchmark <- function(hfserie,lfserie,
                            include.differenciation=FALSE,include.rho=FALSE,
                            set.coeff=NULL,set.const=NULL,
                            start.coeff.calc=start(lfserie)[1L],
                            end.coeff.calc=end(lfserie)[1L],
                            start.benchmark=start(lfserie)[1L],
                            end.benchmark=end.coeff.calc[1L]+1L,
                            start.domain=start(hfserie),
                            end.domain=c(end.benchmark[1L]+2L,frequency(hfserie)),
                            outliers=NULL) {
  
  if (frequency(lfserie) != 1) stop("Not an annual time-serie", call. = FALSE)
  twoStepsBenchmark(hfserie,lfserie,
                    include.differenciation,include.rho,
                    set.coeff,set.const,
                    start.coeff.calc,end.coeff.calc,
                    start.benchmark,end.benchmark,
                    start.domain,end.domain,outliers,
                    cl=match.call())
}

#' Using an estimated benchmark model on another time-serie
#' 
#' This function reapplies the coefficients and parameters of a benchmark on new
#' time-serie.
#'
#' `reUseBenchmark` is primarily meant to be used on a serie that is derived
#' from the previous one, after some modifications that would bias the
#' estimation otherwise. Working-day adjustment is a good example. Hence, by
#' default, the smoothed part of the first model isn't reevaluated ; the
#' aggregated benchmarked serie isn't equal to the low-frequency serie.
#' 
#' @usage
#' reUseBenchmark(hfserie,benchmark,reeval.smoothed.part=FALSE)
#' 
#' @param hfserie the bended time-serie. If it is a matrix time-serie, it has to
#' have the same column names than the `hfserie` used for the benchmark.
#' @param benchmark a twoStepsBenchmark object, from which the parameters and
#' coefficients are taken.
#' @param reeval.smoothed.part a boolean of length 1. If `TRUE`, the smoothed
#' part is reevaluated, hence the aggregated benchmarked serie is equal to the
#' low-frequency serie.
#' @return `reUseBenchmark` returns an object of class \link{twoStepsBenchmark}.
#' @examples 
#' benchmark <- twoStepsBenchmark(turnover,construction) 
#' turnover_modif <- turnover
#' turnover_modif[2] <- turnover[2]+2
#' benchmark2 <- reUseBenchmark(turnover_modif,benchmark)
#' @export
reUseBenchmark <- function(hfserie,benchmark,reeval.smoothed.part=FALSE) {
  m <- model.list(benchmark)
  
  coefs <- coef(benchmark)
  set.const <- coefs["constant"]
  set.coeff <- coefs[names(coefs) != "constant"]
  
  twoStepsBenchmark(hfserie,m$lfserie,
                    m$include.differenciation,m$include.rho,
                    set.coeff,set.const,
                    m$start.coeff.calc,m$end.coeff.calc,
                    m$start.benchmark,m$end.benchmark,
                    m$start.domain,m$end.domain,
                    outliers = outliers(benchmark),cl=match.call(),
                    set.smoothed.part = if (!reeval.smoothed.part) smoothed.part(benchmark))
}
