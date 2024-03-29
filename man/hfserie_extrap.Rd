% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/threeRuleSmooth.R
\name{hfserie_extrap}
\alias{hfserie_extrap}
\title{Extrapolation function for the hfserie in a threeRuleSmooth}
\usage{
hfserie_extrap(hfserie, lffreq)
}
\arguments{
\item{hfserie}{a time series, the high frequency series to extrapolate}

\item{lffreq}{an integer of length 1. The low frequency}
}
\value{
a time series, the extrapolated hfserie
}
\description{
This function replaces the incomplete low frequency cycles, at the start and
the end of the hfserie, with respectively the first and the last complete
cycles.
It may seem very raw, but it's only used for the weights in \code{bflSmooth}, in
order to get the high-frequency rate.
}
\keyword{internal}
