
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R build
status](https://github.com/arnaud-feldmann/disaggR/workflows/R-CMD-check/badge.svg)](https://github.com/arnaud-feldmann/disaggR/actions)
[![codecov](https://codecov.io/gh/arnaud-feldmann/disaggR/branch/master/graph/badge.svg)](https://codecov.io/gh/arnaud-feldmann/disaggR)

<!-- badges: end -->

## Overview

The R package disaggR is an implementation of the French Quarterly
National Accounts method for temporal disaggregation of time-series.
`twoStepsBenchmark()` bends a time-serie with another one of a lower
frequency.

## Installation

As for now, only the development version is available, from Github.

``` r
# Development version
# install.packages("devtools")
install_github("arnaud-feldmann/disaggR")
```

## Usage

``` r
library(disaggR)
library(ggplot2)

benchmark <- twoStepsBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = TRUE)
as.ts(benchmark)
coef(benchmark)
summary(benchmark)
autoplot(benchmark)
autoplot(in_sample(benchmark))
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="50%" /><img src="man/figures/README-unnamed-chunk-3-2.png" width="50%" />
