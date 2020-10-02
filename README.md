
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R build
status](https://github.com/arnaud-feldmann/disaggR/workflows/R-CMD-check/badge.svg)](https://github.com/arnaud-feldmann/disaggR/actions)
[![codecov](https://codecov.io/gh/arnaud-feldmann/disaggR/branch/master/graph/badge.svg)](https://codecov.io/gh/arnaud-feldmann/disaggR)
[![CRAN
status](https://www.r-pkg.org/badges/version/disaggR)](https://cran.r-project.org/package=disaggR)
[![Downloads](https://cranlogs.r-pkg.org/badges/disaggR)](https://cran.r-project.org/package=disaggR)

<!-- badges: end -->

## Overview

The R package disaggR is an implementation of the French Quarterly
National Accounts method for temporal disaggregation of time-series.
`twoStepsBenchmark()` bends a time-serie with another one of a lower
frequency.

## Installation

You can install the **stable** version from
[CRAN](https://cran.r-project.org/package=disaggR).

``` r
install.packages('disaggR', dependencies = TRUE)
```

You can install the **development** version from
[Github](https://github.com/arnaud-feldmann/disaggR).

``` r
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

<img src="man/figures/README-unnamed-chunk-4-1.png" width="50%" /><img src="man/figures/README-unnamed-chunk-4-2.png" width="50%" />
