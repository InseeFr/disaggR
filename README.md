
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R build
status](https://github.com/arnaud-feldmann/disaggR/workflows/R-CMD-check/badge.svg)](https://github.com/arnaud-feldmann/disaggR/actions)

[![R build
status](https://github.com/arnaud-feldmann/disaggR/workflows/test-coverage/badge.svg)](https://github.com/arnaud-feldmann/disaggR/actions)

<!-- badges: end -->

## Overview

The R package disaggR is an implementation of the French Quarterly
National Accounts method for temporal disaggregation of time-series.
`twoStepsBenchmark()` bends a time-série with another.

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

benchmark <- annualBenchmark(hfserie = turnover,
                            lfserie = construction,
                            include.differenciation = TRUE)
as.ts(benchmark)
coef(benchmark)
summary(benchmark)
autoplot(benchmark)
autoplot(in_sample(benchmark))

## How to manually set the coefficient

benchmark <- annualBenchmark(hfserie = turnover,
                             lfserie = construction,
                             include.differenciation = TRUE,
                             set.coeff = 0.1)
coef(benchmark)
```
