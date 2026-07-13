# Extracting the standard error

The function `outliers` returns the outliers from either a
[praislm](https://inseefr.github.io/disaggR/reference/prais.md) or a
[twoStepsBenchmark](https://inseefr.github.io/disaggR/reference/twoStepsBenchmark.md)
object.

## Usage

``` r
outliers(object, as.ts = FALSE)
```

## Arguments

- object:

  a praislm or twoStepsBenchmark object.

- as.ts:

  a boolean of length 1. If `TRUE`, the returned outliers are returned
  as a time series with (dim and colnames). If `FALSE`, the returned
  outliers is the named list that was submitted as a function argument.

## Value

a named list or a time series, depending of the argument `"as.ts"`.
