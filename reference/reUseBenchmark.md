# Using an estimated benchmark model on another time series

This function reapplies the coefficients and parameters of a benchmark
on new time series.

## Usage

``` r
reUseBenchmark(hfserie,benchmark,reeval.smoothed.part=FALSE)
```

## Arguments

- hfserie:

  the bended time series. If it is a matrix time series, it has to have
  the same column names than the `hfserie` used for the benchmark.

- benchmark:

  a twoStepsBenchmark object, from which the parameters and coefficients
  are taken.

- reeval.smoothed.part:

  a boolean of length 1. If `TRUE`, the smoothed part is reevaluated,
  hence the aggregated benchmarked series is equal to the low-frequency
  series.

## Value

`reUseBenchmark` returns an object of class
[twoStepsBenchmark](https://inseefr.github.io/disaggR/reference/twoStepsBenchmark.md).

## Details

`reUseBenchmark` is primarily meant to be used on a series that is
derived from the previous one, after some modifications that would bias
the estimation otherwise. Working-day adjustment is a good example.
Hence, by default, the smoothed part of the first model isn't
reevaluated ; the aggregated benchmarked series isn't equal to the
low-frequency series.

## Examples

``` r
benchmark <- twoStepsBenchmark(turnover,construction) 
turnover_modif <- turnover
turnover_modif[2] <- turnover[2]+2
benchmark2 <- reUseBenchmark(turnover_modif,benchmark)
```
