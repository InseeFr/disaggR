# Smooth a time series

bflSmooth smoothes a time series into a time series of a higher
frequency that exactly aggregates into the higher one. The process
followed is Boot, Feibes and Lisman, which minimizes the squares of the
variations.

## Usage

``` r
bflSmooth(lfserie, nfrequency, weights = NULL, lfserie.is.rate = FALSE)
```

## Arguments

- lfserie:

  a time series to be smoothed

- nfrequency:

  the new high frequency. It must be a multiple of the low frequency.

- weights:

  NULL or a time series of the same size than the expected
  high-frequency serie.

- lfserie.is.rate:

  TRUE or FALSE. Only taken into account if weights isn't NULL.

## Value

A time series of frequency nfrequency

## Details

If `weights` isn't `NULL` the results depends of `lfserie.is.rate` :

- if `FALSE` the rate output/weights is smoothed with the constraint
  that the aggregated output is equal to the input lfserie.

- if `TRUE` the input lfserie is the rate to be smoothed, with the
  constraint that the low-frequency weighted means of the output are
  equal to lfserie.
