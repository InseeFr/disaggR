# Extrapolation function for the hfserie in a threeRuleSmooth

This function replaces the incomplete low frequency cycles, at the start
and the end of the hfserie, with respectively the first and the last
complete cycles. It may seem very raw, but it's only used for the
weights in `bflSmooth`, in order to get the high-frequency rate.

## Usage

``` r
hfserie_extrap(hfserie, lffreq)
```

## Arguments

- hfserie:

  a time series, the high frequency series to extrapolate

- lffreq:

  an integer of length 1. The low frequency

## Value

a time series, the extrapolated hfserie
