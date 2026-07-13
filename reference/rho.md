# Extracting the autocorrelation parameter

The function `rho` returns the autocorrelation parameter from either a
[praislm](https://inseefr.github.io/disaggR/reference/prais.md) or a
[twoStepsBenchmark](https://inseefr.github.io/disaggR/reference/twoStepsBenchmark.md)
object. If `include.rho` is `FALSE`, `rho` returns zero.

## Usage

``` r
rho(object)
```

## Arguments

- object:

  a praislm or twoStepsBenchmark object.

## Value

a double of length 1.

## Examples

``` r
benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE); rho(benchmark)
#> [1] 0.8450681
```
