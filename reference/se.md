# Extracting the standard error

The function `se` returns the standard error of the coefficients from
either a [praislm](https://inseefr.github.io/disaggR/reference/prais.md)
or a
[twoStepsBenchmark](https://inseefr.github.io/disaggR/reference/twoStepsBenchmark.md)
object.

## Usage

``` r
se(object)
```

## Arguments

- object:

  a praislm or twoStepsBenchmark object.

## Value

a numeric, that is named the same way that the coefficients are. If some
coefficients are set by the user, they return `NA` as for their standard
error.
