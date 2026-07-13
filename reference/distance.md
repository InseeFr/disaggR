# Distance computation for disaggregations

This function `distance` computes the Minkowski distance of exponent p,
related to a tscomparison object, produced with `in_sample`,
`in_disaggr` or `in_revisions`

## Usage

``` r
distance(x, p = 2)
```

## Arguments

- x:

  an object of class `tscomparison`

- p:

  an integer greater than 1L, or Inf.

## Value

a numeric of length 1, the distance.

## Details

The meaning depends on the tscomparison function :

- `in_sample` will produce the low-frequency distance between the
  predicted value and the response, on the coefficient calculation
  window.

- `in_disaggr` will produce the high-frequency distance between the
  inputs (eventually, the sum of its contributions) and the benchmarked
  series.

- `in_revisions` will produce the high-frequency distance between the
  two benchmarked series (contributions distance isn't permitted).

## See also

in_sample in_disaggr in_revisions

## Examples

``` r
benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
distance(in_sample(benchmark,type="changes"))
#> [1] 1.554383
distance(in_disaggr(benchmark,type="contributions"),p=1L)
#> [1] 0.116331
distance(in_disaggr(benchmark,type="changes"),p=Inf)
#> [1] 1.936705
```
