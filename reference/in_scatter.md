# Comparing the inputs of a praislm regression

The function `in_scatter` returns low-frequency comparisons of the
inputs from a
[praislm](https://inseefr.github.io/disaggR/reference/prais.md), a
[twoStepsBenchmark](https://inseefr.github.io/disaggR/reference/twoStepsBenchmark.md)
or
[threeRuleSmooth](https://inseefr.github.io/disaggR/reference/threeRuleSmooth.md).

## Usage

``` r
in_scatter(
  object,
  type = if (model.list(object)$include.differenciation) "changes" else "levels"
)
```

## Arguments

- object:

  an object of class `"praislm"`, `"twoStepsBenchmark"` or
  `"threeRuleSmooth"`.

- type:

  `"levels"` or `"changes"`. This defines the type of output. A
  differencied model can't have a scatterplot in levels.

## Value

a named matrix time series of two or three columns, one for the
low-frequency serie and the others for the high-frequency series
(eventually differentiated if `include.differenciation` is `TRUE`). A
`tscomparison` class is added to the object. For a `twoStepsBenchmark`
object, this matrix has three columns, for the low-frequency series, the
high-frequency on the regression span and the high-frequency series on
the benchmark span.

If outlier effects are estimated, the contributions of the outliers are
substracted from the low-frequency series.

## Details

The functions `plot` and `autoplot` can be used on this object to
produce graphics.

## See also

[in_sample](https://inseefr.github.io/disaggR/reference/in_sample.md)
[in_disaggr](https://inseefr.github.io/disaggR/reference/in_disaggr.md)
[in_revisions](https://inseefr.github.io/disaggR/reference/in_revisions.md)
[plot.tscomparison](https://inseefr.github.io/disaggR/reference/plot.tscomparison.md)

## Examples

``` r
benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
plot(in_scatter(benchmark))
```
