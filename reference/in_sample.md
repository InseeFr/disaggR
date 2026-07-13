# Producing the in sample predictions of a prais-lm regression

The function `in_sample` returns in-sample predictions from a
[praislm](https://inseefr.github.io/disaggR/reference/prais.md) or a
[twoStepsBenchmark](https://inseefr.github.io/disaggR/reference/twoStepsBenchmark.md)
object.

## Usage

``` r
in_sample(object, type = "changes")
```

## Arguments

- object:

  an object of class `"praislm"` or `"twoStepsBenchmark"`.

- type:

  `"changes"` or `"levels"`. The results are either returned in changes
  or in levels.

## Value

a named matrix time series of two columns, one for the response and the
other for the predicted value. A `"tscomparison"` class is added to the
object.

## Details

The functions `plot` and `autoplot` can be used on this object to
produce graphics.

The predicted values are different from the fitted values :

- they are eventually reintegrated.

- they contain the autocorrelated part of the residuals.

Besides, changes are relative to the latest benchmark value, not the
latest predicted value.

## See also

[in_disaggr](https://inseefr.github.io/disaggR/reference/in_disaggr.md)
[in_revisions](https://inseefr.github.io/disaggR/reference/in_revisions.md)
[in_scatter](https://inseefr.github.io/disaggR/reference/in_scatter.md)
[plot.tscomparison](https://inseefr.github.io/disaggR/reference/plot.tscomparison.md)

## Examples

``` r
benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
plot(in_sample(benchmark))
```
