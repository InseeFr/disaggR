# Comparing two disaggregations together

The function `in_revisions`takes two inputs,
[twoStepsBenchmark](https://inseefr.github.io/disaggR/reference/twoStepsBenchmark.md)
or a
[threeRuleSmooth](https://inseefr.github.io/disaggR/reference/threeRuleSmooth.md),
and produces a comparison between those.

## Usage

``` r
in_revisions(object, object_old, type = "changes")
```

## Arguments

- object:

  an object of class `"twoStepsBenchmark"` or `"threeRuleSmooth"`.

- object_old:

  an object of class `"twoStepsBenchmark"` or `"threeRuleSmooth"`.

- type:

  `"levels"`,`"levels-rebased"`, `"changes"` or `"contributions"`. This
  defines the type of output.

## Value

a named matrix time series of two columns, one for the response and the
other for the predicted value. A `tscomparison` class is added to the
object.

## Details

The functions `plot` and `autoplot` can be used on this object to
produce graphics.

## See also

[in_sample](https://inseefr.github.io/disaggR/reference/in_sample.md)
[in_disaggr](https://inseefr.github.io/disaggR/reference/in_disaggr.md)
[in_scatter](https://inseefr.github.io/disaggR/reference/in_scatter.md)
[plot.tscomparison](https://inseefr.github.io/disaggR/reference/plot.tscomparison.md)

## Examples

``` r
benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
benchmark2 <- twoStepsBenchmark(turnover,construction,include.differenciation = TRUE)
plot(in_revisions(benchmark,benchmark2))
```
