# Comparing a disaggregation with the high-frequency input

The function `in_disaggr` takes a
[twoStepsBenchmark](https://inseefr.github.io/disaggR/reference/twoStepsBenchmark.md)
or a
[threeRuleSmooth](https://inseefr.github.io/disaggR/reference/threeRuleSmooth.md)
object as an input. It produces a comparison between the benchmarked
time series and the high-frequency input.

## Usage

``` r
in_disaggr(object, type = "changes")
```

## Arguments

- object:

  an object of class `"twoStepsBenchmark"` or `"threeRuleSmooth"`.

- type:

  `"levels"`,`"levels-rebased"`, `"changes"` or `"contributions"`. This
  defines the type of output.

## Value

a named matrix time series of two columns, one for the response and the
other for the input. A `tscomparison` class is added to the object.

## Details

The functions `plot` and `autoplot` can be used on this object to
produce graphics.

## See also

[in_sample](https://inseefr.github.io/disaggR/reference/in_sample.md)
[in_revisions](https://inseefr.github.io/disaggR/reference/in_revisions.md)
[in_scatter](https://inseefr.github.io/disaggR/reference/in_scatter.md)
[plot.tscomparison](https://inseefr.github.io/disaggR/reference/plot.tscomparison.md)

## Examples

``` r
benchmark <- twoStepsBenchmark(turnover,construction,include.rho = TRUE)
plot(in_disaggr(benchmark))
```
