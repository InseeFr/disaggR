# Generating a clone for bflSmooth_matrices_impl

This *function factory* returns a clone of bflSmooth_matrices_impl that
gives the same results than the original function but uses cache to
store computed matrices, which is useful when making a large number of
similar calls, like calls to
[`threeRuleSmooth()`](https://inseefr.github.io/disaggR/reference/threeRuleSmooth.md)
with the same hfserie but different lfserie, or to
[`twoStepsBenchmark()`](https://inseefr.github.io/disaggR/reference/twoStepsBenchmark.md)
with different lfserie of the same length.

## Usage

``` r
bflSmooth_matrices_factory(cache_size = 100L)
```

## Details

bflSmooth_matrices_factory is only run at build time.
