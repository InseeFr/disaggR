# Extrapolation function for the residuals in a twoStepsBenchmark

This function is the rule to extrapolate the low-frequency residuals. If
include.differenciation is `TRUE`, u(n+1)-u(n) = rho\*(u(n)-u(n-1)) Else
u(n+1) = rho \* u(n)

## Usage

``` r
residuals_extrap(lfresiduals, rho, include.differenciation)
```

## Arguments

- lfresiduals:

  the residuals to extrapolate

- rho:

  the autocorrelation parameter of the regression

- include.differenciation:

  a boolean, the same as submitted to twoStepsBenchmark

## Value

a numeric, the extrapolated sequence of residuals, to replace the NA of
the residuals
