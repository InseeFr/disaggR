# Extracting the regression of a twoStepsBenchmark

prais extracts the regression, which is an object of class `"praislm"`,
of a twoStepsBenchmark object.

## Usage

``` r
prais(x)

praislm(X, y, include.rho, include.differenciation, set_coefficients, cl)
```

## Arguments

- x:

  a twoStepsBenchmark

## Value

prais returns an object of class `"praislm"`.

The functions that can be used on that class are almost the same than
for the class `twoStepsBenchmark`. `summary`, `coefficients`,
`residuals` will return the same values. However, as for
`fitted.values`, the accessor returns the fitted values of the
regression, not the high-frequency, eventually integrated, time series
contained in a twoStepsBenchmark.

An object of class `"praislm"` is a list containing the following
components :

- coefficients:

  a named vector of coefficients.

- residuals:

  the residuals, that is response minus fitted values.

- fitted.values:

  a time series, the fitted mean values

- se:

  a named vector of standard errors.

- df.residuals:

  the residual degrees of freedom.

- rho:

  the autocorrelation coefficients of the residuals. It is equal to zero
  if twoStepsBenchmark was called with `include.rho=FALSE`

- residuals.decorrelated:

  the residuals of the model after having been transformed by rho in a
  least square model.

- fitted.values.decorrelated:

  the fitted values of the model after having been transformed by rho in
  a least square model.

## Examples

``` r
benchmark <- twoStepsBenchmark(turnover,construction); prais(benchmark)
#> 
#> Call:
#> twoStepsBenchmark(hfserie = turnover, lfserie = construction)
#> 
#> Coefficients:
#> constant   hfserie  
#>   44.282     0.141  
#> 
```
