# A shiny app to reView and modify twoStepsBenchmarks

reView allows the user to easily access diverse outputs in order to
review a benchmark object, made with
[twoStepsBenchmark](https://inseefr.github.io/disaggR/reference/twoStepsBenchmark.md).

The `hfserie_name` and `lfserie_name` define :

## Usage

``` r
reView(object, hfserie_name = NULL, lfserie_name = NULL, compare = TRUE)
```

## Arguments

- object:

  a twoStepsBenchmark with an univariate hfserie, a reViewOutput, or a
  character of length 1 with the path of their RDS file. If a
  reViewOutput is chosen, the former new benchmark is taken as the old
  one.

- hfserie_name:

  a language object or a character of length 1. The name of the hfserie,
  eventually its expression.

- lfserie_name:

  a language object or a character of length 1. The name of the lfserie,
  eventually its expression.

- compare:

  a boolean of length 1, that tells if the outputs of the old benchmark
  should be displayed.

## Value

a list, of class reViewOutput, containing the new benchmark, the old
one, the names of the series and the boolean compare. This object can
also be saved in RDS format through the app. The reViewOutput object can
be displayed as a html report with the same informations than in shiny,
with the [rePort](https://inseefr.github.io/disaggR/reference/rePort.md)
method.

## Details

- the default file name of the RDS file

- the names of the series in the output `call` element

By default, these are set as defined in their `call` element.

The app is made of shiny modules in order to make it easy to integrate
it into a wider application. In the module part, every input are defined
as reactive variables.

## See also

[rePort](https://inseefr.github.io/disaggR/reference/rePort.md)

## Examples

``` r
if (FALSE) { # \dontrun{
reView(twoStepsBenchmark(turnover,construction))
} # }
```
