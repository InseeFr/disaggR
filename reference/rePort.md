# Producing a report

This function takes an output of the
[reView](https://inseefr.github.io/disaggR/reference/reView.md) shiny
application and produces an html report with the same outputs than in
shiny.

## Usage

``` r
rePort(
  object,
  output_file = NULL,
  launch.browser = if (is.null(output_file)) TRUE else FALSE,
  hfserie_name = NULL,
  lfserie_name = NULL,
  ...
)
```

## Arguments

- object:

  a twoStepsBenchmark with an univariate hfserie, a reViewOutput, or a
  character of length 1 with the path of their RDS file. If a
  reViewOutput is chosen, the former new benchmark is taken as the old
  one.

- output_file:

  The file in which the html should be saved. If `NULL` the file is
  temporary, and opened in a tab of the default browser.

- launch.browser:

  `TRUE` or `FALSE`. If TRUE, the output is opened in the browser.
  Defaults to TRUE if output_file is NULL.

- hfserie_name:

  a language object or a character of length 1. The name of the hfserie,
  eventually its expression.

- lfserie_name:

  a language object or a character of length 1. The name of the lfserie,
  eventually its expression.

- ...:

  other arguments passed to rmarkdown::render

## Details

It can also directly take a
[twoStepsBenchmark](https://inseefr.github.io/disaggR/reference/twoStepsBenchmark.md)
as an input.

## See also

reView
