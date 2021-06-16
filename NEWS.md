# disaggR 1.0
* added support of outliers.
* In `twoStepsBenchmark`, the set.coeff names used to be replaced by `"hfserie"` if `NCOL(hfserie) == 1L` and `length(set.coeff) == 1L`. This behavior was contradictory with the documentation if `set.coeff=c(constant=1L)`. As for now, set.coeff names will never be replaced. Then, it makes the controls stricter because `set.coeff=c(x.name.herited.from.anywhere=1)` will lead to an error.
* as for the `in` time-series plots, the y window now ignores the infinite values.

# disaggR 0.2.1
* fixed some issues with ts.eps-delayed tsps.

# disaggR 0.2
* reView : a shiny reviewing application for twoStepsBenchmarks.
* rePort : a rmarkdown html report for twoStepsBenchmark and reView outputs.
* start.domain and end.domain know crops the hfserie *after* having calculated the coefficients. That way, it is possible to evaluate the coefficients on a full hfserie, cropping them for the application.
* *in_sample* now generates a more general class *tscomparison*, with a "in_sample" func attribute. In previous versions,
the S3 class was named "insample".
* the new functions *in_scatter*, *in_benchmark* and *in_revisions* also produce tscomparisons, with plot and autoplot methods.
* the graphics are prettier thanks to the package scales.
* the plot and autoplot methods now have xlab, ylab, start, end, col, lty, show.legend, main and mar arguments. The autoplot methods have also a theme argument.
* removed the c++ code to improve readability
* most of the stats methods for time-series now coerce twoStepsBenchmarks or rateSmooths into time-series
* *reUseBenchmark* now induces a "set.smoothed.part" element in *model.list* if reeval.smoothed.part is
`FALSE`.
* *threeRuleSmooth* makes it easier than bflSmooth to procede to a rate smooth.

# disaggR 0.1.7
* various optimizations including cache for bflSmooth, which is now much faster, and alternative internal methods for time-series
* added a weights arg to bflSmooth, that reproduces the *lissage en taux* methodology
* the praislm and twoStepsBenchmark summaries now print some disclaimer to tell if the regression includes a differenciation
