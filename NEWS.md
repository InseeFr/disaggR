# disaggR 0.2.0
* reView : a shiny reviewing application for twoStepsBenchmarks.
* rePort : a rmarkdown html report for twoStepsBenchmark and reView outputs.
* start.domain and end.domain know crops the hfserie *after* having calculated the coefficients. That way, it is possible to evaluate the coefficients on a full hfserie, cropping them for the application.
* *in_sample* now generates a more general class *tscomparison*, with a "in_sample" func attribute. In previous versions,
the S3 class was named "insample".
* the new functions *in_scatter*, *in_benchmark* and *in_revisions* also produce tscomparisons, with plot and autoplot methods.
* the graphics are more beautiful thanks to the package scales.
* the plot and autoplot methods now have xlab, ylab, start, end, col, lty, show.legend, main and mar arguments. The autoplot methods have also a theme argument.
* removed the c++ code to improve readability
* the default argument end.coeff.calc, of annualBenchmark, changed to remove the last annual value from the coefficient calculation

# disaggR 0.1.9
* fixed the R check for old-release (3.6.3).

# disaggR 0.1.8
* fixed the R check for Solaris.

# disaggR 0.1.7
* various optimizations including cache for bflSmooth, which is now much faster, and alternative internal methods for time-series
* added a weights arg to bflSmooth, that reproduces the *lissage en taux* methodology
* the praislm and twoStepsBenchmark summaries now print some disclaimer to tell if the regression includes a differenciation