# disaggR 0.1.9
* fixed the R check for old-release (3.6.3).

# disaggR 0.1.8
* fixed the R check for Solaris.

# disaggR 0.1.7
* various optimizations including cache for bflSmooth, which is now much faster, and alternative internal methods for time-series
* added a weights arg to bflSmooth, that reproduces the *lissage en taux* methodology
* the praislm and twoStepsBenchmark summaries now print some disclaimer to tell if the regression includes a differenciation