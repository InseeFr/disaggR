# disaggR 0.1.7
* various optimizations including cache for bflSmooth, which is now *very fast*, and alternative internal methods for time-series
* added a `weights` arg to bflSmooth, that reproduces the *lissage en taux* methodology
* the praislm and twoStepsBenchmark summaries now print if the regression includes a differenciation