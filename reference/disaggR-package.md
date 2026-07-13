# Two-Steps Benchmarks for Time Series Disaggregation

The
[`twoStepsBenchmark()`](https://inseefr.github.io/disaggR/reference/twoStepsBenchmark.md)
and
[`threeRuleSmooth()`](https://inseefr.github.io/disaggR/reference/threeRuleSmooth.md)
functions allow you to disaggregate a low-frequency time series with
higher frequency time series, using the French National Accounts
methodology. The aggregated sum of the resulting time series is strictly
equal to the low-frequency series within the benchmarking window.
Typically, the low-frequency series is an annual one, unknown for the
last year, and the high frequency one is either quarterly or monthly.

See "Methodology of quarterly national accounts", Insee Méthodes N°126,
by Insee (2012, ISBN:978-2-11-068613-8,
https://www.insee.fr/en/information/2579410).

## See also

Useful links:

- <https://inseefr.github.io/disaggR/>

- Report bugs at <https://github.com/InseeFr/disaggR/issues>

## Author

**Maintainer**: Pauline Meinzel <pauline.meinzel@insee.fr>

Authors:

- Arnaud Feldmann <arnaud.feldmann@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-0109-7505)) (Author and maintener
  of the package until the version 1.0.1)

Other contributors:

- Thomas Laurent (Maintener of the package from 1.0.2 to 1.0.5.2)
  \[contributor\]

- Franck Arnaud (barplot base graphics method for the mts class)
  \[contributor\]

- Institut national de la statistique et des Ã©tudes Ã©conomiques
  <comptes-trimestriels@insee.fr> (https://www.insee.fr/) \[copyright
  holder\]
