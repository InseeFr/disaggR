
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R build
status](https://github.com/arnaud-feldmann/disaggR/workflows/R-CMD-check/badge.svg)](https://github.com/arnaud-feldmann/disaggR/actions)

[![R build
status](https://github.com/arnaud-feldmann/disaggR/workflows/test-coverage/badge.svg)](https://github.com/arnaud-feldmann/disaggR/actions)

<!-- badges: end -->

## Overview

The R package disaggR is an implementation of the French Quarterly
National Accounts method for temporal disaggregation of time-series.
`twoStepsBenchmark()` bends a time-série with another.

## Installation

As for now, only the development version is available, from Github.

``` r
# Development version
# install.packages("devtools")
install_github("arnaud-feldmann/disaggR")
```

## Usage

``` r
library(disaggR)
library(ggplot2)

benchmark <- annualBenchmark(hfserie = turnover,
                            lfserie = construction,
                            include.differenciation = TRUE)
as.ts(benchmark)
#>           Jan      Feb      Mar      Apr      May      Jun      Jul      Aug
#> 2000 11.17700 11.04207 11.30599 11.37970 11.47701 11.49781 11.72211 11.66105
#> 2001 11.89094 12.04555 11.99175 11.92219 11.97747 11.95641 11.75168 12.11955
#> 2002 12.04505 11.99360 12.01425 12.34021 12.28432 12.23926 12.22597 12.40743
#> 2003 12.54418 12.73492 12.74958 12.77800 12.61418 12.92972 12.98486 13.24296
#> 2004 13.73260 13.75309 13.74720 14.00883 13.99247 14.02048 14.17511 13.97745
#> 2005 14.53067 14.59171 14.68202 14.83262 14.82514 14.93149 15.24996 15.34505
#> 2006 15.76440 15.93940 16.15566 16.15555 16.50044 16.67241 16.81045 16.74473
#> 2007 17.61900 17.79354 17.87374 17.75804 18.19482 18.17083 18.12359 18.12016
#> 2008 18.92809 19.03223 19.44686 19.04125 18.80992 19.06958 19.01202 19.10261
#> 2009 18.04222 17.96939 17.87080 17.86250 17.60512 17.61975 17.66641 17.47091
#> 2010 17.32496 17.03446 17.00853 17.07836 17.31439 17.48615 17.60940 17.52062
#> 2011 18.05172 18.08003 18.22176 18.27064 18.42212 18.27264 18.74332 18.24732
#> 2012 18.45136 18.18350 18.48822 18.42778 18.51033 18.32792 18.64129 18.30657
#> 2013 18.34038 18.07477 17.66732 18.32748 18.30897 18.17121 18.44463 18.53804
#> 2014 17.92671 18.28864 18.24467 18.11979 17.82660 18.34486 17.89213 18.44459
#> 2015 17.52633 17.47514 17.45221 17.36974 17.03087 17.45992 17.36176 17.26746
#> 2016 17.26209 17.56020 17.24355 17.13983 17.54505 17.14967 17.19524 17.22284
#> 2017 17.76438 17.97544 18.43136 17.62099 18.76126 19.03718 18.59885 18.33180
#> 2018 19.13599 19.17992 18.48777 19.26319 18.71380 19.39700 18.99372 19.59311
#> 2019 19.84989 20.11050 20.58002 20.57662 20.55897 20.02924 20.05154 21.22012
#> 2020 20.55726 19.76227 18.79744 16.26776 14.92504       NA       NA       NA
#> 2021       NA       NA       NA       NA       NA       NA       NA       NA
#> 2022       NA       NA       NA       NA       NA       NA       NA       NA
#>           Sep      Oct      Nov      Dec
#> 2000 11.70245 11.73897 11.73440 11.96144
#> 2001 11.80969 12.15522 12.09540 12.18417
#> 2002 12.43825 12.55473 12.32781 12.42910
#> 2003 13.17520 13.33399 13.34131 13.47110
#> 2004 14.33491 14.46016 14.52502 14.57269
#> 2005 15.49772 15.48365 15.86170 15.56830
#> 2006 16.94023 16.87345 17.13330 17.50996
#> 2007 18.21930 18.35075 18.06131 18.41492
#> 2008 18.88726 18.94011 18.32882 18.40127
#> 2009 17.30719 17.41416 17.34217 17.62939
#> 2010 17.87613 17.76407 18.12541 17.95753
#> 2011 18.34994 18.31190 18.76082 18.76779
#> 2012 18.18362 18.33421 18.02311 18.02208
#> 2013 18.36018 18.25783 18.67344 18.93574
#> 2014 17.98186 17.88175 17.57220 17.77621
#> 2015 17.25067 17.38667 17.56695 17.35227
#> 2016 17.67258 17.54541 17.83535 17.62820
#> 2017 18.53875 18.55796 18.72386 19.25816
#> 2018 19.26608 19.82517 19.58827 19.85598
#> 2019 20.63372 20.48835 20.55241 20.44861
#> 2020       NA       NA       NA       NA
#> 2021       NA       NA       NA       NA
#> 2022       NA       NA       NA       NA
coef(benchmark)
#>   constant    hfserie 
#> -1.4082504  0.1682139
summary(benchmark)
#> 
#> Call:
#> annualBenchmark(hfserie = turnover, lfserie = construction, include.differenciation = TRUE)
#> 
#> Residuals:
#>    Min. 1st Qu.  Median 3rd Qu.    Max. 
#> -4.4490 -2.2050  0.1239  1.7760  4.4660 
#> 
#>          Estimate   StdErr t.value  p.value    
#> constant -1.40825  0.82581  -1.705    0.106    
#> hfserie   0.16821  0.01279  13.155 2.44e-10 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 2.746 on 17 degrees of freedom
#> Multiple R-squared: 0.9369,  Adjusted R-squared: 0.9295
#> 
#> Portmanteau:
#>   statistic p.value                   
#> u     1.335  0.2479   || Where Y=X*C+u
autoplot(benchmark)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r
autoplot(in_sample(benchmark))
```

<img src="man/figures/README-unnamed-chunk-3-2.png" width="100%" />

``` r

## How to manually set the coefficient

benchmark <- annualBenchmark(hfserie = turnover,
                             lfserie = construction,
                             include.differenciation = TRUE,
                             set.coeff = 0.1)
coef(benchmark)
#> constant  hfserie 
#>  1.44013  0.10000
```
