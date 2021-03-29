
<!-- README.md is generated from README.Rmd. Please edit that file -->
# iNZightBayes

<!-- badges: start -->
[![R-CMD-check](https://github.com/iNZightVIT/iNZightBayes/workflows/R-CMD-check/badge.svg)](https://github.com/iNZightVIT/iNZightBayes/actions) [![Codecov test coverage](https://codecov.io/gh/iNZightVIT/iNZightBayes/branch/master/graph/badge.svg)](https://codecov.io/gh/iNZightVIT/iNZightBayes?branch=master) <!-- badges: end -->

The goal of iNZightBayes is to ...

## Installation

You can install the released version of iNZightBayes from [CRAN](https://CRAN.R-project.org) with:

``` r
# install.packages("iNZightBayes")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("iNZightVIT/iNZightBayes")
```

## Example

``` r
post <- estimate_mean(~Sepal.Width, data = iris)
summary(post)
plot(post)
```
