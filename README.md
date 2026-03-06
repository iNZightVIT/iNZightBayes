
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iNZightBayes

<!-- badges: start -->

[![R-CMD-check](https://github.com/iNZightVIT/iNZightBayes/workflows/R-CMD-check/badge.svg)](https://github.com/iNZightVIT/iNZightBayes/actions)
[![Codecov test
coverage](https://codecov.io/gh/iNZightVIT/iNZightBayes/branch/master/graph/badge.svg)](https://codecov.io/gh/iNZightVIT/iNZightBayes?branch=master)

<!-- badges: end -->

The goal of iNZightBayes is to …

## Installation

You can install the released version of iNZightBayes from
[CRAN](https://CRAN.R-project.org) with:

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
library(iNZightBayes)

post <- estimate_mean(~Sepal.Width, data = iris)
summary(post)
#> 
#> Iterations = 1001:2000
#> Thinning interval = 1 
#> Number of chains = 1 
#> Sample size per chain = 1000 
#> 
#> 1. Empirical mean and standard deviation for each variable,
#>    plus standard error of the mean:
#> 
#>          Mean      SD  Naive SE Time-series SE
#> mu     3.0574 0.03625 0.0011463      0.0011463
#> sigma2 0.1931 0.02300 0.0007272      0.0007272
#> 
#> 2. Quantiles for each variable:
#> 
#>          2.5%    25%    50%   75% 97.5%
#> mu     2.9872 3.0331 3.0558 3.082 3.132
#> sigma2 0.1527 0.1773 0.1911 0.207 0.244
plot(post)
#> Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`.
#> ℹ See also `vignette("ggplot2-in-packages")` for more information.
#> ℹ The deprecated feature was likely used in the iNZightBayes package.
#>   Please report the issue to the authors.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
```

<img src="man/figures/README-example-1.png" alt="" width="100%" />

In some cases, the posterior can be calculated exactly.

``` r
post <- estimate_proportions(c(20, 50, 30), alpha = c(10, 10, 10))
summary(post)
#>          mean       var  2.5% 97.5%
#> theta_1 0.231 -4.52e-05 0.163 0.306
#> theta_2 0.462 -3.16e-05 0.377 0.547
#> theta_3 0.308 -4.07e-05 0.232 0.390
plot(post)
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the iNZightBayes package.
#>   Please report the issue to the authors.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
```

<img src="man/figures/README-exact-1.png" alt="" width="100%" />

There’s also linear regression:

``` r
# temporary syntax:
y <- iris$Sepal.Length
x <- cbind(iris$Sepal.Width)
post <- gibbs_lm(y, x, 100)

par(mfrow = c(1, 2))
plot(post$posterior$beta.1)
plot(x[,1], y)
apply(post$posterior[,1:2], 1,
  function(b)
    lines(x[,1], cbind(1, x) %*% b, col = "#00990030"))
```

<img src="man/figures/README-regression-1.png" alt="" width="100%" />

    #> NULL
