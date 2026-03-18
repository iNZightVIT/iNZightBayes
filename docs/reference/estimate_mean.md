# Estimate the sample mean

Estimate the sample mean

## Usage

``` r
estimate_mean(x, y, ...)

# Default S3 method
estimate_mean(x, y, ...)

# S3 method for class 'formula'
estimate_mean(x, y, data, ...)
```

## Arguments

- x:

  a vector of values or a formula

- y:

  optional, a grouping vector

- ...:

  additional arguments passed to child functions

- data:

  optional location to find formula values

## Value

a posterior distribution for the mean

## Methods (by class)

- `estimate_mean(default)`: Default method

- `estimate_mean(formula)`: Method for a formula

## See also

[inzposterior](https://bayes.inzight.nz/reference/inzposterior.md)

## Examples

``` r
post <- estimate_mean(rnorm(100, 50, 5))
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
#>         Mean     SD Naive SE Time-series SE
#> mu     50.12 0.4555  0.01440        0.01349
#> sigma2 21.08 2.9777  0.09416        0.08881
#> 
#> 2. Quantiles for each variable:
#> 
#>         2.5%   25%   50%   75% 97.5%
#> mu     49.18 49.83 50.11 50.43 51.00
#> sigma2 16.02 19.02 20.82 22.85 27.62
#> 
plot(post)
```
