# Gibbs sampler for linear regression

Gibbs sampler for linear regression

## Usage

``` r
gibbs_lm_R(y, X, steps)
```

## Arguments

- y:

  a vector of the response variable

- X:

  a matrix whose columns are the explanatory variables

- steps:

  number of iterations to run Gibbs sampler for

## Value

a list of posterior samples for regression coefficients

## Details

Assumes Normal likelihood and joint prior proportional to 1 / sigma2

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(1234)
n = 100
sigma = 40
beta = c(150, 5, 10)
x = seq(-4, 10, length = n)
y = beta[1] + beta[2] * x + beta[3] * x ^ 2 + rnorm(n, 0, sigma)
X = cbind(x, x ^ 2)
mcmc = gibbs_lm_R(y, X, 10000)
} # }
```
