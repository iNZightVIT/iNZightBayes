# Posterior Object Summary

Compute and display posterior summaries. This includes the point
estimate (posterior mean) and equal-tailed credible intervals.

## Usage

``` r
# S3 method for class 'inz_dbeta'
summary(object, ...)

# S3 method for class 'inz_ddir'
summary(object, ...)

# S3 method for class 'inz_dNIG'
summary(object, ...)

# S3 method for class 'inz_dNIG_reg'
summary(object, ...)
```

## Arguments

- object:

  A posterior object returned by `calculate_posterior`.

- ...:

  Currently no additional arguments.

## Value

An object of class `summary.inz_*`, which is used by the corresponding
`print` method to automatically print the output.

## Details

**Beta-Binomial** (`inz_dbeta`):

The point estimates are calculated using the expectation formula of the
Beta distribution: \$\$\frac{\alpha}{\alpha+\beta}\$\$

The credible intervals are calculated using the quantile function
[qbeta](https://rdrr.io/r/stats/Beta.html).

**Dirichlet-Multinomial** (`inz_ddir`):

The point estimates are calculated using the expectation formula of the
Dirichlet distribution: \$\$\frac{\alpha_i}{\alpha_0}\$\$

The marginal distribution of Dirichlet follows a Beta distribution.
Hence, the credible intervals are calculated using the quantile function
[qbeta](https://rdrr.io/r/stats/Beta.html).

**Normal-Inverse-Gamma Prior, Normal Likelihood** (`inz_dNIG` or
`inz_dNIG_reg`):

The posterior mean computed in `calculate_posterior` (\\m_n\\ or
\\\boldsymbol{\mu}\_n\\ for regression) are the point estimates.

The marginal posterior distribution of the mean follows a
t-distribution. Hence, the credible intervals are calculated using the
quantile function [qt](https://rdrr.io/r/stats/TDist.html).

## See also

[`calculate_posterior`](https://bayes.inzight.nz/reference/calculate_posterior.md)

## Examples

``` r
# Beta-Binomial example
if (FALSE) { # \dontrun{
lik <- inz_lbinom(surf_data, Gender)
prior <- inz_dbeta(likelihood = lik)
posterior <- calculate_posterior(prior = prior)
summary(posterior)
} # }

# Regression example (Normal-Inverse-Gamma)
if (FALSE) { # \dontrun{
lik <- inz_lnorm(surf_data, Income, Hours)
prior <- inz_dNIG(likelihood = lik)
posterior <- calculate_posterior(prior = prior)
summary(posterior)
} # }
```
