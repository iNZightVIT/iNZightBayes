# Gibbs Sample for Single Mean and Variance

Gibbs sampler to sample mean and variance of one numeric variable

## Usage

``` r
gibbs_mean(
  y,
  steps = 1000L,
  burnin = 1000L,
  thin = 1L,
  mu_0 = 0,
  sigma2_0 = 1e+06,
  alpha = 0.001,
  beta = 0.001
)
```

## Arguments

- y:

  a vector of values

- steps:

  number of iterations to run Gibbs sampler for

- burnin:

  number of burn-in iterations to discard before proper steps

- thin:

  thinning factor (default 1)

- mu_0:

  prior mean for mu (default 0)

- sigma2_0:

  prior variance for mu (default 1e6)

- alpha:

  prior shape parameter for sigma2 (default 1e-3)

- beta:

  prior scale parameter for sigma2 (default 1e-3)

## Details

Assumes conjugate Normal-Inverse-Gamma priors on mean and variance:
\$\$y \sim \mathrm{Normal}(\mu, \sigma^2)\$\$
