# Gibbs sampler for one-way ANOVA

Gibbs sampler for one-way ANOVA

## Usage

``` r
gibbs_anova(y, steps, burnin, thin = 1, mu.0 = 0, sigma2.0 = 1e+06)
```

## Arguments

- y:

  a matrix with two columns (column 1 is the data, column 2 is the
  group)

- steps:

  number of iterations to run Gibbs sampler for after burn-in

- burnin:

  number of burn-in iterations

- thin:

  thinning factor (default = 1)

- mu.0:

  mean hyperparameter for grand mean

- sigma2.0:

  variance hyperparameter for grand mean

## Value

a list of posterior samples for group means, grand mean, variance
within, and variance between

## Details

Assumes Normal likelihood, Normal and log Uniform priors, Normal and log
Uniform hyperpriors

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(1234)
# Starling weight data
y = data.frame(obs = c(78, 88, 87, 88, 83, 82, 81, 80, 80, 89,
                       78, 78, 83, 81, 78, 81, 81, 82, 76, 76,
                       79, 73, 79, 75, 77, 78, 80, 78, 83, 84,
                       77, 69, 75, 70, 74, 83, 80, 75, 76, 75),
               group = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                         2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                         3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
                         4, 4, 4, 4, 4, 4, 4, 4, 4, 4))
mcmc = gibbs_anova(y, 10000, 5000)
} # }
```
