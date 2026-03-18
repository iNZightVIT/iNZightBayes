# Exact posterior for proportions of one categorical variable

Exact posterior estimation for proportions of one categorical variable
using dirichlet prior and multinomial likelihood.

## Usage

``` r
estimate_proportions(x, alpha = rep(1, length(x)))
```

## Arguments

- x:

  a vector of counts ("successes" in each group)

- alpha:

  a vector of prior concentration parameters (default uniform)

## Value

the posterior distribution estimate

## Details

Assumes multinomial likelihood and Dirichlet prior

## Examples

``` r
if (FALSE) { # \dontrun{
x = c(100, 200, 300)
post = estimate_proportions(x)
} # }
```
