# Beta Distribution for Prior and Posterior

Creates an object containing the parameters of a Beta distribution. This
is used for constructing the prior or storing the information of the
posterior.

## Usage

``` r
inz_dbeta(likelihood = NULL, alpha = NULL, beta = NULL)
```

## Arguments

- likelihood:

  The likelihood object of class `"inz_lbinom"`.

- alpha:

  The shape parameter \\\alpha\\.

- beta:

  The shape parameter \\\beta\\.

## Value

Returns an object of [class](https://rdrr.io/r/base/class.html)
`"inz_dbeta"`.

An object of class `"inz_dbeta"` is a list which contains the following:

- alpha:

  the shape parameter \\\alpha\\.

- beta:

  the shape parameter \\\beta\\.

If the function is used to construct the prior, the `likelihood` is
stored as an attribute.

## Details

- Parameter 1: shape \\\alpha \> 0\\

- Parameter 2: shape \\\beta \> 0\\

The Beta distribution is the conjugate prior for (but is not limited to)
the Binomial likelihood.

**For prior use only:** If no `alpha` or `beta` parameter values are
provided, a default value of \\\alpha = 1\\ and \\\beta = 1\\ will be
used respectively. Hence, a default prior of Beta(1,1) will be used if
`alpha` and `beta` are both `NULL`, which is equivalent to a
uninformative, Uniform(0,1) prior.

## See also

[`inz_lbinom`](https://bayes.inzight.nz/reference/inz_lbinom.md)

[`calculate_posterior`](https://bayes.inzight.nz/reference/calculate_posterior.md)

## Examples

``` r
# Constructing the prior with the likelihood (default prior is used)
if (FALSE) { # \dontrun{
lik <- inz_lbinom(surf_data, Gender)
inz_dbeta(likelihood = lik)

# Using a subjective prior 
inz_dbeta(likelihood = lik, alpha = 8, beta = 2)
} # }

# Grouped data example
if (FALSE) { # \dontrun{
grouped_lik <- inz_lbinom(surf_data, Gender, Qualification)
inz_dbeta(likelihood = grouped_lik)
} # }

# Example of inz_dbeta usage in the calculate_posterior function
inz_dbeta(alpha = 108, beta = 94)
#> $alpha
#> [1] 108
#> 
#> $beta
#> [1] 94
#> 
#> attr(,"class")
#> [1] "inz_dbeta"
```
