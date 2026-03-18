# Dirichlet Distribution for Prior and Posterior

Creates an object containing the parameters of a Dirichlet distribution.
This is used for constructing the prior or storing the information of
the posterior.

## Usage

``` r
inz_ddir(likelihood = NULL, alpha = NULL, k = NULL)
```

## Arguments

- likelihood:

  The likelihood object of class `"inz_lmulti"`.

- alpha:

  The shape parameters \\\alpha\\ (vector or matrix).

- k:

  The number of categories.

## Value

Returns an object of [class](https://rdrr.io/r/base/class.html)
`"inz_ddir"`.

An object of class `"inz_ddir"` is a list which contains the following:

- alpha:

  a vector or a matrix of the shape parameters \\\alpha\\.

- k:

  the number of categories.

If the function is used to construct the prior, the `likelihood` is
stored as an attribute.

## Details

- The shape parameter: \\\alpha_i \> 0\\; each corresponding to a
  level/group in the categorical variable

- \\k ≥ 2\\

The Dirichlet distribution is the conjugate prior for (but is not
limited to) the Multinomial likelihood.

If the data is grouped, `alpha` should be a matrix with one row per
group. Otherwise, `alpha` should be a vector.

**For prior use only:** If no `alpha` values are provided, the default
value of \\\alpha_i = 1\\, will be used for the prior (e.g.
Dirichlet(1,1,1,1) for a k=4 level categorical variable).

## See also

[`inz_lmulti`](https://bayes.inzight.nz/reference/inz_lmulti.md)

[`calculate_posterior`](https://bayes.inzight.nz/reference/calculate_posterior.md)

## Examples

``` r
# Constructing the prior with the likelihood (default prior is used)
if (FALSE) { # \dontrun{
lik <- inz_lmulti(surf_data, Qualification)
inz_ddir(likelihood = lik)

# Using a subjective prior (prior belief that degree and school qualifications are more common)
inz_ddir(likelihood = lik, alpha = c(10, 2, 10, 2))
} # }

# Grouped data example
if (FALSE) { # \dontrun{
grouped_lik <- inz_lmulti(surf_data, Qualification, Ethnicity)
inz_ddir(likelihood = grouped_lik)
} # }

# Example of inz_ddir usage in the calculate_posterior function
posterior <- inz_ddir(alpha = c(29,40,67,68), k = 4)
```
