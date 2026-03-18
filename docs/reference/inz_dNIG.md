# Normal-Inverse-Gamma Distribution for Prior and Posterior

Creates an object containing the parameters of a Normal-inverse-gamma
distribution. This is used for constructing the prior or storing the
information of the posterior.

## Usage

``` r
inz_dNIG(
  likelihood = NULL,
  mu = NULL,
  lambda = NULL,
  alpha = NULL,
  beta = NULL
)
```

## Arguments

- likelihood:

  The likelihood object of class `"inz_lnorm"`.

- mu:

  The normal mean parameter \\\mu\\. For regression, this is a vector of
  coefficient values.

- lambda:

  The normal precision scaling parameter \\\lambda\\. For regression,
  this is a precision matrix \\\Lambda\\.

- alpha:

  The inverse-gamma shape parameter \\\alpha\\.

- beta:

  The inverse-gamma scale parameter \\\beta\\.

## Value

Returns an object of [class](https://rdrr.io/r/base/class.html)
`"inz_dNIG"` (single variable/grouped) or `"inz_dNIG_reg"` (regression).

An object of class `"inz_dNIG"` or `"inz_dNIG_reg"` is a list which
contains the following:

- mu:

  the mean parameter \\\mu\\.

- lambda:

  the precision scale parameter \\\lambda\\ or a precision matrix
  \\\Lambda\\.

- V:

  the variance scale factor or a covariance matrix.

- alpha:

  the shape parameter \\\alpha\\.

- beta:

  the scale parameter \\\beta\\.

If the function is used to construct the prior, the `likelihood` is
stored as an attribute.

## Details

The Normal-Inverse-Gamma distribution is the conjugate prior for the
Normal likelihood, when both the mean and the variance is unknown.

- \\\lambda \> 0\\

- \\\alpha \> 0\\

- \\\beta \> 0\\

**Single Variable and Grouped Data:** \$\$\sigma^2 \sim
\Gamma^{-1}(\alpha, \beta)\$\$ \$\$\theta \| \sigma^2 \sim \text{N}(\mu,
\sigma^2V)\$\$ where \\V=1/\lambda\\, a variance scale factor.

For a single variable, \\\mu\\ and \\\lambda\\ are scalars.

For grouped data, \\\mu\\ and \\\lambda\\ are vectors where each value
corresponds to a group.

**Regression:** \$\$\sigma^2 \sim \Gamma^{-1}(\alpha, \beta)\$\$
\$\$\beta \| \sigma^2 \sim \text{N}(\mu, \sigma^2 \Lambda^{-1})\$\$

For regression, \\\mu\\ is a vector of length 2 and \\\Lambda\\ is a 2x2
precision matrix.

**For prior use only:** If no prior parameters are provided,
uninformative default values are used for the prior:

- `mu = 0` (or vector of zeros)

- `lambda = 1` (or vector of ones, or identity matrix for regression)

- `alpha = 0.001`

- `beta = 0.001`

## See also

[`inz_lnorm`](https://bayes.inzight.nz/reference/inz_lnorm.md)

[`calculate_posterior`](https://bayes.inzight.nz/reference/calculate_posterior.md)

## Examples

``` r
# Constructing the prior with the likelihood (default prior is used)
if (FALSE) { # \dontrun{
lik <- inz_lnorm(surf_data, Income)
inz_dNIG(likelihood = lik)

# Using a subjective prior
inz_dNIG(likelihood = lik, mu = 500, lambda = 10, alpha = 5, beta = 1)
} # }

# Regression example
if (FALSE) { # \dontrun{
reg_lik <- inz_lnorm(surf_data, Income, Hours)
inz_dNIG(likelihood = reg_lik)
} # }

# Example of inz_dNIG usage in the calculate_posterior function
inz_dNIG(mu = 572.4975, lambda = 201, alpha = 100.001, beta = 12118391)
#> $mu
#> [1] 572.4975
#> 
#> $lambda
#> [1] 201
#> 
#> $V
#> [1] 0.004975124
#> 
#> $alpha
#> [1] 100.001
#> 
#> $beta
#> [1] 12118391
#> 
#> attr(,"class")
#> [1] "inz_dNIG"
```
