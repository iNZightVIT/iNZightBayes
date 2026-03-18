# Calculate the Posterior Distribution

`calculate_posterior` is a generic function used to calculate the
posterior distribution given the prior and its associated likelihood.

## Usage

``` r
calculate_posterior(prior, ...)

# S3 method for class 'inz_dbeta'
calculate_posterior(prior, cred_level = 95, signif_value = 4, ...)

# S3 method for class 'inz_ddir'
calculate_posterior(prior, cred_level = 95, signif_value = 4, ...)

# S3 method for class 'inz_dNIG'
calculate_posterior(prior, cred_level = 95, signif_value = 4, ...)

# S3 method for class 'inz_dNIG_reg'
calculate_posterior(prior, cred_level = 95, signif_value = 4, ...)
```

## Arguments

- prior:

  The prior object.

- ...:

  Currently no additional arguments.

- cred_level:

  The credible level (%) for the credible interval (default of 95).

- signif_value:

  The number of significant figures for output (default of 4).

## Value

Returns an object of the same [class](https://rdrr.io/r/base/class.html)
as the `prior`. It is a list which contains the likelihood, the prior,
and the posterior parameter values.

The `cred_level` and `signif_value` is stored as an attribute.

## Details

**Beta-Binomial conjugacy:**

The posterior parameters for `inz_dbeta` are calculated and updated
using: \$\$ \begin{aligned} \alpha\_{post} &= \alpha\_{prior} + x \\
\beta\_{post} &= \beta\_{prior} + N - x \end{aligned} \$\$

**Dirichlet-Multinomial conjugacy:**

The posterior parameters for `inz_ddir` are calculated and updated
using: \$\$\boldsymbol{\alpha}\_{post} = \boldsymbol{\alpha}\_{prior} +
\mathbf{x}\$\$

**Normal-Inverse-Gamma Prior, Normal Likelihood conjugacy:**

The posterior parameters for `inz_dNIG` are calculated and updated
using: \$\$ \begin{aligned} V_n &= \left( \frac{1}{V_0} + n \right)^{-1}
\\ m_n &= V_n \left( \frac{m_0}{V_0} + n \bar{x} \right) \\ a_n &= a_0 +
\frac{n}{2} \\ b_n &= b_0 + \frac{1}{2} \left(\frac{m_0^2}{V_0} +
\sum\_{i} x_i^2 - \frac{m_n^2}{V_n} \right) \end{aligned} \$\$ (Murphy,
2007, Section 6.3).

**Normal-Inverse-Gamma Prior, Normal Likelihood conjugacy for
regression:**

The posterior parameters for `inz_dNIG_reg` is calculated and updated
using: \$\$ \begin{aligned} \boldsymbol{\Lambda}\_n &= \mathbf{X}^T
\mathbf{X} + \boldsymbol{\Lambda}\_0 \\ \boldsymbol{\mu}\_n &=
\boldsymbol{\Lambda}\_n^{-1} (\mathbf{X}^T \mathbf{X}
\hat{\boldsymbol{\beta}} + \boldsymbol{\Lambda}\_0 \boldsymbol{\mu}\_0)
\\ a_n &= a_0 + \frac{n}{2} \\ b_n &= b_0 + \frac{1}{2} (\mathbf{y}^T
\mathbf{y} + \boldsymbol{\mu}\_0^T \boldsymbol{\Lambda}\_0
\boldsymbol{\mu}\_0 - \boldsymbol{\mu}\_n^T \boldsymbol{\Lambda}\_n
\boldsymbol{\mu}\_n) \end{aligned} \$\$

## References

Murphy, K. P. (2007). *Conjugate Bayesian analysis of the Gaussian
distribution* (Technical Report). The University of British Columbia.
<https://www.cs.ubc.ca/~murphyk/Papers/bayesGauss.pdf>

## Examples

``` r
# Beta-Binomial example
if (FALSE) { # \dontrun{
lik <- inz_lbinom(surf_data, Gender)
prior <- inz_dbeta(likelihood = lik)
calculate_posterior(prior = prior)
} # }

# Regression example (Normal-Inverse-Gamma)
if (FALSE) { # \dontrun{
lik <- inz_lnorm(surf_data, Income, Hours)
prior <- inz_dNIG(likelihood = lik)
calculate_posterior(prior = prior)
} # }
```
