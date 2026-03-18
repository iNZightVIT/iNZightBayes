# Construct a Likelihood Object for Normal Data

Creates a likelihood object from a numeric variable, optionally grouped
by a categorical variable. Or, optionally use another numeric variable
to explore the relationship.

## Usage

``` r
inz_lnorm(data, primary, secondary = NULL)
```

## Arguments

- data:

  A data frame containing the variables.

- primary:

  The primary numeric variable of interest.

- secondary:

  An optional secondary variable of interest (categorical for grouping
  or numeric for regression).

## Value

Returns an object of [class](https://rdrr.io/r/base/class.html)
`"inz_lnorm"`.

An object of class `"inz_lnorm"` is a list which contains the following:

**For a single numeric variable:**

- x:

  the numeric variable.

- summary_stat:

  the summary statistics of the variable.

- x_bar:

  the sample mean.

- n:

  the sample size.

- raw_sum_sq:

  the raw sum of squares.

- primary_var:

  the name of the numeric variable.

**For numeric and categorical:**

- grouped_data:

  the numeric variable grouped by the categorical variable.

- summary_stat:

  the summary statistics for each group.

- x_bar:

  a vector of sample means for each group.

- n:

  a vector of sample sizes for each group.

- raw_sum_sq:

  a vector of the raw sum of squares for each group.

- primary_var:

  the name of the numeric variable.

- secondary_var:

  the name of the categorical variable.

**For numeric and numeric:**

- x:

  the explanatory/independent variable.

- y:

  the response/dependent variable.

- spearman_correlation:

  Spearman's rank correlation coefficient.

- x_label:

  the name of the explanatory variable.

- y_label:

  the name of the response variable.

## Details

`primary` and `secondary` arguments can be passed as a string (e.g.
`"Height"`) or as a non-string (e.g. `Height`).

The function behaves differently depending on the input.

**Single variable - numeric:** Computes summary statistics for the
numeric variable.

**Two variables - numeric and categorical:** Groups the numeric variable
by the categorical variable and computes summary statistics for each
group. The function automatically identifies the variable which is
numeric and uses it as the `primary` variable regardless of the order in
which the variables are inputted.

**Two variables - numeric and numeric:** Computes Spearman's
correlation. The `secondary` variable is used as the explanatory
variable and the `primary` variable is used as the response variable for
regression.

## Examples

``` r
# Single variable
if (FALSE) inz_lnorm(surf_data, Hours) # \dontrun{}

# Numeric and Categorical (grouped data)
if (FALSE) inz_lnorm(surf_data, Income, Qualification) # \dontrun{}
if (FALSE) inz_lnorm(surf_data, Qualification, Income) # \dontrun{} # gives the same output

# Numeric and Numeric (regression)
if (FALSE) inz_lnorm(surf_data, Income, Hours) # \dontrun{}
```
