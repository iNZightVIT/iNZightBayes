# Construct a Likelihood Object for Multinomial Data

Creates a likelihood object from a categorical variable with three or
more levels, grouped by a secondary categorical variable if provided.

## Usage

``` r
inz_lmulti(data, primary, secondary = NULL)
```

## Arguments

- data:

  A data frame containing the variables.

- primary:

  The primary variable of interest.

- secondary:

  An optional secondary variable of interest (for grouping).

## Value

Returns an object of [class](https://rdrr.io/r/base/class.html)
"inz_lmulti".

An object of class "inz_lmulti" is a list which contains the following:

- x:

  the count of observations in each level (as a vector or as a table if
  grouped).

- levels:

  the levels of the primary variable.

- groups:

  the levels of the secondary variable, or "none" if not grouped.

## Details

Both `primary` and `secondary` must be categorical variables. They can
be passed as a string (e.g. `"Qualification"`) or as a non-string (e.g.
`Qualification`).

The `primary` variable must have three or more levels. If the `primary`
variable has only two levels, use
[`inz_lbinom`](https://bayes.inzight.nz/reference/inz_lbinom.md).

## See also

[`inz_lbinom`](https://bayes.inzight.nz/reference/inz_lbinom.md)

## Examples

``` r
# Single variable
if (FALSE) inz_lmulti(surf_data, Qualification) # \dontrun{}

# Two variables (grouped data)
if (FALSE) inz_lmulti(surf_data, Qualification, Gender) # \dontrun{}
if (FALSE) inz_lmulti(surf_data, Qualification, Ethnicity) # \dontrun{}
```
