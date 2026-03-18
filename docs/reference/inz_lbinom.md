# Construct a Likelihood Object for Binomial Data

Creates a likelihood object from a binary categorical variable, grouped
by a secondary categorical variable if provided.

## Usage

``` r
inz_lbinom(data, primary, secondary = NULL)
```

## Arguments

- data:

  A data frame containing the variables.

- primary:

  The primary binary variable of interest.

- secondary:

  An optional secondary variable of interest (for grouping).

## Value

Returns an object of [class](https://rdrr.io/r/base/class.html)
`"inz_lbinom"`.

An object of class `"inz_lbinom"` is a list which contains the
following:

- x:

  the number of successes.

- N:

  the total number of observations.

- levels:

  the levels of the primary variable (the binary outcomes).

- groups:

  the levels of the secondary variable, or "none" if not grouped.

## Details

Both `primary` and `secondary` must be categorical variables. They can
be passed as a string (e.g. `"Gender"`) or as a non-string (e.g.
`Gender`).

The `primary` variable must be binary (i.e. a two-level categorical
variable). The first level of the variable is treated as the 'success'
(`x`).

For grouped data, the function calculates the Binomial parameters, `x`
and `N`, for each level of the `secondary` variable.

## Examples

``` r
# Single variable
if (FALSE) inz_lbinom(surf_data, Gender) # \dontrun{}

# Two variables (grouped data)
if (FALSE) inz_lbinom(surf_data, Gender, Qualification) # \dontrun{}
```
