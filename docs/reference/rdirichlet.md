# The Dirichlet Distribution

Random generation of Dirichlet distribution

## Usage

``` r
rdirichlet(n, alpha)
```

## Arguments

- n:

  number of random samples

- alpha:

  vector of concentration parameters

## Value

a matrix of random samples from Diriclet distribution

## Details

Draws independent Gamma random variables and normalises

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(1234)
rdirichlet(10, c(1, 1, 1))
} # }
```
