# iNZight Posterior Object

All posterior results are returned with this class, which includes
methods for obtaining posterior summaries and samples.

## Usage

``` r
# S3 method for class 'inzposterior'
print(x, ...)

# S3 method for class 'inzposterior'
summary(object, ...)

# S3 method for class 'inzexact'
summary(object, ...)

# S3 method for class 'inzexactsummary'
print(x, ...)

# S3 method for class 'inzposterior'
plot(x, y, ...)

# S3 method for class 'inzexact'
plot(x, y, ...)

# S3 method for class 'inzposterior'
mean(x, ...)
```

## Arguments

- x:

  an object of class `inzposterior`

- ...:

  additional arguments passed to methods

- object:

  an object of class `inzposterior`

- y:

  optional, the parameter to plot. All shown if ommitted

## Methods (by generic)

- `print(inzposterior)`: Print method for iNZight posterior obejcts

- `summary(inzposterior)`: Summary method for iNZight posterior objects

- `plot(inzposterior)`: Plot method for iNZight posterior objects

- `mean(inzposterior)`: Returns the mean of the estimated parameters

## Functions

- `summary(inzexact)`: Summary method for iNZight posterior objects

- `print(inzexactsummary)`: Plot method for iNZight posterior objects

- `plot(inzexact)`: Plot method for iNZight exact-posterior objects
