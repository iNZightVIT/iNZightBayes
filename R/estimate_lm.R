#' @title Gibbs sampler for linear regression
#' @description Gibbs sampler for linear regression
#' @details Assumes Normal likelihood and joint prior proportional to 1 / sigma2
#' @param y a vector of the response variable
#' @param X a matrix whose columns are the explanatory variables
#' @param steps number of iterations to run Gibbs sampler for
#' @return a list of posterior samples for regression coefficients
#' @md
#' @export
#' @examples
#' \dontrun{
#' set.seed(1234)
#' n = 100
#' sigma = 40
#' beta = c(150, 5, 10)
#' x = seq(-4, 10, length = n)
#' y = beta[1] + beta[2] * x + beta[3] * x ^ 2 + rnorm(n, 0, sigma)
#' X = cbind(x, x ^ 2)
#' mcmc = gibbs_lm_R(y, X, 10000)
#' }
gibbs_lm_R = function(y,
                    X,
                    steps) {

  # Assumes: U(-Inf, Inf) prior on beta; log-Uniform prior on sigma2
  # i.e., standard nonuniform prior

  # Mean center explanatory variables for stability
  X.mean = apply(X, 2, mean)
  X = t(t(X) - X.mean)

  # Create column of 1s for intercept
  X = cbind(1, X)

  # Open posterior objects
  beta = matrix(NA, ncol = ncol(X), nrow = steps)
  sigma2 = rep(NA, length = steps)

  # Computations outside loop
  n = length(y)                        # Number of data points
  k = ncol(X)                          # Number of regression coefficients
  df = n - k                           # Degrees of freedom
  XT = t(X)                            # Transpose of design matrix
  Sigma = MASS::ginv(XT %*% X)         # Normal posterior covariance (LS solution)
  mu = Sigma %*% XT %*% y              # Normal posterior mean (LS solution)
  a = df / 2                           # Inverse-Gamma posterior shape parameter
  b = 0.5 * sum((y - X %*% mu) ^ 2)    # Inverse-Gamma posterior scale parameter

  for (i in 1:steps) {

    # 1. Sample error variance (not conditioned on regression coefficients)
    sigma2[i] = 1 / rgamma(1, a, b)

    # 2. Sample regression coefficients given error variance
    beta[i, ] = MASS::mvrnorm(1, mu, Sigma * sigma2[i])

  }

  # Transform intercept
  beta[, 1] = beta[, 1] - beta[, 2:k] %*% X.mean

  return(list(beta = beta,
              sigma2 = sigma2))

}

#' Estimate a linear regression model
#'
#' @param y a vector of response values or a formula
#' @param ... additional arguments passed to child functions
#' @return a posterior distribution for the regression parameters
#' @md
#' @export
#' @seealso [inzposterior]
#' @examples
#' post <- estimate_lm(Petal.Length ~ Petal.Width, data = iris)
#' summary(post)
#' plot(post)
estimate_lm <- function(y, ...) {
    UseMethod("estimate_lm", y)
}

#' @describeIn estimate_lm Default method
#' @param x predictor(s), a matrix
#' @export
estimate_lm.default <- function(y, x, data, ...) {

    samples <- gibbs_lm(y, x, steps = 1000)

    structure(
        list(
            posterior = coda::mcmc(samples$posterior,
                start = samples$mcmc_info$burnin + 1L,
                end = samples$mcmc_info$burnin + samples$mcmc_info$iter,
                thin = samples$mcmc_info$thin
            )
        ),
        class = "inzposterior"
    )
}

#' @describeIn estimate_lm Method for a formula
#' @param data a data.frame containing the data
#' @export
estimate_lm.formula <- function(y, data, ...) {
    data <- stats::model.frame(y, data)
    vt <- ifelse(sapply(data, is.numeric), "num", "cat")
    if (length(vt) == 1L) {
        if (vt[1L] == "cat") stop("Variable must be numeric")
        x <- data[, 1L]
        y <- NULL
    } else if (length(vt) == 2L) {
        if (!all(c("num", "cat") %in% vt))
            stop("Need to specify a numeric and a factor variable")
        x <- data[, vt == "num"]
        y <- data[, vt == "cat"]
    } else {
        stop("Cannot handle formula", x)
    }
    estimate_mean.default(x, y, ...)
}
