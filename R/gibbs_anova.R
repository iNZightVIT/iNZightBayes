#' @title Gibbs sampler for one-way ANOVA
#' @description Gibbs sampler for one-way ANOVA
#' @details Assumes Normal likelihood, Normal and log Uniform priors, Normal and log Uniform hyperpriors
#' @param y a matrix with two columns (column 1 is the data, column 2 is the group)
#' @param steps number of iterations to run Gibbs sampler for after burn-in
#' @param burnin number of burn-in iterations
#' @param thin thinning factor (default = 1)
#' @param mu.0 mean hyperparameter for grand mean
#' @param sigma2.0 variance hyperparameter for grand mean
#' @return a list of posterior samples for group means, grand mean, variance within, and variance between
#' @md
#' @importFrom stats rgamma rnorm
#' @export
#' @examples
#' \dontrun{
#' set.seed(1234)
#' # Starling weight data
#' y = data.frame(obs = c(78, 88, 87, 88, 83, 82, 81, 80, 80, 89,
#'                        78, 78, 83, 81, 78, 81, 81, 82, 76, 76,
#'                        79, 73, 79, 75, 77, 78, 80, 78, 83, 84,
#'                        77, 69, 75, 70, 74, 83, 80, 75, 76, 75),
#'                group = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'                          2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'                          3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
#'                          4, 4, 4, 4, 4, 4, 4, 4, 4, 4))
#' mcmc = gibbs_anova(y, 10000, 5000)
#' }
gibbs_anova = function(y, steps, burnin, thin = 1,
                       mu.0 = 0, sigma2.0 = 1e6) {

  # TO DO: Investigate numerical issues when variability between groups relative to within groups is small (close to 0)
  # TO DO: Inverse-Gamma conjugate for two variance parameters?  log-Uniform seems okay.

  n = nrow(y)         # Sample size
  U = unique(y[, 2])  # Unique groups
  K = length(U)       # Number of groups

  # Convert data frame to list
  Y = list()
  for (k in 1:K) {
    Y[[k]] = y[which(y[, 2] == U[k]), 1]
  }

  # Open empty objects
  mu = matrix(NA, nrow = steps + burnin, ncol = K)
  mu.G = sigma2.G = sigma2 = rep(NA, steps + burnin)

  # Random initial points from hyperprior
  mu[1, ] = rnorm(K, mean = mu.0, sd = sigma2.0)
  mu.G[1] = rnorm(1, mean = mu.0, sd = sigma2.0)

  sigma2[1] = 1    # To do: Randomise initial point
  sigma2.G[1] = 1  # To do: Randomise initial point

  # Gibbs sampler
  for (i in 1:(steps + burnin - 1)) {

    # Sample grand mean
    sigma2.n = 1 / (1 / sigma2.0 + K / sigma2.G[i])
    mu.n = sigma2.n * (mu.0 / sigma2.0 + sum(mu[i, ]) / sigma2.G[i])
    mu.G[i + 1] = rnorm(1, mu.n, sqrt(sigma2.n))

    # Sample grand variance (between group variability)
    alpha.n = K / 2
    beta.n = 0.5 * sum((mu[i, ] - mu.G[i + 1]) ^ 2)
    sigma2.G[i + 1] = 1 / rgamma(1, alpha.n, beta.n)

    # Sample group means
    for (k in 1:K) {

      # Compute posterior parameters
      sigma2.k = 1 / (1 / sigma2.G[i + 1] + length(Y[[k]]) / sigma2[i])
      mu.k = sigma2.k * (mu.G[i + 1] / sigma2.G[i + 1] + sum(Y[[k]]) / sigma2[i])

      # Sample group means
      mu[i + 1, k] = rnorm(1, mu.k, sqrt(sigma2.k))

    }

    # Sample variance (within group variability) given other parameters
    alpha.n = n / 2
    beta.n = 0
    for (k in 1:K) {
      beta.n = beta.n + sum((Y[[k]] - mu[i + 1, k]) ^ 2)
    }
    beta.n = 0.5 * beta.n
    sigma2[i + 1] = 1 / rgamma(1, alpha.n, beta.n)

  }

  keep = seq(burnin + 1, steps + burnin, by = thin)

  return(list(mu = mu[keep, ],
              sigma2 = sigma2[keep],
              mu.G = mu.G[keep],
              sigma2.G = sigma2.G[keep]))


}
