#' @title Gibbs sampler to sample mean and variance of one numeric variable
#' @description Gibbs sampler to sample mean and variance of one numeric variable
#' @details Assumes conjugate Normal-Inverse-Gamma priors on mean and variance
#' @param y a vector of values
#' @param steps number of iterations to run Gibbs sampler for
#' @param burnin number of burn-in iterations to discard before proper steps
#' @param thin thinning factor (default 1)
#' @param mu.0 prior mean for mu (default 0)
#' @param sigma2.0 prior variance for mu (default 1e6)
#' @param alpha prior shape parameter for sigma2 (default 1e-3)
#' @param beta prior scale parameter for sigma2 (default 1e-3)
#' @param verbose logical, if `TRUE` progress will be printed to console
#' @return a list of posterior samples for the mean and variance of one numeric variable
#' @md
#' @export
#' @examples
#' \dontrun{
#' set.seed(1234)
#' n = 1000
#' y = rnorm(n, 10, sqrt(4))
#' mcmc = gibbs_one_numeric(y, 20000, 5000)
#' }
gibbs_one_numeric_R = function(y,
                               steps,
                               burnin,
                               thin = 1,
                               mu.0 = 0,
                               sigma2.0 = 1e6,
                               alpha = 1e-3,
                               beta = 1e-3,
                               verbose = TRUE) {

  # Create vectors to store posterior samples
  mu = sigma2 = rep(NA, steps + burnin)
  n = length(y)

  # Set good starting values
  mu[1] = mean(y)
  sigma2[1] = stats::var(y)

  #####
  # Run MCMC
  #####

  for (i in 1:(burnin + steps - 1)) {

    # Print out progress
    if (verbose && i %% 1000 == 0) {
      if (i <= burnin) print(paste0("Burn-in Iteration ", i))
      else print(paste0("Proper Iteration ", i - burnin))
    }

    #####
    # Step 1: Sample from full conditional of mu | sigma2
    #####

    # Compute posterior parameters for mu | sigma2
    sigma2.n = 1 / (1 / sigma2.0 + n / sigma2[i])
    mu.n = sigma2.n * (mu.0 / sigma2.0 + sum(y) / sigma2[i])

    # Sample mu[i + 1] | sigma2[i] using Normal conjugate
    mu[i + 1] = stats::rnorm(1, mu.n, sqrt(sigma2.n))

    #####
    # Step 2: Sample from full conditional of sigma2 | mu
    #####

    # Compute posterior parameters for sigma2 | mu
    alpha.n = alpha + n / 2
    beta.n = beta + 0.5 * sum((y - mu[i + 1]) ^ 2)

    # Sample sigma2[i + 1] given mu[i + 1] using Inverse-Gamma conjugate
    sigma2[i + 1] = 1 / stats::rgamma(1, alpha.n, beta.n)

    # cat("[", mu[i+1], ", ", sigma2[i+1], "], [", alpha.n, ", ", beta.n, "]\n", sep ="");

  }

  #####
  # Remove burn-in and thin
  #####
  keep = seq(burnin + 1, burnin + steps, by = thin)
  mu = mu[keep]
  sigma2 = sigma2[keep]

  return(list(mu = mu,
              sigma2 = sigma2))

}
