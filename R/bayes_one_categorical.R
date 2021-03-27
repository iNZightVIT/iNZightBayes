#' @title The Dirichlet Distribution
#' @description Random generation of Dirichlet distribution
#' @details Draws independent Gamma random variables and normalises
#' @param n number of random samples
#' @param alpha vector of concentration parameters
#' @return a matrix of random samples from Diriclet distribution
#' @md
#' @export
#' @examples
#' \dontrun{
#' set.seed(1234)
#' rdirichlet(10, c(1, 1, 1))
#' }
rdirichlet = function(n, alpha) {

  if (any(alpha <= 0)) stop("all alpha must be > 0")

  # Number of groups
  k = length(alpha)

  # Generate independent Gamma random variables with alpha shape
  Y = matrix(stats::rgamma(n * k, alpha), ncol = k, byrow = TRUE)

  # Compute normalisation for each random sample
  S = apply(Y, 1, sum)

  # Parameterise into Dirichlet random variables
  return(Y / S)

}

#' @title Gibbs sampler to sample proportions of one categorical variable
#' @description Gibbs sampler to sample proportions of one categorical variable
#' @details Assumes multinomial likelihood and Dirichlet prior
#' @param x a vector of counts ("successes" in each group)
#' @param steps number of iterations to run Gibbs sampler for
#' @param alpha a vector of prior concentration parameters (default uniform)
#' @return a list of posterior samples for the proportions of one categorical variable
#' @md
#' @export
#' @examples
#' \dontrun{
#' set.seed(1234)
#' x = c(100, 200, 300)
#' mcmc = gibbs_one_categorical(x, 20000)
#' }
gibbs_one_categorical = function(x,
                                 steps,
                                 alpha = rep(1, length(x))) {

  # Directly sample from Dirichlet posterior
  theta = rdirichlet(steps, x + alpha)
  return(list(theta = theta))

}
