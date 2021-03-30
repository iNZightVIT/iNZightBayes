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
    list(theta = theta)
}


#' Exact posterior for proportions of one categorical variable
#'
#' Exact posterior estimation for proportions of one categorical variable
#' using dirichlet prior and multinomial likelihood.
#'
#' @details Assumes multinomial likelihood and Dirichlet prior
#' @param x a vector of counts ("successes" in each group)
#' @param alpha a vector of prior concentration parameters (default uniform)
#' @return the posterior distribution estimate
#' @md
#' @export
#' @examples
#' \dontrun{
#' x = c(100, 200, 300)
#' post = estimate_proportions(x)
#' }
estimate_proportions = function(x, alpha = rep(1, length(x))) {
    theta <- x + alpha
    parnames <- paste("theta", seq_along(theta), sep = "_")
    names(theta) <- parnames
    structure(
        list(
            density = function(x) DirichletReg::ddirichlet(x, theta),
            parameters = list(alpha = x + alpha),
            sampler = function(n)
                structure(rdirichlet(n, theta),
                    .Dimnames = list(NULL, parnames)
                ),
            mean = theta / sum(theta),
            variance = (theta - sum(theta)) / (sum(theta)^2 * (sum(theta) + 1))
        ),
        class = c("inzexact", "inzposterior")
    )
}
