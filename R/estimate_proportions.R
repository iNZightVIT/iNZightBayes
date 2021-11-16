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
estimate_proportions <- function(x, alpha = rep(1, length(x))) {
    theta <- x + alpha
    parnames <- paste("theta", seq_along(theta), sep = "_")
    names(theta) <- parnames
    structure(
        list(
            density = function(x) DirichletReg::ddirichlet(x, theta),
            marginal = function(par, x) stats::dbeta(x, theta[par], sum(theta) - theta[par]),
            parameters = theta,
            sampler = function(n)
                structure(rdirichlet(n, theta),
                    .Dimnames = list(NULL, parnames)
                ),
            mean = theta / sum(theta),
            variance = (theta - sum(theta)) / (sum(theta)^2 * (sum(theta) + 1)),
            parameters = "theta[i]"
        ),
        class = c("inzexact", "inzposterior")
    )
}
