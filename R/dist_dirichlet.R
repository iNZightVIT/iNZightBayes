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
rdirichlet <- function(n, alpha) {

    if (any(alpha <= 0)) stop("all alpha must be > 0")

    # Number of groups
    k <- length(alpha)

    # Generate independent Gamma random variables with alpha shape
    Y <- matrix(stats::rgamma(n * k, alpha), ncol = k, byrow = TRUE)

    # Compute normalisation for each random sample
    S <- rowSums(Y)

    # Parameterise into Dirichlet random variables
    return(Y / S)
}
