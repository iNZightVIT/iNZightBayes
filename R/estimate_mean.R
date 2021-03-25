#' Estimate the sample mean
#'
#' @param x a vector of values
#' @return a posterior distribution for the mean
#' @md
#' @export
#' @examples
#' estimate_mean(rnorm(100, 50, 5))
estimate_mean <- function(x) {
    structure(
        list(posterior = mean(x), N = 1),
        class = "inzposterior"
    )
}
