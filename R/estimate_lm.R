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

    cn <- colnames(samples$posterior)
    cnb <- grepl("beta", cn)
    colnames(samples$posterior)[cnb] <- paste0(gsub("\\.", "[", cn[cnb]), "]")

    structure(
        list(
            posterior = coda::mcmc(samples$posterior,
                start = samples$mcmc_info$burnin + 1L,
                end = samples$mcmc_info$burnin + samples$mcmc_info$iter,
                thin = samples$mcmc_info$thin
            ),
            parameters = c("beta[i]", "sigma2")
        ),
        class = "inzposterior"
    )
}

#' @describeIn estimate_lm Method for a formula
#' @param data a data.frame containing the data
#' @export
estimate_lm.formula <- function(y, data, ...) {
    data <- stats::model.frame(y, data)

    if (!all(sapply(data, is.numeric))) {
        stop("Both variables must be numeric")
    }

    x <- as.matrix(data[, -1, drop = FALSE])
    y <- as.double(data[, 1])

    estimate_lm(y, x, ...)
}
