#' Estimate the sample mean
#'
#' @param x a vector of values or a formula
#' @param y optional, a grouping vector
#' @param ... additional arguments passed to child functions
#' @return a posterior distribution for the mean
#' @md
#' @export
#' @seealso [inzposterior]
#' @examples
#' post <- estimate_mean(rnorm(100, 50, 5))
#' summary(post)
#' plot(post)
estimate_mean <- function(x, y, ...) {
    UseMethod("estimate_mean", x)
}

#' @describeIn estimate_mean Default method
#' @export
estimate_mean.default <- function(x, y, ...) {

    onesample <- (missing(y) || is.null(y))

    if (onesample) {
        samples <- gibbs_mean(x, ...)
    } else {
        samples <- gibbs_anova(x, y, ...)
    }

    structure(
        list(
            posterior = coda::mcmc(samples$posterior,
                start = samples$mcmc_info$burnin + 1L,
                end = samples$mcmc_info$burnin + samples$mcmc_info$iter,
                thin = samples$mcmc_info$thin
            ),
            parameters = c("mu", "sigma2")
        ),
        class = "inzposterior"
    )
}

#' @describeIn estimate_mean Method for a formula
#' @param data optional location to find formula values
#' @export
estimate_mean.formula <- function(x, y, data, ...) {
    data <- stats::model.frame(x, data)
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
