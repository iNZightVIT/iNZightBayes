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
        samples <- gibbs_one_numeric(x, ...)
    } else {
        stop("Not implemented yet")
    }

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

#' iNZight Posterior Object
#' @name inzposterior
NULL

#' @describeIn inzposterior Print method for iNZight posterior obejcts
#' @export
#' @param x an object of class `inzposterior`
#' @param ... additional arguments passed to methods
#' @md
print.inzposterior <- function(x, ...) {
    cat("A posterior distribution\n")
}

#' @describeIn inzposterior Summary method for iNZight posterior objects
#' @export
#' @param object an object of class `inzposterior`
#' @md
summary.inzposterior <- function(object, ...) {
    summary(object$posterior)
}

#' @describeIn inzposterior Plot method for iNZight posterior objects
#' @export
#' @param x an object of class `inzposterior`
#' @param y optional, the parameter to plot. All shown if ommitted
#' @md
plot.inzposterior <- function(x, y, ...) {
    p <- tidybayes::spread_draws(x$posterior,
        !!rlang::sym("mu"), !!rlang::sym("sigma2"))
    if (missing(y)) y <- c("mu", "sigma2")

    plot_list <- lapply(y,
        function(yvar) {
            ggplot2::ggplot(p, ggplot2::aes_string(x = yvar)) +
                tidybayes::stat_halfeye()
        }
    )

    patchwork::wrap_plots(plot_list)
}
