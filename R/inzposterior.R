#' iNZight Posterior Object
#'
#' All posterior results are returned with this class, which includes
#' methods for obtaining posterior summaries and samples.
#'
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

#' @describeIn inzposterior Summary method for iNZight posterior objects
#' @export
#' @param object an object of class `inzposterior`
#' @md
summary.inzexact <- function(object, ...) {
    samples <- NULL
    sampler <- function(samples) if (is.null(samples)) object$sampler(1e4) else samples

    mean <- if (is.null(object$mean)) {
        colMeans(as.matrix(sampler(samples)))
    } else object$mean

    var <- if (is.null(object$variance)) {
        apply(as.matrix(sampler(samples)), 2L, var)
    } else object$variance

    quantiles <- if (is.null(object$quantiles)) {
        apply(as.matrix(sampler(samples)), 2L, stats::quantile, probs = c(0.025, 0.975))
    } else object$quantiles(c(0.025, 0.975))

    structure(
        cbind(
            mean = mean,
            var = var,
            t(quantiles)
        ),
        class = "inzexactsummary"
    )
}

#' @describeIn inzposterior Plot method for iNZight posterior objects
#' @export
print.inzexactsummary <- function(x, ...) {
    print.default(x, digits = 3)
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

#' @describeIn inzposterior Returns the mean of the estimated parameters
#' @export
#' @param x an object of class `inzposterior`
#' @md
mean.inzposterior <- function(x, ...) {
    colMeans(x$posterior)
}

#' Calculate statistics
#'
#' @param x an `inzposterior` object
#' @param f a function to compute for each parameter
#' @param ... arguments passed to `f`
#' @return a vector or matrix of values
#' @md
#' @export
calc <- function(x, f, ...) {
    apply(x$posterior, 2L, f, ...)
}
