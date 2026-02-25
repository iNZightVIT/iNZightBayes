#' Calculate the Posterior Distribution
#' 
#' \code{calculate_posterior} is a generic function used to calculate the posterior 
#' distribution given the prior and its associated likelihood.
#' 
#' @param prior The prior object.
#' @param ... Currently no additional arguments.
#' 
#' @return
#' Returns an object of the same \link{class} as the \code{prior}. 
#' It is a list which contains the likelihood, the prior, and the posterior 
#' parameter values.
#' 
#' The \code{cred_level} and \code{signif_value} is stored as an attribute.
#' 
#' @export
#' 
#' @examples
#' # Beta-Binomial example
#' lik <- inz_lbinom(surf_data, Gender)
#' prior <- inz_dbeta(likelihood = lik)
#' calculate_posterior(prior = prior)
#' 
calculate_posterior <- function(prior, ...) {
  UseMethod("calculate_posterior")
}



#' @rdname calculate_posterior
#' 
#' @param cred_level The credible level (%) for the credible interval (default of 95).
#' @param signif_value The number of significant figures for output (default of 4).
#' 
#' @details
#' \bold{Beta-Binomial conjugacy:}
#' 
#' The posterior parameters for \code{inz_dbeta} are calculated and updated using:
#' \deqn{\alpha_{post} = \alpha_{prior} + x}
#' \deqn{\beta_{post} = \beta_{prior} + N - x}
#' 
#' @export
calculate_posterior.inz_dbeta <- function(prior, cred_level=95, signif_value=4, ...) {
  
  likelihood <- attr(prior,"likelihood")
  attr(prior,"likelihood") <- NULL
  
  # Posterior calculation
  alpha_post <- prior$alpha + likelihood$x
  beta_post <- prior$beta + likelihood$N - likelihood$x
  
  # Result object
  result_obj <- list(data = likelihood,
                     prior = prior,
                     posterior = inz_dbeta(alpha = alpha_post, 
                                           beta = beta_post))
  
  # Output
  structure(result_obj,
            cred_level = cred_level,
            signif_value = signif_value,
            class = class(prior)) 
  
}



#' @rdname calculate_posterior
#' 
#' @inheritParams calculate_posterior.inz_dbeta
#' 
#' @details
#' \bold{Dirichlet-Multinomial conjugacy:}
#' 
#' The posterior parameters for \code{inz_ddir} are calculated and updated using:
#' \deqn{\boldsymbol{\alpha}_{post} = \boldsymbol{\alpha}_{prior} + \mathbf{x}}
#' 
#' @export
calculate_posterior.inz_ddir <- function(prior, cred_level=95, signif_value=4, ...) {
  
  likelihood <- attr(prior,"likelihood")
  attr(prior,"likelihood") <- NULL
  
  # Posterior calculation
  alpha_post <- prior$alpha + likelihood$x
  
  # Result object
  result_obj <- list(data = likelihood,
                     prior = prior,
                     posterior = inz_ddir(alpha = alpha_post, k=prior$k))
  
  # Output
  structure(result_obj,
            cred_level = cred_level,
            signif_value = signif_value,
            class = class(prior))
}



#' @rdname calculate_posterior
#' 
#' @inheritParams calculate_posterior.inz_dbeta
#' 
#' @details
#' \bold{Normal-Inverse-Gamma Prior, Normal Likelihood conjugacy:}
#' 
#' The posterior parameters for \code{inz_dNIG} are calculated and updated using:
#' \deqn{V_n = \left( \frac{1}{V_0} + n \right)^{-1}}
#' \deqn{m_n = V_n \left( \frac{m_0}{V_0} + n \bar{x} \right)}
#' \deqn{a_n = a_0 + \frac{n}{2}}
#' \deqn{b_n = b_0 + \frac{1}{2} \left(\frac{m_0^2}{V_0} + \sum_{i} x_i^2 - \frac{m_n^2}{V_n} \right)}
#' (Murphy, 2007, Section 6.3).
#' 
#' @references 
#' Murphy, K. P. (2007). \emph{Conjugate Bayesian analysis of the Gaussian distribution} (Technical Report).
#' The University of British Columbia. \url{https://www.cs.ubc.ca/~murphyk/Papers/bayesGauss.pdf}
#' 
#' @export
calculate_posterior.inz_dNIG <- function(prior, cred_level=95, signif_value=4, ...) {
  
  likelihood <- attr(prior,"likelihood")
  attr(prior,"likelihood") <- NULL
  
  # Prior parameters
  m0 <- prior$mu
  V0 <- prior$V 
  a0 <- prior$alpha
  b0 <- prior$beta
  
  # Data 
  x_bar <- likelihood$x_bar
  n <- likelihood$n
  raw_sum_sq <- likelihood$raw_sum_sq
  
  # Posterior calculation
  Vn <- 1 / (1/V0 + n)
  mn <- Vn * (((1/V0)*m0) + n*x_bar)
  an <- a0 + n/2
  bn <- b0 + (1/2 * ((m0^2 * (1/V0)) + raw_sum_sq - (mn^2 * (1/Vn))))
  
  # Result object
  result_obj <- list(
    data = likelihood,
    prior = prior,
    posterior = inz_dNIG(mu = mn, lambda= 1/Vn, alpha = an, beta = bn)
  )
  
  # Output
  structure(result_obj,
            cred_level = cred_level, signif_value = signif_value,
            class = class(prior))
  
}



#' @rdname calculate_posterior
#' 
#' @inheritParams calculate_posterior.inz_dbeta
#' 
#' @details
#' \bold{Normal-Inverse-Gamma Prior, Normal Likelihood conjugacy for regression:}
#' 
#' The posterior parameters for \code{inz_dNIG_reg} is calculated and updated using:
#' \deqn{\boldsymbol{\Lambda}_n = \mathbf{X}^T \mathbf{X} + \boldsymbol{\Lambda}_0}
#' \deqn{\boldsymbol{\mu}_n = \boldsymbol{\Lambda}_n^{-1} (\mathbf{X}^T \mathbf{X} \hat{\boldsymbol{\beta}} + \boldsymbol{\Lambda}_0 \boldsymbol{\mu}_0)}
#' \deqn{a_n = a_0 + \frac{n}{2}}
#' \deqn{b_n = b_0 + \frac{1}{2} (\mathbf{y}^T \mathbf{y} + \boldsymbol{\mu}_0^T \boldsymbol{\Lambda}_0 \boldsymbol{\mu}_0 - \boldsymbol{\mu}_n^T \boldsymbol{\Lambda}_n \boldsymbol{\mu}_n)}
#' 
#' @export
calculate_posterior.inz_dNIG_reg <- function(prior, cred_level=95, signif_value=4, ...) {
  
  likelihood <- attr(prior,"likelihood")
  attr(prior,"likelihood") <- NULL
  
  # Prior parameters
  m0 <- prior$mu 
  lambda0 <- prior$lambda
  a0 <- prior$alpha
  b0 <- prior$beta
  
  # Data
  x <- likelihood$x
  y <- likelihood$y
  n <- length(x)
  
  # Design Matrix
  X <- cbind(1,x)
  
  ### QR DECOMP ###
  QR <- qr(X) # QR decomposition
  R <- qr.R(QR) # Upper triangular matrix
  
  beta_hat <- qr.solve(X,y) 
  
  XtX <- crossprod(R) #t(R) %*% R
  Xty <- XtX %*% beta_hat #XtXb^ 
  yty <- crossprod(y) # same as t(y) %*% y
  
  # Posterior updates 
  lambdan <- XtX + lambda0
  mn <- qr.solve(lambdan, (Xty + (lambda0 %*% m0)))
  an <- a0 + n/2
  bn <- b0 + 1/2 * drop(yty + (t(m0) %*% lambda0 %*% m0) - (t(mn) %*% lambdan %*% mn))
  
  # Result object
  result_obj <- list(
    data = likelihood,
    prior = prior,
    posterior = inz_dNIG(mu = mn, lambda = lambdan, alpha = an, beta = bn)
  )
  
  # Output
  structure(result_obj,
            cred_level = cred_level, signif_value = signif_value,
            class = class(prior))
  
}

