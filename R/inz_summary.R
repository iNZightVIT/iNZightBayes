#' Posterior Object Summary
#' @name posterior_summary
#' 
#' @description
#' Compute and display posterior summaries. This includes the point 
#' estimate (posterior mean) and equal-tailed credible intervals.
#' 
#' @param posterior_obj A posterior object returned by \code{calculate_posterior}.
#' @param ... Currently no additional arguments.
#' 
#' @return
#' An object of class \code{summary.inz_*}, which is automatically printed
#' via the corresponding \code{print} method.
#' 
#' @seealso  
#' \code{\link{calculate_posterior}}
#' 
#' @examples
#' 
#' 
NULL


#' @rdname posterior_summary
#' 
#' @details
#' \bold{Beta-Binomial} (\code{inz_dbeta}):
#' 
#' The point estimates are calculated using the expectation formula of the Beta
#' distribution:
#' \deqn{\frac{\alpha}{\alpha+\beta}}
#' 
#' The credible intervals are calculated using the quantile function \link{qbeta}.
#' 
#' @export
summary.inz_dbeta <- function(posterior_obj, ...) {
  
  alpha_post <- posterior_obj$posterior$alpha
  beta_post <- posterior_obj$posterior$beta
  cred_level <- attr(posterior_obj, "cred_level")
  signif_value <- attr(posterior_obj, "signif_value")
  
  prior <- posterior_obj$prior
  levels <- posterior_obj$data$levels
  groups <- posterior_obj$data$groups
  
  # Estimate
  post_mean <- alpha_post/(alpha_post+beta_post)
  
  # Credible Interval
  cred_prob_lower <- (1-(cred_level*0.01))/2
  
  lower <- qbeta(cred_prob_lower, alpha_post, beta_post)
  upper <- qbeta(1-cred_prob_lower, alpha_post, beta_post)
  
  
  if (length(groups)==1) {    # univariate
    
    ### Putting everything together ###
    
    result <- list(
      estimates = matrix(c(post_mean, lower, upper,
                           1-post_mean, 1-upper, 1-lower),
                         byrow = TRUE, nrow = 2,
                         dimnames = list(levels, c("Estimate", "Lower", "Upper"))),
      
      prior = prior,
      groups = groups,
      cred_level = cred_level,
      signif_value = signif_value
    )
    
  } else {    # bivariate
    
    ### Putting everything together ###
    
    result <- list(
      prior_param = matrix(paste0("Beta(", prior$alpha, ",", prior$beta, ")"),
                           nrow = length(groups),
                           dimnames = list(groups, "")),
      
      estimates = matrix(c(post_mean, 1-post_mean), # figure out how to do row sums
                         nrow = length(groups),
                         dimnames = list(groups, levels)),
      
      cred_interval = matrix(c(rbind(lower, 1-upper, upper, 1-lower)),
                             ncol = 2, byrow = TRUE,
                             dimnames = list(rbind(groups, ""), levels)),
      
      prior = prior,
      groups = groups,
      cred_level = cred_level,
      signif_value = signif_value
    )
    
  }
  
  class(result) <- "summary.inz_dbeta"
  
  return(result)
  
}

#' @exportS3Method
print.summary.inz_dbeta <- function(result, ...) {
  
  groups <- result$groups
  
  if (length(groups)==1) {
    
    cat("Estimated Proportions with ", result$cred_level, 
        "% Credible Interval using a Beta(", result$prior$alpha, ",",
        result$prior$beta, ") prior\n\n", sep="")
    
    print(signif(result$estimates, result$signif_value))
    
  } else {
    
    cat("Prior\n")
    
    print(noquote(result$prior_param))
    
    cat("\n\nEstimated Proportions\n\n")
    
    print(signif(result$estimates, result$signif_value))
    
    cat("\n\n", result$cred_level, "% Credible Intervals\n\n", sep="")
    
    print(signif(result$cred_interval, result$signif_value))
    
  }
  
}



#' @rdname posterior_summary
#' 
#' @details
#' \bold{Dirichlet-Multinomial} (\code{inz_ddir}):
#' 
#' The point estimates are calculated using the expectation formula of the Dirichlet
#' distribution:
#' \deqn{\frac{\alpha_i}{\alpha_0}}
#' 
#' The marginal distribution of Dirichlet follows a Beta distribution.
#' Hence, the credible intervals are calculated using the quantile function 
#' \link{qbeta}.
#' 
#' @export
summary.inz_ddir <- function(posterior_obj, ...) {
  
  prior <- posterior_obj$prior
  levels <- posterior_obj$data$levels
  groups <- posterior_obj$data$groups
  cred_level <- attr(posterior_obj, "cred_level")
  signif_value <- attr(posterior_obj, "signif_value")
  
  k <- posterior_obj$posterior$k
  
  alpha_post <- posterior_obj$posterior$alpha
  
  
  # Concentration parameter calculation
  if (is.matrix(alpha_post)) {
    
    alpha_0 <- unname(rowSums(alpha_post))
    
  } else if (is.vector(alpha_post)) {
    
    alpha_0 <- sum(alpha_post) 
    
  }
  
  # Estimates
  post_mean <- alpha_post/alpha_0
  
  # Credible Interval (using the marginal distribution)
  cred_prob_lower <- (1-(cred_level*0.01))/2
  
  beta <- alpha_0 - alpha_post 
  
  lower <- qbeta(cred_prob_lower, alpha_post, beta)
  upper <- qbeta(1-cred_prob_lower, alpha_post, beta)
  
  
  if (is.vector(alpha_post)) {  # univariate
    
    ### Putting everything together ###
    
    result <- list(
      estimates = matrix(c(post_mean, lower, upper), nrow = k,
                         dimnames = list(levels, c("Estimate", "Lower", "Upper"))),
      
      groups = groups,
      prior = prior,
      cred_level = cred_level,
      signif_value = signif_value
      
    )
    
  } else {  # bivariate
    
    ### Putting everything together ###
    
    prior_print <- NULL
    
    for (i in 1:nrow(prior$alpha)) {
      prior_print[i] <- paste0("Dirichlet (", paste(prior$alpha[i, ], collapse=","), ")")
    }
    
    cred <- NULL
    
    for (i in 1:length(groups)) {
      cred <- rbind(cred, lower[i,], upper[i,])
    }
    
    dimnames(post_mean) <- list(groups, levels)
    colnames(cred) <- levels
    rownames(cred) <- rbind(groups, "")
    
    
    result <- list(
      prior_param = matrix(prior_print, nrow=length(groups),
                           dimnames=list(groups, "")),
      estimates = post_mean,
      cred_interval = cred,
      groups=groups,
      prior = prior,
      cred_level = cred_level,
      signif_value = signif_value
    )
    
  }
  
  class(result) <- "summary.inz_ddir"
  
  return(result)
  
}


#' @exportS3Method
print.summary.inz_ddir <- function(result, ...) {
  
  groups <- result$groups
  
  if (length(groups) == 1) {
    
    cat("Estimated Proportions with ", result$cred_level,
        "% Credible Interval using a Dirichlet(", 
        paste(result$prior$alpha, collapse =","), 
        ") prior\n\n", sep="") 
    
    print(signif(result$estimates, result$signif_value))
    
  } else {
    
    cat("Prior\n")
    
    print(noquote(result$prior_param))
    
    cat("\n\nEstimated Proportions\n\n")
    
    print(signif(result$estimates, result$signif_value))
    
    cat("\n\n", result$cred_level, "% Credible Intervals\n\n", sep="")
    
    print(signif(result$cred_interval, result$signif_value))
    
  }
  
}



#' @rdname posterior_summary
#' 
#' @details
#' \bold{Normal-Inverse-Gamma Prior, Normal Likelihood} (\code{inz_dNIG} or \code{inz_dNIG_reg}):
#' 
#' The point estimates are the posterior mean computed in \code{calculate_posterior} 
#' (\eqn{m_n} or \eqn{\boldsymbol{\mu}_n} for regression).
#' 
#' The marginal posterior distribution of the mean follows a t-distribution.
#' Hence, the credible intervals are calculated using the quantile function \link{qt}. 
#' 
#' @export
summary.inz_dNIG <- function(posterior_obj, ...) {
  
  mn <- posterior_obj$posterior$mu
  Vn <- posterior_obj$posterior$V
  an <- posterior_obj$posterior$alpha
  bn <- posterior_obj$posterior$beta
  
  group <- rownames(posterior_obj$data$summary_stat)
  cred_level <- attr(posterior_obj, "cred_level")
  signif_value <- attr(posterior_obj, "signif_value")
  
  
  # t-distribution parameters
  df <- 2*an
  scale <- sqrt((bn/an) * Vn)
  
  # Credible Interval (using the marginal distribution)
  cred_prob_lower <- (1-(cred_level*0.01))/2
  
  lower <- mn + qt(cred_prob_lower, df) * scale
  upper <- mn + qt(1-cred_prob_lower, df) * scale
  
  
  ### Putting everything together ###
  
  if (length(group) == 1) { # Univariate
    
    estimate <- matrix(c(mn, lower, upper), nrow=1)
    colnames(estimate) <- c("Estimate", "Lower", "Upper")
    rownames(estimate) <- ""
    
  } else { # Bivariate
    
    estimate <- matrix(c(mn, lower, upper), 
                       nrow=length(group),
                       dimnames = list(group, c("Estimate", "Lower", "Upper")))
    
  }
  
  # Output 
  result <- list(
    estimate = estimate,
    prior = posterior_obj$prior,
    group = group,
    cred_level = cred_level,
    signif_value = signif_value
  )
  
  class(result) <- "summary.inz_dNIG"
  
  return(result)
  
}


#' @exportS3Method
print.summary.inz_dNIG <- function(result, ...) {
  
  group <- result$group 
  
  if (length(group)==1) {
    cat("Mean with ", result$cred_level, 
        "% Credible Interval\n\n", sep="")
    
    print(signif(result$estimate, result$signif_value))
    
  } else {
    cat("Group Means with ", result$cred_level, 
        "% Credible Intervals\n\n", sep="")
    
    print(signif(result$estimate, result$signif_value))
  }
  
}



#' @rdname posterior_summary
#' @export
summary.inz_dNIG_reg <- function(posterior_obj, ...) {
  
  mn <- posterior_obj$posterior$mu
  lambdan <- posterior_obj$posterior$lambda
  an <- posterior_obj$posterior$alpha
  bn <- posterior_obj$posterior$beta
  
  y_label <- posterior_obj$data$y_label
  cred_level <- attr(posterior_obj, "cred_level")
  signif_value <- attr(posterior_obj, "signif_value")
  
  
  # t-distribution parameters
  df <- 2*an
  
  Vn <- solve(lambdan)
  V_diag <- diag(Vn)
  
  scale <- sqrt(as.numeric(bn/an) * V_diag)
  
  # Credible Interval (using the marginal distribution)
  cred_prob_lower <- (1-(cred_level*0.01))/2
  
  lower <- mn + qt(cred_prob_lower, df) * scale
  upper <- mn + qt(1-cred_prob_lower, df) * scale
  
  
  ### Putting everything together ###
  
  estimate <- matrix(c(mn, lower, upper), nrow=length(mn),
                     dimnames = list(c("Intercept", y_label),
                                     c("Estimate", "Lower", "Upper")))
  
  # Output 
  result <- list(
    estimate = estimate,
    prior = posterior_obj$prior,
    cred_level = cred_level,
    signif_value = signif_value
  )
  
  class(result) <- "summary.inz_dNIG_reg"
  
  return(result)
  
}


#' @exportS3Method
print.summary.inz_dNIG_reg <- function(result, ...) {
  
  cat("Linear Trend Coefficients with ", result$cred_level, 
      "% Credible Intervals\n\n", sep="")
  
  print(signif(result$estimate, result$signif_value))
  
}

