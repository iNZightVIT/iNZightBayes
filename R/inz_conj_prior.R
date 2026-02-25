### Prior/Posterior ###

#' Beta Distribution for Prior and Posterior 
#'
#' Creates an object containing the parameters of a Beta distribution.
#' This is used for constructing the prior or storing the information 
#' of the posterior.
#'
#' @param likelihood The likelihood object of class \code{"inz_lbinom"}.
#' @param alpha The shape parameter \eqn{\alpha}.
#' @param beta The shape parameter \eqn{\beta}.
#'
#' @details
#' \itemize{
#'   \item Parameter 1: shape \eqn{\alpha > 0}
#'   \item Parameter 2: shape \eqn{\beta > 0}
#' }
#' 
#' The Beta distribution is the conjugate prior for (but is not limited to) 
#' the Binomial likelihood. 
#' 
#' \bold{For prior use only:}
#' If no \code{alpha} or \code{beta} parameter values are provided, a default 
#' value of \code{\eqn{\alpha}=1} and \code{\eqn{\beta}=1} will be used respectively.
#' Hence, a default prior of Beta(1,1) will be used if \code{alpha} and \code{beta} 
#' are both \code{NULL}, which is equivalent to a uninformative, Uniform(0,1) prior.
#' 
#' @return
#' Returns an object of \link{class} \code{"inz_dbeta"}. 
#' 
#' An object of class \code{"inz_dbeta"} is a list which contains the following: 
#' \item{alpha}{the shape parameter \eqn{\alpha}.}
#' \item{beta}{the shape parameter \eqn{\beta}.}
#' 
#' If the function is used to construct the prior, the \code{likelihood} is stored 
#' as an attribute. 
#' 
#' @export
#'
#' @examples
#' # Constructing the prior with the likelihood (default prior is used)
#' lik <- inz_lbinom(surf_data, Gender)
#' inz_dbeta(likelihood = lik)
#' 
#' # Using a subjective prior 
#' inz_dbeta(likelihood = lik, alpha = 8, beta = 2)
#' 
#' # Grouped data example
#' grouped_lik <- inz_lbinom(surf_data, Gender, Qualification)
#' inz_dbeta(likelihood = grouped_lik)
#' 
#' # Example of inz_dbeta usage in the calculate_posterior function
#' inz_dbeta(alpha = 108, beta = 94)
#' @seealso
#' \code{\link{inz_lbinom}}
#' 
#' \code{\link{calculate_posterior}}
inz_dbeta <- function(likelihood=NULL, alpha=NULL, beta=NULL) {
  
  num_groups <- length(likelihood$groups)
  
  if(is.null(alpha))
    alpha <- rep(1, num_groups)
  
  if(is.null(beta))
    beta <- rep(1, num_groups)
  
  
  if (!is.numeric(alpha) || any(alpha <= 0))
    stop("alpha must be a numeric value greater than 0.")
  
  if (!is.numeric(beta) || any(beta <= 0))
    stop("beta must be a numeric value greater than 0.")
  
  if (length(alpha) != length(beta))
    stop("alpha and beta parameters must have the same length.")
  
  
  structure(list(alpha=alpha, beta=beta),
            likelihood = likelihood,
            class = "inz_dbeta")
  
}



#' Dirichlet Distribution for Prior and Posterior 
#'
#' Creates an object containing the parameters of a Dirichlet distribution.
#' This is used for constructing the prior or storing the information 
#' of the posterior.
#' 
#' @param likelihood The likelihood object of class \code{"inz_lmulti"}.
#' @param alpha The shape parameters \eqn{\alpha} (vector or matrix).
#' @param k The number of categories.
#' 
#' @details
#' \itemize{
#'   \item The shape parameter: \eqn{\alpha_i > 0}; each corresponding to a level/group 
#'   in the categorical variable
#'   \item \eqn{k ≥ 2}
#' }
#' 
#' The Dirichlet distribution is the conjugate prior for (but is not limited to) 
#' the Multinomial likelihood. 
#' 
#' If the data is grouped, \code{alpha} should be a matrix with one row per group.
#' Otherwise, \code{alpha} should be a vector.
#' 
#' \bold{For prior use only:}
#' If no \code{alpha} values are provided, the default value of \eqn{\alpha_i = 1}, 
#' will be used for the prior (e.g. Dirichlet(1,1,1,1) for a k=4 level categorical
#' variable).
#' 
#' @return
#' Returns an object of \link{class} \code{"inz_ddir"}. 
#' 
#' An object of class \code{"inz_ddir"} is a list which contains the following: 
#' \item{alpha}{a vector or a matrix of the shape parameters \eqn{\alpha}.}
#' \item{k}{the number of categories.}
#' 
#' If the function is used to construct the prior, the \code{likelihood} is stored 
#' as an attribute. 
#' 
#' @export
#'
#' @examples
#' # Constructing the prior with the likelihood (default prior is used)
#' lik <- inz_lmulti(surf_data, Qualification)
#' inz_ddir(likelihood = lik)
#' 
#' # Using a subjective prior (prior belief that degree and school qualifications are more common)
#' inz_ddir(likelihood = lik, alpha = c(10, 2, 10, 2))
#' 
#' # Grouped data example
#' grouped_lik <- inz_lmulti(surf_data, Qualification, Ethnicity)
#' inz_ddir(likelihood = grouped_lik)
#' 
#' # Example of inz_ddir usage in the calculate_posterior function
#' posterior <- inz_ddir(alpha = c(29,40,67,68), k = 4)
#' @seealso
#' \code{\link{inz_lmulti}}
#' 
#' \code{\link{calculate_posterior}}
inz_ddir <- function(likelihood=NULL, alpha=NULL, k=NULL) {
  
  if (!is.null(likelihood)) {
    k <- length(likelihood$levels)
  }
  
  num_groups <- length(likelihood$groups)
  x <- likelihood$x 
  
  
  if (is.vector(x) && is.null(alpha)) {
    
    alpha <- rep(1, k)
    
  } else if (is.matrix(x) && is.null(alpha)) {
    
    alpha <- matrix(rep(1, k*num_groups), ncol=k, byrow=TRUE)
    
  }
  
  
  if (!(all(alpha>0)))
    stop("Each alpha value must be greater than 0.")
  
  if (k < 2)
    stop("The number of categories must be greater than or equal to 2.")
  
  if (is.vector(alpha)) {
    if (length(alpha) != k) 
      stop("The number of shape parameter (alpha) values must match the number of categories.")
  } else if (is.matrix(alpha)) {
    if (ncol(alpha) != k)
      stop("The number of shape parameter (alpha) values for each group must match the number of categories.")
  }
  
  structure(list(alpha=alpha, k=k),
            likelihood=likelihood,
            class= "inz_ddir")
}



#' Normal-Inverse-Gamma Distribution for Prior and Posterior 
#'
#' Creates an object containing the parameters of a Normal-inverse-gamma
#' distribution. This is used for constructing the prior or storing the 
#' information of the posterior.
#'
#' @param likelihood The likelihood object of class \code{"inz_lnorm"}.
#' @param mu The normal mean parameter \eqn{\mu}. For regression, this is a vector of 
#' coefficient values.
#' @param lambda The normal precision scaling parameter \eqn{\lambda}. For regression,
#' this is a precision matrix \eqn{\Lambda}.
#' @param alpha The inverse-gamma shape parameter \eqn{\alpha}.
#' @param beta The inverse-gamma scale parameter \eqn{\beta}.
#' 
#' @details
#' The Normal-Inverse-Gamma distribution is the conjugate prior for the Normal 
#' likelihood, when both the mean and the variance is unknown.
#' 
#' \itemize{
#'   \item \eqn{\lambda > 0}
#'   \item \eqn{\alpha > 0}
#'   \item \eqn{\beta > 0}
#' }
#' 
#' \bold{Single Variable and Grouped Data:}
#' \deqn{\sigma^2 \sim \Gamma^{-1}(\alpha, \beta)}
#' \deqn{\theta | \sigma^2 \sim \text{N}(\mu, \sigma^2V)} where \eqn{V=1/\lambda}, 
#' a variance scale factor.
#' 
#' For a single variable, \eqn{\mu} and \eqn{\lambda} are scalars.
#' 
#' For grouped data, \eqn{\mu} and \eqn{\lambda} are vectors where each 
#' value corresponds to a group.
#' 
#' \bold{Regression:}
#' \deqn{\sigma^2 \sim \Gamma^{-1}(\alpha, \beta)}
#' \deqn{\beta | \sigma^2 \sim \text{N}(\mu, \sigma^2 \Lambda^{-1})}
#' 
#' For regression, \eqn{\mu} is a vector of length 2 and \eqn{\Lambda} is a 2x2 
#' precision matrix.
#' 
#' \bold{For prior use only:}
#' If no prior parameters are provided, uninformative default values are used for the prior:
#' \itemize{
#'   \item \code{mu = 0} (or vector of zeros)
#'   \item \code{lambda = 1} (or vector of ones, or identity matrix for regression)
#'   \item \code{alpha = 0.001}
#'   \item \code{beta = 0.001}
#' }
#'  
#' @return
#' Returns an object of \link{class} \code{"inz_dNIG"} (single variable/grouped)
#' or \code{"inz_dNIG_reg"} (regression).
#' 
#' An object of class \code{"inz_dNIG"} or \code{"inz_dNIG_reg"} is a list 
#' which contains the following: 
#' \item{mu}{the mean parameter \eqn{\mu}.}
#' \item{lambda}{the precision scale parameter \eqn{\lambda} or a precision matrix
#' \eqn{\Lambda}.}
#' \item{V}{the variance scale factor or a covariance matrix.}
#' \item{alpha}{the shape parameter \eqn{\alpha}.}
#' \item{beta}{the scale parameter \eqn{\beta}.}
#' 
#' If the function is used to construct the prior, the \code{likelihood} is stored 
#' as an attribute. 
#' @export
#'
#' @examples
#' # Constructing the prior with the likelihood (default prior is used)
#' lik <- inz_lnorm(surf_data, Income)
#' inz_dNIG(likelihood = lik)
#' 
#' # Using a subjective prior
#' inz_dNIG(likelihood = lik, mu = 500, lambda = 10, alpha = 5, beta = 1)
#' 
#' # Regression example
#' reg_lik <- inz_lnorm(surf_data, Income, Hours)
#' inz_dNIG(likelihood = reg_lik)
#' 
#' # Example of inz_dNIG usage in the calculate_posterior function
#' inz_dNIG(mu = 572.4975, lambda = 201, alpha = 100.001, beta = 12118391)
#' 
#' @seealso
#' \code{\link{inz_lnorm}}
#' 
#' \code{\link{calculate_posterior}}
inz_dNIG <- function(likelihood=NULL, mu=NULL, lambda=NULL, alpha=NULL, beta=NULL) {
  
  is_regression <- !is.null(likelihood$y)
  
  if (is.null(mu)) {
    if (is_regression) {
      mu <- c(0,0) 
    } else if (!is.null(likelihood$secondary_var)){
      mu <- rep(0, length(likelihood$grouped_data))
    } else {
      mu <- 0
    }
  }
  
  if (is.null(lambda)) {
    if (is_regression) {
      lambda <- diag(1, 2)
    } else if (!is.null(likelihood$secondary_var)){
      lambda <- rep(1, length(likelihood$grouped_data))
    } else {
      lambda <- 1
    }
  }
  
  if (is.null(alpha)) {
    alpha <- 0.001
  }
  
  if (is.null(beta)) {
    beta <- 0.001
  }
  
  
  if (any(alpha <= 0))
    stop("The alpha parameter must be greater than 0.")
  
  if (any(beta <= 0))
    stop("The beta parameter must be greater than 0.")
  
  if (is.vector(lambda)) {
    if (any(lambda <= 0))
      stop("The precision parameter value(s) must be greater than 0.")
  } else if (is.matrix(lambda)) {
    if (any(diag(lambda) <= 0)) {
      stop("The precision parameter value(s) must be greater than 0.")
    }
  }
  
  
  if (is.vector(lambda)) {
    V <- 1/lambda
    param <- list(mu=mu, lambda=lambda, V=V, alpha=alpha, beta=beta)
    class <- "inz_dNIG"
  } else if (is.matrix(lambda)) {
    V <- solve(lambda)
    param <- list(mu=mu, lambda=lambda, V=V, alpha=alpha, beta=beta)
    class <- "inz_dNIG_reg"
  }
  
  structure(param, 
            likelihood=likelihood,
            class=class)
}
