#' Construct a Likelihood Object for Binomial Data
#' 
#' Creates a likelihood object from a binary categorical variable,
#' grouped by a secondary categorical variable if provided.
#' 
#' @param data A data frame containing the variables.
#' @param primary The primary binary variable of interest. 
#' @param secondary An optional secondary variable of interest (for grouping).
#' 
#' @details
#' Both \code{primary} and \code{secondary} must be categorical variables.
#' They can be passed as a string (e.g. \code{"Gender"}) or as a non-string (e.g. \code{Gender}).
#' 
#' The \code{primary} variable must be binary (i.e. a two-level categorical variable).
#' The first level of the variable is treated as the 'success' (\code{x}).
#' 
#' For grouped data, the function calculates the Binomial parameters, \code{x} and \code{N},
#' for each level of the \code{secondary} variable.
#' 
#' @return 
#' Returns an object of \link{class} \code{"inz_lbinom"}. 
#' 
#' An object of class \code{"inz_lbinom"} is a list which contains the following:
#' \item{x}{the number of successes.}
#' \item{N}{the total number of observations.}
#' \item{levels}{the levels of the primary variable (the binary outcomes).}
#' \item{groups}{the levels of the secondary variable, or "none" if not grouped.}
#' 
#' @export
#'
#' @examples
#' # Single variable case
#' inz_lbinom(surf_data, Gender)
#' 
#' # Two-variable case
#' inz_lbinom(surf_data, Gender, Qualification)
inz_lbinom <- function(data, primary, secondary=NULL) {
  
  function_call <- match.call()
  
  if (is.character(data)) {
    data <- get(data, envir = parent.frame())
  }
  
  if (is.character(function_call$primary)) {
    primary_var <- function_call$primary
  } else {
    primary_var <- deparse(function_call$primary)
  }
  
  if (is.null(function_call$secondary)) {
    secondary_var <- NULL
  } else if (is.character(function_call$secondary)) {
    secondary_var <- function_call$secondary
  } else {
    secondary_var <- deparse(function_call$secondary)
  }
  
  
  if (is.null(secondary_var)) {
    
    summary <- table(data[[primary_var]])
    
    groups <- "none"
    levels <- names(summary)
    N <- sum(summary)
    x <- summary[[1]]
    
  } else if (!is.null(secondary_var)) {
    
    summary <- table(data[[primary_var]], data[[secondary_var]])
    
    groups <- colnames(summary)
    levels <- rownames(summary)
    N <- unname(colSums(summary))
    x <- unname(summary[levels[1],])
    
  }
  
  
  if (length(levels) != 2)
    stop("The Binomial Distribution assumes 2 outcomes (success/failure).") 
  
  
  structure(list(x=x,N=N,levels=levels, groups=groups),
            class="inz_lbinom")
  
}



#' Construct a Likelihood Object for Multinomial Data
#' 
#' Creates a likelihood object from a categorical variable with three or more levels,
#' grouped by a secondary categorical variable if provided.
#' 
#' @param data A data frame containing the variables.
#' @param primary The primary variable of interest. 
#' @param secondary An optional secondary variable of interest (for grouping). 
#'
#' @details
#' Both \code{primary} and \code{secondary} must be categorical variables.
#' They can be passed as a string (e.g. \code{"Qualification"}) or as a non-string (e.g. \code{Qualification}).
#' 
#' The \code{primary} variable must have three or more levels.
#' If the \code{primary} variable has only two levels, use \code{\link{inz_lbinom}}.
#' 
#' @return
#' Returns an object of \link{class} "inz_lmulti". 
#' 
#' An object of class "inz_lmulti" is a list which contains the following:
#' \item{x}{the count of observations in each level (as a vector or as a table if grouped).}
#' \item{levels}{the levels of the primary variable.}
#' \item{groups}{the levels of the secondary variable, or "none" if not grouped.}
#' 
#' @seealso
#' \code{\link{inz_lbinom}}
#' 
#' @export
#'
#' @examples
#' # Single variable case
#' inz_lmulti(surf_data, Qualification)
#' 
#' # Two-variable case
#' inz_lmulti(surf_data, Qualification, Gender)
#' inz_lmulti(surf_data, Qualification, Ethnicity)
inz_lmulti <- function(data, primary, secondary=NULL) {
  
  function_call <- match.call()
  
  if (is.character(data)) {
    data <- get(data, envir = parent.frame())
  }
  
  if (is.character(function_call$primary)) {
    primary_var <- function_call$primary
  } else {
    primary_var <- deparse(function_call$primary)
  }
  
  if (is.null(function_call$secondary)) {
    secondary_var <- NULL
  } else if (is.character(function_call$secondary)) {
    secondary_var <- function_call$secondary
  } else {
    secondary_var <- deparse(function_call$secondary)
  }
  
  
  if (is.null(secondary_var)) {
    
    summary <- table(data[[primary_var]])
    
    groups <- "none"
    levels <- names(summary)
    x <- as.vector(unname(summary))
    
  } else if (!is.null(secondary_var)) {
    
    summary <- table(data[[secondary_var]], data[[primary_var]])
    
    groups <- rownames(summary)
    levels <- colnames(summary)
    x <- summary
    
  }
  
  if (length(levels) < 3)
    stop("The primary variable must have three or more levels (categories).")
  
  structure(list(x=x, levels=levels, groups=groups),
            class = "inz_lmulti")
}



#' Construct a Likelihood Object for Normal Data
#'
#' Creates a likelihood object from a numeric variable, optionally grouped by a 
#' categorical variable. Or, optionally use another numeric variable to explore
#' the relationship.
#' 
#' @param data A data frame containing the variables.
#' @param primary The primary numeric variable of interest. 
#' @param secondary An optional secondary variable of interest (categorical for grouping
#' or numeric for regression).
#' 
#' @details
#' \code{primary} and \code{secondary} arguments can be passed as a string 
#' (e.g. \code{"Height"}) or as a non-string (e.g. \code{Height}).
#' 
#' The function behaves differently depending on the input.
#' 
#' \strong{Single variable - numeric:} Computes summary statistics for the 
#' numeric variable.
#' 
#' \strong{Two variables - numeric and categorical:} Groups the numeric variable 
#' by the categorical variable and computes summary statistics for each group. 
#' The function automatically identifies the variable of which is numeric and uses it
#' as the \code{primary} variable regardless of the order in which the variables 
#' are inputted.
#' 
#' \strong{Two variables - numeric and numeric:} Computes Spearman's correlation.
#' The \code{secondary} variable is used as the explanatory variable and the 
#' \code{primary} variable is used as the response variable for regression.
#' 
#' @return
#' Returns an object of \link{class} \code{"inz_lnorm"}. 
#' 
#' An object of class \code{"inz_lnorm"} is a list which contains the following:
#' 
#' \bold{For a single numeric variable:}
#' \item{x}{the numeric variable.}
#' \item{summary_stat}{the summary statistics of the variable.}
#' \item{x_bar}{the sample mean.}
#' \item{n}{the sample size.}
#' \item{raw_sum_sq}{the raw sum of squares.}
#' \item{primary_var}{the name of the numeric variable.}
#' 
#' \bold{For numeric and categorical:}
#' \item{grouped_data}{the numeric variable grouped by the categorical variable.}
#' \item{summary_stat}{the summary statistics for each group.}
#' \item{x_bar}{a vector of sample means for each group.}
#' \item{n}{a vector of sample sizes for each group.}
#' \item{raw_sum_sq}{a vector of the raw sum of squares for each group.}
#' \item{primary_var}{the name of the numeric variable.}
#' \item{secondary_var}{the name of the categorical variable.}
#' 
#' \bold{For numeric and numeric:}
#' \item{x}{the explanatory/independent variable.}
#' \item{y}{the response/dependent variable.}
#' \item{spearman_correlation}{Spearman's rank correlation coefficient.}
#' \item{x_label}{the name of the explanatory variable.}
#' \item{y_label}{the name of the response variable.}
#' 
#' @export
#'
#' @examples
#' # Single variable case
#' inz_lnorm(surf_data, Hours)
#' 
#' # Numeric and Categorical case (grouped data)
#' inz_lnorm(surf_data, Income, Qualification)
#' inz_lnorm(surf_data, Qualification, Income) # gives the same output
#' 
#' # Numeric and Numeric case (regression)
#' inz_lnorm(surf_data, Income, Hours)
inz_lnorm <- function(data, primary, secondary=NULL) {
  
  function_call <- match.call()
  
  if (is.character(data)) {
    data <- get(data, envir = parent.frame())
  }
  
  if (is.character(function_call$primary)) {
    primary_var <- function_call$primary
  } else {
    primary_var <- deparse(function_call$primary)
  }
  
  if (is.null(function_call$secondary)) {
    secondary_var <- NULL
  } else if (is.character(function_call$secondary)) {
    secondary_var <- function_call$secondary
  } else {
    secondary_var <- deparse(function_call$secondary)
  }
  
  if (is.null(secondary_var) && !is.numeric(data[[primary_var]]))
    stop("The primary variable of interest must be a numeric variable.")
  
  if (is.null(secondary_var)) {
    
    x <- na.omit(data[[primary_var]])
    
    summary_stat <- c(min(x), quantile(x, probs=0.25), 
                      median(x), quantile(x, probs=0.75), 
                      max(x), mean(x), sd(x), length(x))
    
    summary_stat <- matrix(summary_stat, nrow=1)
    
    colnames(summary_stat) <- c("Min", "25%", "Median", "75%", "Max", "Mean", "SD", "Sample Size")
    rownames(summary_stat) <- ""
    
    x_bar <- unname(summary_stat[, "Mean"])
    n <- unname(summary_stat[, "Sample Size"])
    raw_sum_sq <- sum(x^2)
    
    result <- list(x=x, summary_stat=summary_stat, x_bar=x_bar, n=n, raw_sum_sq=raw_sum_sq, primary_var=primary_var)
    
    
  } else if (!is.null(secondary_var)) {
    
    # For numeric/categorical case -> numeric variable will be the primary variable of interest
    # and categorical variable will be the secondary variable of interest. This is regardless of
    # the type of variable selected as the first variable or second variable.
    
    if ((is.factor(data[[primary_var]]) || is.character(data[[primary_var]])) &&
        is.numeric(data[[secondary_var]])) {
      
      og_primary_var <- primary_var
      
      # switch
      primary_var <- secondary_var
      secondary_var <- og_primary_var
      
    } 
    
    # The output for inz_lnorm(data, categorical, numeric) will be the same as 
    # the output for inz_lnorm(data, numeric, categorical).
    
    
    ### numeric/categorical ###
    if (is.factor(data[[secondary_var]]) || is.character(data[[secondary_var]])) {
      
      # Removing NA obs.
      non_na <- complete.cases(data[[primary_var]], data[[secondary_var]])
      clean_primary_var <- data[[primary_var]][non_na]
      clean_secondary_var <- data[[secondary_var]][non_na]
      
      grouped_data <- split(clean_primary_var, clean_secondary_var)
      
      summary_stat <- lapply(grouped_data, function(group) {
        
        c(min(group), quantile(group, probs=0.25), median(group),
          quantile(group, probs=0.75), max(group), mean(group),
          sd(group), length(group))
        
      })
      
      summary_stat <- do.call(rbind, summary_stat)
      colnames(summary_stat) <- c("Min", "25%", "Median", "75%", "Max", "Mean", "SD", "Sample Size")
      
      x_bar <- unname(summary_stat[, "Mean"])
      n <- unname(summary_stat[, "Sample Size"])
      raw_sum_sq <- unname(unlist(lapply(grouped_data, function(group) sum(group^2))))
      
      result <- list(grouped_data=grouped_data, summary_stat=summary_stat, x_bar=x_bar, n=n, raw_sum_sq=raw_sum_sq, primary_var=primary_var, secondary_var=secondary_var)
      
      
    } else if (is.numeric(data[[secondary_var]])) {
      
      # Removing NA obs.
      non_na <- complete.cases(data[[primary_var]], data[[secondary_var]])
      clean_primary_var <- data[[primary_var]][non_na]
      clean_secondary_var <- data[[secondary_var]][non_na]
      
      y <- clean_primary_var #response/dependent var.
      x <- clean_secondary_var #predictor/independent var.
      
      spearman_correlation <- cor(x,y,method="spearman")
      
      result <- list(x=x, y=y, spearman_correlation=spearman_correlation,
                     x_label=secondary_var, y_label=primary_var)
      
    }
    
  }
  
  structure(result,
            class = "inz_lnorm")
  
}


