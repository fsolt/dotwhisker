#' Generate a 'Secret Weapon' Plot of Regression Results from Multiple Models
#'
#' \code{secret_weapon} is a function for plotting regression results of multiple models as a 'secret weapon' plot
#'
#' @param x Either a model object to be tidied with \code{\link[broom]{tidy}}, or a list of such model objects, or a tidy data frame of regression results (see 'Details').
#' @param ci A number indicating the level of confidence intervals; the default is .95.
#' @param margins [Suspended] A logical value indicating whether presenting the average marginal effects of the estimates. See the Details for more information.
#' @param var The predictor whose results are to be shown in the 'secret weapon' plot
#' @param by_2sd When x is a list of model objects, should the coefficients for predictors that are not binary be rescaled by twice the standard deviation of these variables in the dataset analyzed, per Gelman (2008)?  Defaults to \code{TRUE}.  Note that when x is a tidy data frame, one can use \code{\link[dotwhisker]{by_2sd}} to rescale similarly.
#' @param \dots Arguments to pass to \code{\link[dotwhisker]{dwplot}}.
#'
#' @details
#' Andrew Gelman has coined the term \href{https://statmodeling.stat.columbia.edu/2005/03/07/the_secret_weap/}{"the secret weapon"} for dot-and-whisker plots that compare the estimated coefficients for a single predictor across many models or datasets.
#' \code{secret_weapon} takes a tidy data frame of regression results or a list of model objects and generates a dot-and-whisker plot of the results of a single variable across the multiple models.
#'
#' Tidy data frames to be plotted should include the variables \code{term} (names of predictors), \code{estimate} (corresponding estimates of coefficients or other quantities of interest), \code{std.error} (corresponding standard errors), and \code{model} (identifying the corresponding model).
#' In place of \code{std.error} one may substitute \code{lb} (the lower bounds of the confidence intervals of each estimate) and \code{ub} (the corresponding upper bounds).
#'
#' Alternately, \code{secret_weapon} accepts as input a list of model objects that can be tidied by \code{\link[broom]{tidy}} (or \code{\link[parameters]{parameters}} (with proper formatting)), or a list of such model objects.
#'
#' @return The function returns a \code{ggplot} object.
#'
#' @examples
#'
#' library(dplyr)
#' library(broom)
#'
#' # Estimate models across many samples, put results in a tidy data frame
#' by_clarity <- diamonds %>% group_by(clarity) %>%
#'  do(tidy(lm(price ~ carat + cut + color, data = .))) %>%
#'  ungroup %>% rename(model = clarity)
#'
#' # Generate a 'secret weapon' plot of the results of diamond size
#' secret_weapon(by_clarity, "carat")
#'
#'
#' @importFrom dplyr "%>%" filter select rename
#' @importFrom utils globalVariables
#'
#' @export

secret_weapon <- function(x, var = NULL, ci = .95, margins = FALSE, by_2sd = FALSE, ...) {
    # If x is list of model objects, convert to a tidy data frame
    if (!"data.frame" %in% class(x)) {
        df <- dw_tidy(x, ci, by_2sd)
    } else {
        df <- x
    }

    # Set variables that will appear in pipelines to NULL to make R CMD check happy
    term <- model <- NULL

    n_vars <- length(unique(df$term))

    # Confirm number of models, get model names
    if ("model" %in% names(df)) {
        n_models <- length(unique(df$model))
    } else {
        if (length(df$term) == n_vars) {
            stop("The 'secret weapon' is used to compare results for a single predictor across different models; please submit results from more than one model")
        } else {
            stop("Please add a variable named 'model' to distinguish different models")
        }
    }
    mod_names <- unique(df$model)

    df <- df %>% filter(term == var) %>% select(-term) %>% rename(term = model)
    p <- df %>% dwplot(...)
    return(p)
}




