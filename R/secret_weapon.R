#' Generate a 'Secret Weapon' Plot of Regression Results from Multiple Models
#'
#' \code{secret_weapon} is a function for plotting regression results of multiple models as a 'secret weapon' plot
#'
#' @param x Either a tidy data.frame including results from multiple models (see 'Details') or a list of model objects that can be tidied with \code{\link[broom]{tidy}}
#' @param var The predictor whose results are to be shown in the 'secret weapon' plot
#' @param alpha A number setting the criterion of the confidence intervals. The default value is .05, corresponding to 95-percent confidence intervals.
#' @param dot_args A list of arguments specifying the appearance of the dots representing mean estimates.  For supported arguments, see \code{\link{geom_point}}.
#' @param whisker_args A list of arguments specifying the appearance of the whiskers representing confidence intervals.  For supported arguments, see \code{\link{geom_segment}}.
#'
#' @details
#' Andrew Gelman has coined the term \href{http://andrewgelman.com/2005/03/07/the_secret_weap/}{"the secret weapon"} for dot-and-whisker plots that compare the estimated coefficients for a single predictor across many models or datasets.
#' \code{secret_weapon} takes a tidy data.frame of regression results or a list of model objects and generates a dot-and-whisker plot of the results of a single variable across the multiple models.
#'
#' Tidy data.frames to be plotted should include the variables \code{term} (names of predictors), \code{estimate} (corresponding estimates of coefficients or other quantities of interest), \code{std.error} (corresponding standard errors), and \code{model} (identifying the corresponding model).
#' In place of \code{std.error} one may substitute \code{lb} (the lower bounds of the confidence intervals of each estimate) and \code{ub} (the corresponding upper bounds).
#'
#' Alternately, \code{secret_weapon} accepts as input a list of model objects that can be tidied by \code{\link[broom]{tidy}}.
#'
#' @return The function returns a \code{ggplot} object.
#'
#' @examples
#' library(broom)
#' library(dplyr)
#'
#' # Estimate models across many samples, put results in a tidy data.frame
#' by_clarity <- diamonds %>% group_by(clarity) %>%
#'  do(broom::tidy(lm(price ~ carat + cut + color, data = .))) %>%
#'  ungroup %>% rename(model=clarity)
#'
#' # Generate a 'secret weapon' plot of the results of diamond size
#' secret_weapon(by_clarity, "carat")
#'
#'
#' @import dplyr
#' @importFrom broom tidy
#' @importFrom utils globalVariables
#'
#' @export

secret_weapon <- function(x, var = NULL, alpha = .05,
                          dot_args = NULL, whisker_args = NULL) {
    # If x is list of model objects, convert to a tidy data.frame
    df <- dw_tidy(x)

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
    p <- df %>% dwplot(alpha = alpha, dot_args = dot_args, whisker_args = whisker_args)
    return(p)
}




