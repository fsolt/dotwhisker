#' Format Regression Results for a 'Secret Weapon' Plot
#'
#' \code{secret_weapon} is a function for formatting regression results to be passed to \code{\link[dotwhisker]{dwplot}} to generate a 'secret weapon' plot
#'
#' @param x Either a tidy data.frame including results from multiple models (see 'Details') or a list of model objects that can be tidied with \code{\link[broom]{tidy}}
#' @param var The predictor whose results are to be shown in the 'secret weapon' plot
#'
#' @details
#' Andrew Gelman has coined the term \href{http://andrewgelman.com/2005/03/07/the_secret_weap/}{"the secret weapon"} for dot-and-whisker plots that compare the estimated coefficients for a single predictor across many models or datasets.
#' \code{secret_weapon} takes a tidy data.frame of regression results suitable for passing to \code{\link[dotwhisker]{dwplot}} or a list of model objects and reformats these results so that, when passed to \code{\link[dotwhisker]{dwplot}}, the result will be a dot-and-whisker plot of the results of a single variable across the multiple models.
#'
#' @examples
#' # Estimate models across many samples, put results in a tidy data.frame
#' by_clarity <- diamonds %>% group_by(clarity) %>%
#'  do(tidy(lm(price ~ carat + cut + color, data = .))) %>%
#'  ungroup %>% rename(model=clarity)
#'
#' # Format for a 'secret weapon' plot of the results of diamond size, make the plot
#' secret_weapon(by_clarity, "carat") %>% dwplot
#'
#'
#' @importFrom broom tidy
#' @importFrom dplyr filter select rename
#'
#' @export

secret_weapon <- function(x, var) {
    # If x is model object(s), convert to a tidy data.frame
    if (!is.data.frame(x)) {
        if (is.list(x)) {
            for (i in seq(length(x))) {
                dft <- broom::tidy(x[[i]])
                dft$model <- paste("Model", i)
                if (i==1) df <- dft else df <- rbind(df, dft)
            }
        } else {
            df <- broom::tidy(x)
        }
    } else {
        df <- x
    }

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
    return(df)
}




