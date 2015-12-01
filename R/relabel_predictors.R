#' Relabel the Predictors in a Tidy Data Frame of Regression Results
#'
#' \code{relabel_predictors} is a convenience function for relabeling the predictors in a tidy data.frame to be passed to \code{\link[dotwhisker]{dwplot}}
#'
#' @param df A tidy data.frame to be passed to \code{\link[dotwhisker]{dwplot}}
#' @param replace A named character vector, with new values as values, and old values as names
#'
#' @return The function returns a tidy data.frame.
#'
#' @examples
#' library(broom)
#' library(dplyr)
#'
#' data(mtcars)
#' m1 <- lm(mpg ~ wt + cyl + disp, data = mtcars)
#' m1_df <- broom::tidy(m1) %>%
#'          relabel_predictors(c("(Intercept)" = "Intercept",
#'                               wt = "Weight",
#'                               cyl = "Cylinder",
#'                               disp = "Displacement"))
#' dwplot(m1_df)
#'
#' @seealso \code{\link[dotwhisker]{relabel_y_axis}} to relabel the predictors on the y-axis of a dot-whisker plot after using \code{\link[dotwhisker]{dwplot}}
#'
#' @importFrom plyr revalue
#'
#' @export

relabel_predictors <- function(df, replace = NULL) {
    df$term <- plyr::revalue(df$term, replace = replace)
    return(df)
}
