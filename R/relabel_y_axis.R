#' Relabel Y Axis
#'
#' \code{relabel_y_axis} is a convenience function for relabeling the predictors of a dot-whisker plot
#'
#' @param x A vector of labels for predictors, listed from top to bottom
#'
#' @examples
#' data(mtcars)
#' m1 <- lm(mpg ~ wt + cyl + disp, data = mtcars)
#' p <- dwplot(m1) + relabel_y_axis(c("Intercept", "Weight", "Cylinders", "Displacement"))
#'
#' @export

relabel_y_axis <- function(x) {
    scale_y_discrete(breaks = length(x):1, labels = x)
}
