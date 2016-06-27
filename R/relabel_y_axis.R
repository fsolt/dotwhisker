#' Relabel the Y-Axis of a Dot-Whisker Plot
#'
#' \code{relabel_y_axis} is a convenience function for relabeling the predictors on the y-axis of a dot-whisker plot created by \code{\link[dotwhisker]{dwplot}}
#'
#' @param x A vector of labels for predictors, listed from top to bottom
#'
#' @seealso \code{\link[dotwhisker]{relabel_predictors}} to relabel the predictors in a tidy data.frame before using \code{\link[dotwhisker]{dwplot}}
#'
#' @examples
#' data(mtcars)
#' m1 <- lm(mpg ~ wt + cyl + disp, data = mtcars)
#' dwplot(m1) + relabel_y_axis(c("Weight", "Cylinders", "Displacement"))
#'
#' @export

relabel_y_axis <- function(x) {
    scale_y_continuous(breaks = length(x):1, labels = x)
}
