#' Rescale regression results by multiplying by 2 standard deviations
#'
#' \code{by_2sd} rescales regression results to facilitate making dot-and-whisker plots using \code{\link[dwplot]{dwplot}}.
#'
#' @param df A data.frame including the variables \code{term} (names of independent variables), \code{estimate} (corresponding coefficient estimates), \code{std.error} (corresponding standard errors), and optionally \code{model} (when multiple models are desired on a single plot) such as generated those by \code{\link[broom]{broom::tidy}}.
#' @param dataset The data analyzed in the models whose results are recorded in df
#'
#' @details \code{by_2sd} multiplies the results from regression models saved as tidy data frames for predictors that are not binary by twice the standard deviation of these variables in the dataset analyzed.  Standardizing in this way yields coefficients that are directly comparable to those for untransformed binary predictors (Gelman 2008) and so facilitates plotting using \code{\link[dwplot]{dwplot}}.
#'
#'
#' @return A tidy data.frame
#' @examples
#' data(mtcars)
#' m1 <- lm(mpg ~ wt + cyl + disp, data = mtcars)
#' m1_df <- tidy(m1) %>% by_2sd # create data.frame of rescaled regression results
#'
#' @references
#' Gelman, Andrew. 2008. "Scaling Regression Inputs by Dividing by Two Standard Deviations." Statistics in Medicine, 27:2865–2873.
#'
#' @seealso \code{\link[arm::standardize]{arm::standardize}}
#'
#' @note As yet \code{by_2sd} does not handle factors appropriately.
#'
#' @import dplyr magrittr
#'
#' @export

by_2sd <- function(df, dataset) {
    sdX2 <- df$term %>% as.list %>%
        lapply(function(x) {
            dich <- unique(dataset[[x]]) %>% extract(!is.na(.)) %>% sort %>% identical(c(0,1))
            ifelse(!dich, 2*sd(dataset[[x]], na.rm=T), 1)
        }) %>% unlist
    df$estimate %<>% multiply_by(sdX2)
    df$std.error %<>% multiply_by(sdX2)
    df
}
