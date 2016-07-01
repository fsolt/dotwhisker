#' Rescale regression results by multiplying by 2 standard deviations
#'
#' \code{by_2sd} rescales regression results to facilitate making dot-and-whisker plots using \code{\link[dotwhisker]{dwplot}}.
#'
#' @param df A data.frame including the variables \code{term} (names of independent variables), \code{estimate} (corresponding coefficient estimates), \code{std.error} (corresponding standard errors), and optionally \code{model} (when multiple models are desired on a single plot) such as generated those by \code{\link[broom]{tidy}}.
#' @param dataset The data analyzed in the models whose results are recorded in df
#'
#' @details \code{by_2sd} multiplies the results from regression models saved as tidy data frames for predictors that are not binary by twice the standard deviation of these variables in the dataset analyzed.  Standardizing in this way yields coefficients that are directly comparable to those for untransformed binary predictors (Gelman 2008) and so facilitates plotting using \code{\link[dotwhisker]{dwplot}}.
#'
#' An alternative available in some circumstances is to pass a model object to \code{\link[arm]{standardize}} before passing the results to \code{\link[broom]{tidy}} and then on to \code{\link[dotwhisker]{dwplot}}.  The advantage of \code{by_2sd} is that it takes as its input is a tidy data.frame and so is not restricted to only those model objects that \code{standardize} accepts.
#'
#'
#' @return A tidy data.frame
#' @examples
#' library(broom)
#' library(dplyr)
#'
#' data(mtcars)
#' m1 <- lm(mpg ~ wt + cyl + disp, data = mtcars)
#' m1_df <- tidy(m1) %>% by_2sd(mtcars) # create data.frame of rescaled regression results
#'
#' @references
#' Gelman, Andrew. 2008. "Scaling Regression Inputs by Dividing by Two Standard Deviations." Statistics in Medicine, 27:2865-2873.
#'
#' @seealso \code{\link[arm]{standardize}}
#'
#' @import dplyr
#' @importFrom stats sd
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#'
#' @export

by_2sd <- function(df, dataset) {

  sdX2 <- df$term %>% as.list %>%
      lapply(function(x) {
          if(str_detect(x, ":")) {
              first <- str_replace(x, ":.*", "")
              second <- str_replace(x, ".*:", "")
              dataset[[paste0(first,":",second)]] <- dataset[[first]]*dataset[[second]]
          }
          unmatched <- !x %in% names(dataset)
          dich <- ifelse(unmatched, TRUE,
                         unique(dataset[[x]])[!is.na(unique(dataset[[x]]))] %>%
                             sort %>% identical(c(0,1)))
          ifelse(any(dich, unmatched), 1, 2*stats::sd(dataset[[x]], na.rm=T))
      }) %>% unlist

  df$estimate <- df$estimate * sdX2
  df$std.error <- df$std.error * sdX2
  return(df)
}
