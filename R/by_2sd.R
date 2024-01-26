#' Rescale regression results by multiplying by 2 standard deviations
#'
#' \code{by_2sd} rescales regression results to facilitate making dot-and-whisker plots using \code{\link[dotwhisker]{dwplot}}.
#'
#' @param df A data frame including the variables \code{term} (names of independent variables), \code{estimate} (corresponding coefficient estimates), \code{std.error} (corresponding standard errors), and optionally \code{model} (when multiple models are desired on a single plot) such as generated those by \code{\link[broom]{tidy}}.
#' @param dataset The data analyzed in the models whose results are recorded in \code{df}, or (preferably) the \emph{model matrix} used by the models in \code{df}; the information required for complex models can more easily be generated from the model matrix than from the original data set. In many cases the model matrix can be extracted from the original model via \code{\link{model.matrix}}.
#'
#' @details \code{by_2sd} multiplies the results from regression models saved as tidy data frames for predictors that are not binary by twice the standard deviation of these variables in the dataset analyzed. Standardizing in this way yields coefficients that are directly comparable to each other and to those for untransformed binary predictors (Gelman 2008) and so facilitates plotting using \code{\link[dotwhisker]{dwplot}}. Note that the current version of \code{by_2sd} does not subtract the mean (in contrast to Gelman's (2008) formula). However, all estimates and standard errors of the independent variables are the same as if the mean was subtracted. The only difference from Gelman (2008) is that for all variables in the model the intercept is shifted by the coefficient times the mean of the variable.
#'
#' An alternative available in some circumstances is to pass a model object to \code{arm::standardize} before passing the results to \code{\link[broom]{tidy}} and then on to \code{\link[dotwhisker]{dwplot}}.  The advantages of \code{by_2sd} are that (1) it takes a tidy data frame as its input and so is not restricted to only those model objects that \code{standardize} accepts and (2) it is much more efficient because it operates on the parameters rather than refitting the original model with scaled data.
#'
#'
#' @return A tidy data frame
#' @examples
#' library(broom)
#' library(dplyr)
#'
#' data(mtcars)
#' m1 <- lm(mpg ~ wt + cyl + disp, data = mtcars)
#' m1_df <- tidy(m1) %>% by_2sd(mtcars) # create data frame of rescaled regression results
#'
#' @references
#' Gelman, Andrew. 2008. "Scaling Regression Inputs by Dividing by Two Standard Deviations." Statistics in Medicine, 27:2865-2873.
#'
#' @importFrom dplyr "%>%"
#' @importFrom stats sd
#'
#' @export

by_2sd <- function(df, dataset) {
    if (!"by_2sd" %in% names(df)) {
        sdX2 <- df$term %>%
            as.list() %>%
            lapply(function(x) {
                ## FIXME: meaningful error message if names not found?
                ## (e.g. some kind of model object for which model.matrix()
                ## is not available, and we have non-standard contrasts+interactions) ?
                if(any(grep(":", x)) && !x %in% names(dataset)) {
                    ## find interactions; create interaction column if necessary
                    first <- gsub(":.*", "", x)
                    second <- gsub(".*:", "", x)
                    dataset[[paste0(first,":",second)]] <- dataset[[first]]*dataset[[second]]
                }
                unmatched <- !x %in% names(dataset)
                dx <- dataset[[x]]
                ## dichotomous/unmatched variable?
                dich <- (unmatched ||
                         stats::na.omit(unique(dx)) %>% sort() %>% identical(c(0,1)))
                if (dich) 1 else 2*stats::sd(dataset[[x]], na.rm=TRUE)
            }) %>%
            unlist()

        df$estimate <- df$estimate * sdX2
        df$std.error <- df$std.error * sdX2
        if ("conf.high" %in% names(df)) {
            df$conf.high <- df$conf.high * sdX2
            df$conf.low <- df$conf.low * sdX2
        }
        df$by_2sd <- TRUE
    }
  return(df)
}
