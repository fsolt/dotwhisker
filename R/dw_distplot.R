#' Distribution Plots of Regression Results
#'
#' \code{dwplot} is a function for quickly and easily generating normal distribution plots of regression models.
#'
#' @param x Either a tidy data frame of rescaled regression results (see 'Details'), a model object to be tidied with \code{\link[broom]{tidy}}, or a list of such model objects.
#' @param order_vars A vector of variable names that specifies the order in which the variables are to appear along the y-axis of the plot.
#' @param show_intercept A logical constant indicating whether the coefficient of the intercept term should be plotted.
#' @param model_name The name of a variable that distinguishes separate models within a tidy data frame.
#' @param dist_args A list of arguments specifying the appearance of the normally distributed regression estimates.  For supported arguments, see \code{\link[ggplot2]{geom_polygon}}.
#' @param line_args A list of arguments specifying the appearance of the line beneath the normal distribution.  For supported arguments, see \code{\link[ggstance]{geom_linerangeh}}.
#' @param \dots Extra arguments to pass to \code{\link[broom]{tidy}}.
#'
#' @details \code{dw_distplot} visualizes regression model objects or regression results saved in tidy data frames by, e.g., \code{\link[broom]{tidy}} as plots of standardized normally distributed regression estimates, rescaled by twice the standard deviation of their respective variables in the analyzed dataset.  A line marking the width of the chosen confidence interval underlies the distribution.
#'
#' Tidy data frames to be plotted should include the variables \code{term} (names of predictors), \code{estimate} (corresponding estimates of coefficients or other quantities of interest), \code{std.error} (corresponding standard errors), and optionally \code{model} (when multiple models are desired on a single plot; a different name for this last variable may be specified using the model_name argument).
#' In place of \code{std.error} one may substitute \code{conf.low} (the lower bounds of the confidence intervals of each estimate) and \code{conf.high} (the corresponding upper bounds).  These results should be rescaled using the \code{\link[dotwhisker]{by_2sd}} function before being passed to \code{dw_distplot}.
#'
#' For convenience, \code{dw_distplot} also accepts as input those model objects that can be tidied by \code{\link[broom]{tidy}}, or a list of such model objects.
#'
#' Because the function can take a data frame as input, it is easily employed for a wide range of models, including those not supported by \code{\link[broom]{tidy}}.
#' And because the output is a \code{ggplot} object, it can easily be further customized with any additional arguments and layers supported by \code{ggplot2}.
#' Together, these two features make \code{dw_distplot} extremely flexible.
#'
#' @references
#' Kastellec, Jonathan P. and Leoni, Eduardo L. 2007. "Using Graphs Instead of Tables in Political Science." Perspectives on Politics, 5(4):755-771.
#'
#' @return The function returns a \code{ggplot} object.
#'
#' @import ggplot2
#' @importFrom broom tidy
#' @importFrom dplyr "%>%" filter arrange left_join full_join bind_rows group_by n mutate if_else
#' @importFrom stats qnorm
#' @importFrom stats reorder
#' @importFrom ggstance geom_pointrangeh
#' @importFrom ggstance position_dodgev
#' @importFrom purrr map_df
#' @importFrom stats dnorm model.frame
#'
#' @examples
#' library(broom)
#' library(dplyr)
#' # Plot regression coefficients from a single model object
#' data(mtcars)
#' m1 <- lm(mpg ~ wt + cyl + disp, data = mtcars)
#' dw_distplot(m1) +
#'     xlab("Coefficient") +
#'     geom_vline(xintercept = 0, colour = "grey50", linetype = 2)
#' # Plot regression coefficients from multiple models on the fly
#' m2 <- update(m1, . ~ . - disp)
#' dw_distplot(list(full = m1, nodisp = m2))
#' # Change the width and appearance of the line marking the confidence interval
#' dw_distplot(m1, alpha = .1, line_args = list(size = 2, colour = "yellow"))
#' # Plot regression coefficients from multiple models in a tidy data frame
#' by_trans <- mtcars %>% group_by(am) %>%
#'     do(tidy(lm(mpg ~ wt + cyl + disp + gear, data = .))) %>%
#'     by_2sd(mtcars) %>% rename(model=am) %>%
#'     relabel_predictors(c(wt = "Weight", cyl = "Cylinders", disp = "Displacement", gear = "Gears"))
#' dw_distplot(by_trans) +
#'     theme_bw() + xlab("Coefficient") + ylab("") +
#'     geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
#'     ggtitle("Predicting Gas Mileage, OLS Estimates") +
#'     theme(plot.title = element_text(face = "bold"),
#'           legend.position=c(.01, .05), legend.justification=c(0, 0),
#'           legend.background = element_rect(colour="grey80"),
#'           legend.title.align = .5) +
#'     scale_colour_grey(start = .4, end = .8,
#'                       name = "Transmission",
#'                       breaks = c(0, 1),
#'                       labels = c("Automatic", "Manual")) +
#'     scale_fill_grey(start = .4, end = .8,
#'                       name = "Transmission",
#'                       breaks = c(0, 1),
#'                       labels = c("Automatic", "Manual"))
#'
#' @export

dw_distplot <- function(x,
                        order_vars = NULL,
                        show_intercept = FALSE,
                        model_name = "model",
                        dist_args = list(alpha = .5),
                        line_args = list(alpha = .05, size = 1),
                        ...) {

    # Set variables that will appear in pipelines to NULL to make R CMD check happy
    estimate <- model <- conf.low <- conf.high <- term <- std.error <- n <- loc <- dens <- conf.low <- conf.high <- NULL

    df <- dw_tidy(x, ...)

    if (!show_intercept) df <- df %>% filter(!grepl("^\\(Intercept\\)$|^\\w+\\|\\w+$", term)) # enable detecting intercept in polr objects

    # Specify order of variables if an order is provided
    if (!is.null(order_vars)) {
        df$term <- factor(df$term, levels = order_vars)
        df <- df[match(order_vars, df$term),] %>% stats::na.omit()
    }

    n_vars <- length(unique(df$term))

    # Confirm number of models, get model names
    if (model_name %in% names(df)) {
        dfmod <- df[[model_name]]
        n_models <- length(unique(dfmod))
        ## re-order/restore levels by order in data set
        df[[model_name]] <- factor(dfmod, levels = unique(dfmod))
    } else {
        if (length(df$term) == n_vars) {
            df[[model_name]] <- factor("one")
            n_models <- 1
        } else {
            stop("Please add a variable named '",
                 model_name,"' to distinguish different models")
        }
    }
    mod_names <- unique(df[[model_name]])

    # Add rows of NAs for variables not included in a particular model
    if (n_models > 1) {
        df <- add_NAs(df, n_models, mod_names)
    }

    # Prep arguments to ggplot
    var_names <- df$term

    y_ind <- rep(seq(n_vars, 1), n_models)
    df$y_ind <- y_ind

    # Catch difference between single and multiple models
    if (length(y_ind) != length(var_names)) {
        var_names <- unique(var_names)
    }

    df1 <- purrr::map_df(1:101, function(x) df) %>%
        arrange(term, model) %>%
        group_by(term, model) %>%
        dplyr::mutate(n = 1:n(),
                      loc = estimate - 3 * std.error + (6 * std.error)/100 * (n - 1),
                      dens = dnorm(loc, mean = estimate, sd = std.error) + y_ind) %>%
        filter(!is.na(estimate))

    line_args0 <- list(y = df1$y_ind)
    line_args1 <- c(line_args0, line_args)

    p <- ggplot(df1, aes(x = loc, y = dens, xmin = conf.low, xmax = conf.high,
                    fill = model, color = model, group = interaction(model, term))) +
        do.call(geom_polygon, dist_args) +
        do.call(ggstance::geom_linerangeh, line_args1) +
        scale_y_continuous(breaks = y_ind, labels = var_names) +
        ylab("") + xlab("")

    # Omit the legend if there is only one model
    if (n_models == 1) {
        p <- p + theme(legend.position = "none")
    }

    return(p)
}
