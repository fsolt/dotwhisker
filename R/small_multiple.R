#' Generate a 'Small Multiple' Plot of Regression Results
#'
#' \code{small_multiple} is a function for plotting regression results of multiple models as a 'small multiple' plot
#'
#' @param x Either a model object to be tidied with \code{\link[broom]{tidy}}, or a list of such model objects, or a tidy data frame of regression results (see 'Details').
#' @param ci A number indicating the level of confidence intervals; the default is .95.
#' @param margins [Suspended] A logical value indicating whether presenting the average marginal effects of the estimates. See the Details for more information.
#' @param dodge_size A number (typically between 0 and 0.3; the default is .06) indicating how much horizontal separation should appear between different submodels' coefficients when multiple submodels are graphed in a single plot.  Lower values tend to look better when the number of models is small, while a higher value may be helpful when many submodels appear on the same plot.
#' @param show_intercept A logical constant indicating whether the coefficient of the intercept term should be plotted.
#' @param show_stats A logical constant indicating whether to show a table of model fitness statistics under the dot-whisker plot. The default is \code{TRUE}.
#' @param stats_tb Customized table of model fitness. The table should be in a \code{data.frame}.
#' @param stats_digits A numeric value specifying the digits to display in the fitness table. This parameter is relevant only when \code{show_stats = TRUE}. Default is 3, providing a balance between precision and readability.
#' @param stats_compare A logical constant to enable comparison of statistics in the fitness table. Applicable only when \code{show_stats = TRUE}. The default value is \code{FALSE}. That is, it presents all the statistics across different modeling methods, yet potentially expanding the table's breadth. When set to \code{TRUE}, only the shared, comparable statistics are remained.
#' @param stats_verbose A logical constant to turn on/off the toggle warnings and messages of model fits. The default is \code{FALSE}.
#' @param stats_size A numeric value determining the font size in the fitness table, effective only if \code{show_stats = TRUE}. The standard setting is 10.
#' @param stats_padding Defining the internal margins of the fitness table. Relevant when \code{show_stats = TRUE}. Set by default to \code{unit(c(4, 4), "mm")}, allowing for a balanced layout. Further customization options refer to \code{\link[gridExtra]{tableGrob}}.
#' @param stats_layout Adjusting the spacing between the dotwhisker plot and the fitness table. Effective when \code{show_stats = TRUE}. The initial configuration is \code{c(2, -1, 1)}, ensuring a coherent visual flow. Additional layout settings refer to \code{\link[patchwork]{plot_layout}}.
#' @param dot_args A list of arguments specifying the appearance of the dots representing mean estimates.  For supported arguments, see \code{\link[ggstance]{geom_pointrangeh}}.
#' @param model_order A character vector defining the order of the models when multiple models are involved.
#' @param submodel_order A character vector defining the order of the submodels when multiple submodels are involved.
#' @param axis_switch A logical constant indicating the position of variable labels and y axis ticks. Default is FALSE, when the variable label is on the right side, and y axis ticks is on the left size.
#' @param by_2sd When x is model object or list of model objects, should the coefficients for predictors that are not binary be rescaled by twice the standard deviation of these variables in the dataset analyzed, per Gelman (2008)?  Defaults to \code{TRUE}.  Note that when x is a tidy data frame, one can use \code{\link[dotwhisker]{by_2sd}} to rescale similarly.
#' @param \dots Arguments to pass to \code{\link[dotwhisker]{dwplot}}.
#'
#' @details
#' \code{small_multiple}, following \href{https://www.cambridge.org/core/journals/perspectives-on-politics/article/using-graphs-instead-of-tables-in-political-science/9FD63E9EE686AF046732191EE8A68034}{Kastellec and Leoni (2007)}, provides a compact means of representing numerous regression models in a single plot.
#'
#' Tidy data frames to be plotted should include the variables \code{term} (names of predictors), \code{estimate} (corresponding estimates of coefficients or other quantities of interest), \code{std.error} (corresponding standard errors), and \code{model} (identifying the corresponding model).
#' In place of \code{std.error} one may substitute \code{conf.low} (the lower bounds of the confidence intervals of each estimate) and \code{conf.high} (the corresponding upper bounds).
#'
#' Alternately, \code{small_multiple} accepts as input a list of model objects that can be tidied by \code{\link[broom]{tidy}} (or \code{\link[parameters]{parameters}} (with proper formatting)), or a list of such model objects.
#'
#' Optionally, more than one set of results can be clustered to facilitate comparison within each \code{model}; one example of when this may be desirable is to compare results across samples.  In that case, the data frame should also include a variable \code{submodel} identifying the submodel of the results.
#'
#' To minimize the need for lengthy, distracting regression tables (often relegated to an appendix for dot-whisker plot users), \code{dwplot} incorporates optimal model fit statistics directly beneath the dot-whisker plots. These statistics are derived using the excellent \code{\link[performance]{performance}} functions and integrated at the plot's base via \code{\link[patchwork]{patchwork}} and \code{\link[gridExtra]{tableGrob}} functions. For added flexibility, \code{dwplot} includes the \code{stats_tb} feature, allowing users to input customized statistics. Furthermore, a suite of \code{stats_*} functions is available for fine-tuning the presentation of these statistics, enhancing user control over the visual output.
#'
#' @references
#' Kastellec, Jonathan P. and Leoni, Eduardo L. 2007. "Using Graphs Instead of Tables in Political Science." *Perspectives on Politics*, 5(4):755-771.
#'
#' @return The function returns a \code{ggplot} object.
#'
#' @examples
#' library(broom)
#' library(dplyr)
#'
#' # Generate a tidy data frame of regression results from six models
#'
#' m <- list()
#' ordered_vars <- c("wt", "cyl", "disp", "hp", "gear", "am")
#' m[[1]] <- lm(mpg ~ wt, data = mtcars)
#' m123456_df <- m[[1]] %>% tidy %>% by_2sd(mtcars) %>%
#'   mutate(model = "Model 1")
#'
#' for (i in 2:6) {
#'  m[[i]] <- update(m[[i-1]], paste(". ~ . +", ordered_vars[i]))
#'  m123456_df <- rbind(m123456_df, m[[i]] %>% tidy %>% by_2sd(mtcars) %>%
#'    mutate(model = paste("Model", i)))
#' }
#'
#' # Generate a 'small multiple' plot
#' small_multiple(m123456_df)
#'
#'
#' ## Using submodels to compare results across different samples
#' # Generate a tidy data frame of regression results from five models on
#' # the mtcars data subset by transmission type (am)
#' ordered_vars <- c("wt", "cyl", "disp", "hp", "gear")
#' mod <- "mpg ~ wt"
#' by_trans <- mtcars %>% group_by(am) %>%  # group data by transmission
#'   do(tidy(lm(mod, data = .))) %>%        # run model on each group
#'   rename(submodel = am) %>%              # make submodel variable
#'   mutate(model = "Model 1") %>%          # make model variable
#'  ungroup()
#'
#' for (i in 2:5) {
#'    mod <- paste(mod, "+", ordered_vars[i])
#'    by_trans <- rbind(by_trans, mtcars %>% group_by(am) %>%
#'                          do(tidy(lm(mod, data = .))) %>%
#'                          rename(submodel = am) %>%
#'                          mutate(model = paste("Model", i)) %>%
#'                          ungroup())
#' }
#'
#' small_multiple(by_trans) +
#' theme_bw() + ylab("Coefficient Estimate") +
#'     geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
#'     theme(axis.text.x  = element_text(angle = 45, hjust = 1),
#'           legend.position=c(0, 0), legend.justification=c(0, 0),
#'           legend.title = element_text(size=9),
#'           legend.background = element_rect(color="gray90"),
#'           legend.spacing = unit(-3, "pt"),
#'           legend.key.size = unit(10, "pt")) +
#'     scale_colour_hue(name = "Transmission",
#'     breaks = c(0, 1),
#'     labels = c("Automatic", "Manual"))
#'
#' @importFrom dplyr "%>%" filter
#' @importFrom stringr str_replace
#' @importFrom utils globalVariables
#'
#' @export

small_multiple <- function(x,
                           ci = .95,
                           margins = FALSE,
                           dodge_size = .4,
                           show_intercept = FALSE,
                           show_stats = FALSE,
                           stats_tb = NULL,
                           stats_digits = 3,
                           stats_compare = FALSE,
                           stats_verbose = FALSE,
                           stats_size = 10,
                           stats_padding = unit(c(4, 4), "mm"),
                           stats_layout = c(2, -1, 1),
                           model_order = NULL,
                           submodel_order = NULL,
                           axis_switch = FALSE,
                           by_2sd = FALSE,
                           dot_args = list(size = .3),
                           ...) {
    # If x is list of model objects, convert to a tidy data frame
    df <- dw_tidy(x, ci, by_2sd, margins, ...)

    # Drop intercept if show_intercept = FALSE
    if (!show_intercept) df <- df %>% filter(!grepl("^\\(Intercept\\)$|^\\w+\\|\\w+$", term)) # enable detecting intercept in polr objects

    # Set variables that will appear in pipelines to NULL to make R CMD check happy
    term <- estimate <- model <- submodel <- conf.high <- conf.low <- NULL

    n_vars <- length(unique(df$term))

    # Confirm number of models and submodels, get model names
    if (!"model" %in% names(df)) {
        if (length(df$term) == n_vars) {
            stop("'Small multiple' plots are used to compare results across many different models; please submit results from more than one model")
        } else {
            stop("Please add a variable named 'model' to distinguish different models")
        }
    } else {
        if ("submodel" %in% names(df)) {
            if (!is.factor(df$submodel)) {
                df$submodel <- factor(df$submodel, levels = unique(df$submodel))
            }
            df$mod <- df$model
            df$model <- paste0(df$model, df$submodel)
            sub_names <- unique(df$submodel)
            n_sub <- length(sub_names)
        } else {
            df$submodel <- 1
            n_sub <- 1
        }
    }
    mod_names <- unique(df$model)
    n_models <- length(mod_names)

    # Add rows of NAs for variables not included in a particular model
    df <- add_NAs(df, n_models, mod_names)
    if (n_sub > 1) {
        df$model <- stringr::str_replace(df$model, as.character(df$submodel), "")
        mod_names <- unique(df$model)
        n_models <- length(mod_names)
    }

    # Calculate x-axis shift for plotting multiple submodels, generate x index
    if (n_sub == 1) {
        df$shift <- 0
    } else {
        shift <- seq(-dodge_size, dodge_size, length.out = n_sub)
        df$shift <- rep(rep(shift, each = n_vars), times = n_models)
    }
    x_ind <- rep(seq(1, n_models), each = n_vars*n_sub)
    df$x_ind <- x_ind

    # Catch difference between single and multiple submodels
    if (length(x_ind) != length(mod_names)) {
        x_ind <- unique(x_ind)
    }

    point_args0 <- list(na.rm = TRUE, position=position_dodge(width = dodge_size))
    point_args <- c(point_args0, dot_args)


    # Model order
    if (!is.null(model_order)) df <- mutate(df, model = factor(model, levels = model_order))
    if(!is.null(submodel_order)) df <- mutate(df, submodel = factor(submodel, levels = submodel_order))


    # Plot
    if (axis_switch) {
        p <-
            ggplot(
                df,
                aes(
                    y = estimate,
                    ymin = conf.low,
                    ymax = conf.high,
                    x = as.factor(model),
                    colour = submodel
                )
            ) +
            do.call(geom_pointrange, point_args) +
            ylab("") + xlab("") +
            facet_grid(term ~ .,
                       scales = "free_y",
                       switch = "y") +
            scale_y_continuous(position = "right")
    } else {
        p <-
            ggplot(
                df,
                aes(
                    y = estimate,
                    ymin = conf.low,
                    ymax = conf.high,
                    x = as.factor(model),
                    colour = submodel
                )
            ) +
            do.call(geom_pointrange, point_args) +
            ylab("") + xlab("") +
            facet_grid(term ~ ., scales = "free_y")
    }



    if (n_sub == 1) {
        p <- p + theme(legend.position="none")
    }

    # Adding the stats
    if(show_stats){
        df_stats <- stats_tb

        if(is.null(df_stats)){ # No customized df_stats input
            df_stats <- dw_stats(x, stats_digits = stats_digits, stats_compare = stats_compare, stats_verbose = stats_verbose)
        } else {
            if(!is.data.frame(df_stats)) stop("The customized fitness table has to be a data.frame.")
        }

        p <- p / tableGrob(df_stats, rows = NULL,
                           theme = ttheme_default(base_size = stats_size)) +
            plot_layout(heights = stats_layout) # remove the space between the plot and table
    }

    return(p)
}
