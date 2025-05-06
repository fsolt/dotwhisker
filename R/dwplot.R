#' Dot-and-Whisker Plots of Regression Results
#'
#' \code{dwplot} is a function for quickly and easily generating dot-and-whisker plots of regression models saved in tidy data frames.
#'
#' @param x Either a model object to be tidied with \code{\link[broom]{tidy}}, or a list of such model objects, or a tidy data frame of regression results (see 'Details').
#' @param ci A number indicating the level of confidence intervals; the default is .95.
#' @param dodge_size A number indicating how much vertical separation should be between different models' coefficients when multiple models are graphed in a single plot.  Lower values tend to look better when the number of independent variables is small, while a higher value may be helpful when many models appear on the same plot; the default is 0.4.
#' @param vars_order A vector of variable names that specifies the order in which the variables are to appear along the y-axis of the plot. Note that the order will be overwritten by \code{\link[dotwhisker]{relabel_predictors}}, if the function is following called.
#' @param show_intercept A logical constant indicating whether the coefficient of the intercept term should be plotted. The default is \code{FALSE}.
#' @param show_stats A logical constant indicating whether to show a table of model fitness statistics under the dot-whisker plot. The default is \code{TRUE}.
#' @param stats_tb Customized table of model fit. The table should be in a \code{data.frame}.
#' @param stats_digits A numeric value specifying the digits to display in the fitness table. This parameter is relevant only when \code{show_stats = TRUE}. Default is 3, providing a balance between precision and readability.
#' @param stats_compare A logical constant to enable comparison of statistics in the fitness table. Applicable only when \code{show_stats = TRUE}. The default value is \code{FALSE}. That is, it presents all the statistics across different modeling methods, yet potentially expanding the table's breadth. When set to \code{TRUE}, only the shared, comparable statistics are remained.
#' @param stats_verbose A logical constant to turn on/off the toggle warnings and messages of model fits. The default is \code{FALSE}.
#' @param stats_size A numeric value determining the font size in the fitness table, effective only if \code{show_stats = TRUE}. The standard setting is 10.
#' @param stats_padding Defining the internal margins of the fitness table. Relevant when \code{show_stats = TRUE}. Set by default to \code{unit(c(4, 4), "mm")}, allowing for a balanced layout. Further customization options refer to \code{\link[gridExtra]{tableGrob}}.
#' @param stats_layout Adjusting the spacing between the dotwhisker plot and the fitness table. Effective when \code{show_stats = TRUE}. The initial configuration is \code{c(2, -1, 1)}, ensuring a coherent visual flow. Additional layout settings refer to \code{\link[patchwork]{plot_layout}}.
#' @param margins A logical value indicating whether presenting the average marginal effects of the estimates. See the Details for more information.
#' @param model_name The name of a variable that distinguishes separate models within a tidy data frame.
#' @param model_order A character vector defining the order of the models when multiple models are involved.
#' @param style Either \code{"dotwhisker"} or \code{"distribution"}. \code{"dotwhisker"}, the default, shows the regression coefficients' point estimates as dots with confidence interval whiskers.  \code{"distribution"} shows the normal distribution with mean equal to the point estimate and standard deviation equal to the standard error, underscored with a confidence interval whisker.
#' @param by_2sd When x is model object or list of model objects, should the coefficients for predictors that are not binary be rescaled by twice the standard deviation of these variables in the dataset analyzed, per Gelman (2008)?  Defaults to \code{FALSE}.  Note that when x is a tidy data frame, one can use \code{\link[dotwhisker]{by_2sd}} to rescale similarly.
#' @param vline A \code{geom_vline()} object, typically with \code{xintercept = 0}, to be drawn behind the coefficients.
#' @param dot_args When \code{style} is "dotwhisker", a list of arguments specifying the appearance of the dots representing mean estimates.  For supported arguments, see \code{\link[ggplot2]{geom_point}}.
#' @param whisker_args When \code{style} is "dotwhisker", a list of arguments specifying the appearance of the whiskers representing the confidence intervals.  For supported arguments, see \code{\link[ggstance]{geom_linerangeh}}.
#' @param dist_args When \code{style} is "distribution", a list of arguments specifying the appearance of normally distributed regression estimates. For supported arguments, see \code{\link[ggplot2]{geom_polygon}}.
#' @param line_args When \code{style} is "distribution", a list of arguments specifying the appearance of the line marking the confidence interval beneath the normal distribution.  For supported arguments, see \code{\link[ggstance]{geom_linerangeh}}.
#' @param \dots Extra arguments to pass to \code{\link[parameters]{parameters}}.
#'
#' @details \code{dwplot} visualizes regression model objects or regression results saved in tidy data frames as dot-and-whisker plots generated by \code{\link[ggplot2]{ggplot}}.
#'
#' Tidy data frames to be plotted should include the variables \code{term} (names of predictors), \code{estimate} (corresponding estimates of coefficients or other quantities of interest), \code{std.error} (corresponding standard errors), and optionally \code{model} (when multiple models are desired on a single plot; a different name for this last variable may be specified using the model_name argument).
#' In place of \code{std.error} one may substitute \code{conf.low} (the lower bounds of the confidence intervals of each estimate) and \code{conf.high} (the corresponding upper bounds).
#'
#' For convenience, \code{dwplot} also accepts as input those model objects that can be tidied by \code{\link[broom]{tidy}} (or \code{\link[parameters]{parameters}} (with proper formatting)), or a list of such model objects.
#'
#' By default, the plot will display 95-percent confidence intervals. To display a different interval when passing a model object or objects, specify a \code{ci} argument. When passing a data frame of results, include the variables \code{conf.low} and \code{conf.high} describing the bounds of the desired interval.
#'
#' Because the function can take a data frame as input, it is easily employed for a wide range of models, including those not supported by \code{broom} or \code{parameters}.
#' And because the output is a \code{ggplot} object, it can easily be further customized with any additional arguments and layers supported by \code{ggplot2}.
#' Together, these two features make \code{dwplot} extremely flexible.
#'
#' \code{dwplot} provides an option to present the average marginal effect directly. Users can alter the confidence intervals of the margins through the \code{ci} argument. The `margins` argument also works for \code{small_multiple} and \code{secret_weapon}. Note that for models with categorical outcome variables (such as ordered logit or multinomial regression), each category (level) has its own average marignal effects. To maintain visualization consistency with models with continuous or binary outcomes, \code{dwplot} only displays marginal effects for the first category.
#'
#' To minimize the need for lengthy, distracting regression tables (often relegated to an appendix for dot-whisker plot users), \code{dwplot} incorporates optimal model fit statistics directly beneath the dot-whisker plots. These statistics are derived using the excellent \code{\link[performance]{performance}} functions and integrated at the plot's base via \code{\link[patchwork]{patchwork}} and \code{\link[gridExtra]{tableGrob}} functions. For added flexibility, \code{dwplot} includes the \code{stats_tb} feature, allowing users to input customized statistics. Furthermore, a suite of \code{stats_*} functions is available for fine-tuning the presentation of these statistics, enhancing user control over the visual output.
#'
#' @references
#' Kastellec, Jonathan P. and Leoni, Eduardo L. 2007. "Using Graphs Instead of Tables in Political Science." *Perspectives on Politics*, 5(4):755-771.
#'
#' Gelman, Andrew. 2008. "Scaling Regression Inputs by Dividing by Two Standard Deviations." *Statistics in Medicine*, 27:2865-2873.
#'
#' @return The function returns a \code{ggplot} object.
#'
#' @import ggplot2
#' @import patchwork
#' @import performance
#' @import marginaleffects
#' 
#' @importFrom parameters parameters standardize_names
#' @importFrom dplyr "%>%" n filter arrange left_join full_join bind_rows group_by if_else mutate distinct relocate ends_with across where
#' @importFrom stats qnorm reorder model.matrix dnorm model.frame nobs
#' @importFrom ggstance geom_pointrangeh position_dodgev GeomLinerangeh
#' @importFrom purrr map list_c list_rbind
#' @importFrom utils modifyList globalVariables
#' @importFrom gridExtra tableGrob ttheme_default
#'
#' @examples
#' library(dplyr)
#' # Plot regression coefficients from a single model object
#' data(mtcars)
#' m1 <- lm(mpg ~ wt + cyl + disp, data = mtcars)
#' dwplot(m1, vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) +
#'     xlab("Coefficient")
#' # using 99% confidence interval
#' dwplot(m1, ci = .99)
#' # Plot regression coefficients from multiple models
#' m2 <- update(m1, . ~ . - disp)
#' dwplot(list(full = m1, nodisp = m2))
#' # Change the appearance of dots and whiskers
#' dwplot(m1, dot_args = list(size = 3, pch = 21, fill = "white"))
#' # Plot regression coefficients from multiple models on the fly
#' mtcars %>%
#'     split(.$am) %>%
#'     purrr::map(~ lm(mpg ~ wt + cyl + disp, data = .x)) %>%
#'     dwplot() %>%
#'     relabel_predictors(c(wt = "Weight", cyl = "Cylinders", disp = "Displacement")) +
#'     theme_bw() + xlab("Coefficient") + ylab("") +
#'     geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
#'     ggtitle("Predicting Gas Mileage, OLS Estimates") +
#'     theme(plot.title = element_text(face = "bold"),
#'           legend.position = c(.995, .99),
#'           legend.justification = c(1, 1),
#'           legend.background = element_rect(colour="grey80"),
#'           legend.title.align = .5) +
#'     scale_colour_grey(start = .4, end = .8,
#'                       name = "Transmission",
#'                       breaks = c("Model 0", "Model 1"),
#'                       labels = c("Automatic", "Manual"))
#'
#' @export

dwplot <- function(x,
                ci = .95,
                dodge_size = .4,
                vars_order = NULL,
                show_intercept = FALSE,
                show_stats = FALSE,
                stats_tb = NULL,
                stats_digits = 3,
                stats_compare = FALSE,
                stats_verbose = FALSE,
                stats_size = 10,
                stats_padding = unit(c(4, 4), "mm"),
                stats_layout = c(2, -1, 1),
                margins = FALSE,
                model_name = "model",
                model_order = NULL,
                style = c("dotwhisker", "distribution"),
                by_2sd = FALSE,
                vline = NULL,
                dot_args = list(size = 1.2),
                whisker_args = list(size = .5),
                dist_args = list(alpha = .5),
                line_args = list(alpha = .75, size = 1),
                ...) {
    # argument checks
    if (length(style) > 1)
        style <- style[[1]]
    if (!style %in% c("dotwhisker", "distribution"))
        stop("style must be dotwhisker or distribution")

    # If x is model object(s), convert to a tidy data frame
    df <- dw_tidy(x, ci, by_2sd, margins, ...)

    if (!show_intercept)
        df <-
        df %>% filter(!grepl("^\\(Intercept\\)$|^\\w+\\|\\w+$", term)) # enable detecting intercept in polr objects

    n_vars <- length(unique(df$term))
    dodge_size <- dodge_size

    # Confirm number of models, get model names
    if (model_name %in% names(df)) {
        dfmod <- df[[model_name]]
        n_models <- length(unique(dfmod))
        l_models <- if(is.null(model_order)) unique(dfmod) else model_order
        ## re-order/restore levels by order in data set
        df[[model_name]] <- factor(dfmod, levels = rev(l_models))
    } else {
        if (length(df$term) == n_vars) {
            df[[model_name]] <- factor("one")
            n_models <- 1
        } else {
            stop(
                "Please add a variable named '",
                model_name,
                "' to distinguish different models"
            )
        }
    }
    mod_names <- unique(df[[model_name]])

    # Specify order of variables if an order is provided
    if (!is.null(vars_order)) {
        df$term <- factor(df$term, levels = vars_order)
        df <- df[order(df$term),] %>% filter(!is.na(term))
    }

    # Add rows of NAs for variables not included in a particular model
    if (n_models > 1) {
        df <- add_NAs(df, n_models, mod_names)
    }

    # Prep arguments to ggplot
    var_names <- unique(df$term)

    df <- df %>%
        mutate(y_ind = n_vars - as.numeric(factor(term, levels = var_names)) + 1)

    y_ind <- df$y_ind

    # Make the plot
    if (style == "distribution") {
        if (nrow(df) > n_models * n_vars) {
            # reset df if it was passed by relabel_predictors
            df <- df %>%
                select(-n,-loc,-dens) %>%
                distinct()
        }

        df1 <- purrr::map_dfr(1:101, function(x)
            df) %>%
            arrange(term, model) %>%
            group_by(term, model) %>%
            dplyr::mutate(
                n = 1:dplyr::n(),
                loc = estimate - 3 * std.error + (6 * std.error) /
                    100 * (n - 1),
                dens = dnorm(loc, mean = estimate, sd = std.error) + y_ind
            ) %>%
            filter(!is.na(estimate))

        p <- ggplot(data = df) +
            vline +
            geom_dwdist(df1 = df1,
                        line_args = line_args,
                        dist_args = dist_args) +
            scale_y_continuous(breaks = unique(df$y_ind), labels = var_names) +
            guides(color = guide_legend(reverse = TRUE),
                fill = guide_legend(reverse = TRUE)) +
            ylab("") + xlab("")

    } else {
        # style == "dotwhisker"
        point_args0 <- list(na.rm = TRUE)
        point_args <- c(point_args0, dot_args)
        segment_args0 <- list(na.rm = TRUE)
        segment_args <- c(segment_args0, whisker_args)

        p <- ggplot(data = df) +
            vline +
            geom_dw(
                df = df,
                point_args = point_args,
                segment_args = segment_args,
                dodge_size = dodge_size
            ) +
            guides(color = guide_legend(reverse = TRUE)) +
            ylab("") + xlab("")
    }

    # Omit the legend if there is only one model
    if (n_models == 1) {
        p <- p + theme(legend.position = "none")
    }


    p$args <- list(
        dodge_size = dodge_size,
        vars_order = vars_order,
        show_intercept = show_intercept,
        model_name = model_name,
        model_order = model_order,
        style = style,
        by_2sd = FALSE,
        vline = vline,
        dot_args = dot_args,
        whisker_args = whisker_args,
        dist_args = dist_args,
        line_args = line_args,
        list(...)
    )

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

dw_tidy <- function(x, ci, by_2sd, margins,...) {

    ## return model matrix *or* model frame
    get_dat <- function(x) {
        tryCatch(
            as.data.frame(model.matrix(x)),
            error = function(e)
                model.frame(x)
        )
    }
    ## prepend "Model" to numeric-convertable model labels
    mk_model <- function(x) {
        if (all(!is.na(suppressWarnings(as.numeric(x))))) {
            paste("Model", x)
        } else
            x
    }

    if (!is.data.frame(x)) {
        if (!inherits(x, "list")) {
            if(margins){
                df <- avg_slopes(x, conf_level = ci)
                if(!is.null(df$group)) df <- df[!duplicated(df$term), ] # used the marginal effects for the first category of y
            }else{
                df <- standardize_names(parameters(x, ci, conf.int = TRUE, ...), style = "broom")
            }

            if (by_2sd) {
                df <- df %>% by_2sd(get_dat(x))
            }
        } else {
            # list of models
            if (by_2sd) {
                if(is.null(names(x))) names(x) <- paste0("Model ", seq(x))
                df <- purrr::map(x, \(x){
                    if(margins){
                                            df <- avg_slopes(x, conf_level = ci)
                                            if(!is.null(df$group)) df <- df[!duplicated(df$term), ] # used the marginal effects for the first category of y
                    }else{
                                            df <- standardize_names(parameters(x, ci, conf.int = TRUE, ...), style = "broom")
                                        }
                                        dotwhisker::by_2sd(df, dataset = get_dat(x))
                }) |> 
                    list_rbind(names_to = "model")
            } else {
                if(is.null(names(x))) names(x) <- paste0("Model ", seq(x))
                df <- purrr::map(x, \(x){
                    if(margins){
                                result <- avg_slopes(x, conf_level = ci)
                                if(!is.null(df$group)) result <- result[!duplicated(result$term), ] # used the marginal effects for the first category of y
                    }else{
                                df <- standardize_names(parameters(x, ci, conf.int = TRUE, ...), style = "broom")
                            }
                }) |> 
                    list_rbind(names_to = "model")
            }
        }
    } else {
        # x is a dataframe
        df <- x
        if ((!"conf.low" %in% names(df)) ||
            (!"conf.high" %in% names(df))) {
            if ("std.error" %in% names(df)) {
                df <- transform(
                    df,
                    conf.low = estimate - stats::qnorm(1 - (1 - ci) / 2) * std.error,
                    conf.high = estimate + stats::qnorm(1 - (1 - ci) / 2) * std.error
                )
            } else {
                df <- transform(df,
                                conf.low = NA,
                                conf.high = NA)
            }
        }
    }
    return(df)
}

dw_stats <- function(x,
                    stats_digits,
                    stats_compare = FALSE,
                    stats_verbose = FALSE) {
    N <- Name <- Model <- NULL

    if (!inherits(x, "list")) {
        # single model
        df_stats <- data.frame(N = nobs(x)) |>
            cbind(model_performance(x, metrics = "common", verbose = stats_verbose))
    } else {
        # multiple models
        df_stats <- map(x, nobs) |>
            list_c() |>
            data.frame(N = _) |>
            cbind(compare_performance(x, metrics = "common", verbose = stats_verbose)) |>
            relocate(N, .after = Name) |>
            select(-ends_with("_wt"), -Model)

        if (stats_compare)
            df_stats <-
                select(df_stats, where(\(var) ! any(is.na(var)))) # removed model special stats
    }

    df_stats <- mutate(df_stats,
                    across(where(is.numeric),
                            \(stats) round(stats, digits = stats_digits)))

    return(df_stats)
}

add_NAs <-function(df = df,
            n_models = n_models,
            mod_names = mod_names,
            model_name = "model") {
        # Set variables that will appear in pipelines to NULL to make R CMD check happy
        term <- model <- NULL

        if (!is.factor(df$term)) {
            df$term <- factor(df$term, levels = unique(df$term))
        }
        if (!is.factor(dfmod <- df[[model_name]])) {
            df[[model_name]] <- factor(dfmod, levels = unique(dfmod))
        }
        for (i in seq(n_models)) {
            m <-
                df %>% filter(model == factor(mod_names[[i]], levels = mod_names))
            not_in <- setdiff(unique(df$term), m$term) |> as.character() # if keeping the factor class, sometimes produce NA weirdly 

            for (j in seq(not_in)) {
                t <- data.frame(
                    term = factor(not_in[j], levels = levels(df$term)),
                    model = factor(mod_names[[i]], levels = mod_names)
                )
                if ("submodel" %in% names(m)) {
                    t$submodel <- m$submodel[1]
                }
                if ("submodel" %in% names(m)) {
                    m <- full_join(m, t, by = c("term", "model", "submodel"))
                } else {
                    m <- full_join(m, t, by = c("term", "model"))
                }
            }

            if (i == 1) {
                dft <- m %>% arrange(term)
            } else {
                dft <- bind_rows(dft, m %>% arrange(term))
            }
        }

        df <- dft

        df$estimate <- as.numeric(df$estimate)
        if ("std.error" %in% names(df)) {
            df$std.error <- as.numeric(df$std.error)
        }
        if ("conf.high" %in% names(df)) {
            df$conf.high <- as.numeric(df$conf.high)
        }
        if ("conf.low" %in% names(df)) {
            df$conf.low <- as.numeric(df$conf.low)
        }

        return(df)
    }

geom_dwdist <- function(data = NULL, df1, line_args, dist_args) {
    # Set variables to NULL to make R CMD check happy
    loc <-
        dens <- model <- term <- y_ind <- conf.high <- conf.low <- NULL

    l1 <- layer(
        data = df1,
        mapping = aes(
            x = loc,
            y = dens,
            group = interaction(model, term),
            color = model,
            fill = model
        ),
        stat = "identity",
        position = "identity",
        geom = GeomPolygon,
        params = dist_args
    )
    l2 <- layer(
        data = data,
        mapping = aes(
            y = y_ind,
            xmin = conf.low,
            xmax = conf.high,
            color = model
        ),
        stat = "identity",
        position = "identity",
        geom = ggstance::GeomLinerangeh,
        show.legend = FALSE,
        params = line_args
    )
    return(list(l1, l2))
}

geom_dw <- function(df, point_args, segment_args, dodge_size) {
    # Set variables to NULL to make R CMD check happy
    loc <-
        dens <-
        model <- term <- y_ind <- conf.high <- conf.low <- estimate <- NULL

    point_arguments <-
        tryCatch({
            added_point_aes <- point_args[names(point_args) == ""][[1]]
            point_mapping <-
                modifyList(
                    aes(
                        y = stats::reorder(term, y_ind),
                        x = estimate,
                        group = interaction(model, term),
                        color = model
                    ),
                    added_point_aes
                )
            point_arguments <- point_args[names(point_args) != ""]
            list(point_mapping, point_arguments)
        },
        error = function(e) {
            point_mapping <-
                aes(
                    y = stats::reorder(term, y_ind),
                    x = estimate,
                    group = interaction(model, term),
                    color = model
                )
            return(list(point_mapping, point_args))
        })

    segment_arguments <-
        tryCatch({
            added_segment_aes <- segment_args[names(segment_args) == ""][[1]]
            segment_mapping <-
                modifyList(
                    aes(
                        y = stats::reorder(term, y_ind),
                        xmin = conf.low,
                        xmax = conf.high,
                        group = interaction(model, term),
                        color = model
                    ),
                    added_segment_aes
                )
            segment_arguments <- segment_args[names(segment_args) != ""]
            list(segment_mapping, segment_arguments)
        },
        error = function(e) {
            segment_mapping <-
                aes(
                    y = stats::reorder(term, y_ind),
                    xmin = conf.low,
                    xmax = conf.high,
                    group = interaction(model, term),
                    color = model
                )
            return(list(segment_mapping, segment_args))
        })


    l1 <- layer(
        data = df,
        mapping = point_arguments[[1]],
        stat = "identity",
        position = ggstance::position_dodgev(height = dodge_size),
        geom = "point",
        params = point_arguments[[2]]
    )
    l2 <- layer(
        data = df,
        mapping = segment_arguments[[1]],
        stat = "identity",
        position = ggstance::position_dodgev(height = dodge_size),
        geom = ggstance::GeomLinerangeh,
        show.legend = FALSE,
        params = segment_arguments[[2]]
    )
    return(list(l2, l1))
}


#' @rdname dwplot
dw_plot <- dwplot
