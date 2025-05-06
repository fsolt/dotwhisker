## Version 0.8.4

- Bring the `margin` function back based on Vincent Arel-Bundock's `marginaleffects`.

## Version 0.8.3

- Add argument to turn on/off the toggle warnings and messages when showing the model fit table.
- Fix error from CRAN

## Version 0.8.2

- Submitted just for the upstream package (`prediction`) issue.
- Removed `margins` for the author would not maintain or update it in the foreseeable future.

## Version 0.8.1

Continue to remove the reference to `broomExtra` per CRAN's requirement (email on 2024-02-19)

## Version 0.8.0

### Bug fixed

1. Removed the reference to `broomExtra` per CRAN's requirement (email on 2024-01-23)

### New features

1. Adding the argument `show_stats` to `dwplot` and `small_multiple` to show model fits beneath the plot
2. Adding a hidden function `dw_stats` to extract model fits from the model outputs.

## Version 0.7.4

### Bug fixed

1. Allowing `model_order` accurately work when `relabel_predictors` is used.
2. Letting the vignette correctly shown.

## Version 0.7.3

### New features

1. Adding argument `model_order` and `submodel_order` in `small_multiple` to allow customizing the order of models to present.
2. Adding argument `axis_switch` in `small_multiple` to allow switching the positions of the variable labels and y axis ticks.

## Version 0.7.1

### Bug fixed

1. Fixed the error when setting `style = "distribution"`. Thanks for Indrajee @IndrajeetPatil pointing that out.

## Version 0.7.0

### New features

1. Adding argument `model_order` in `dwplot` to allow customizing the order of models to present.
2. Adding argument `fontSize` in `add_brackets` to allow customizing the font size of bracket labels, and opening possibility for users to further customize bracket labels.
3. Using the `parameters` instead of `broomExtra` as the plotting data frame creator. Thanks for the suggestion from @IndrajeetPatil.

### Bug fixed

1. Models and margins present in the correct order.

## Version 0.6.0

### New features

1. Adding changing the `dw_tidy` engine to `broomExtra::tidy_parapmeter`.
2. Adding the function to plot AME based on [`margins::margins`](https://CRAN.R-project.org/package=margins).

### Bug fixed

1. Allowing the data.frame output varying based on confidence intervals.
2. Setting the default value of `by_2sd` to FALSE.

## Version 0.5.0

#### New features

1. The `vline` argument is now available for `dwplot()`.  Passing a `geom_vline()` object to this argument, typically one with `xintercept = 0`, will plot this line _behind_ the plotted coefficients, which most will find aesthetically preferable.  The default for this argument is `NULL`, so if you prefer not to include such lines or just like them plotted last and foremost, there's no need to change your code.
2. `dwplot()` now again accepts the `whisker_arg` argument to change the appearance of the whiskers representing the confidence intervals that has been lost since v0.3.0.  This means you can, for example, specify different colors for the dots and the whiskers:

```r
# load the library
library(dotwhisker)
#> Loading required package: ggplot2

# linear model of interest
lm_object <- stats::lm(formula = wt ~ am * cyl, data = mtcars)

# creating the plot with dwplot
dwplot(x = lm_object,
       dot_args = list(color = "red"), # color for the dot
       whisker_args = list(color = "black"),   # color for the whisker
       vline = ggplot2::geom_vline(xintercept = 0,  # put vline _behind_ coefs
                                   colour = "grey60",
                                   linetype = 2,
                                   size = 1))
```

![](https://i.imgur.com/Hr3ZOzF.png)

Created on 2018-06-27 by the `reprex` package (v0.2.0).

## Version 0.4.1

#### Bug fixes

1. Fixed a bug in `add_brackets()` that caused brackets to overlap in large models or when many models were included in a single plot.

## Version 0.4.0

#### New features

1. A new plot style!  Specifying `style = "distribution"` in the arguments to `dwplot()` presents regression coefficients as normal distributions, underscored with a line representing the desired confidence interval.
2. `relabel_predictors()` now conveniently _reorders_ the predictors as well.
3. `add_brackets()` can now be added directly to the end of a chain of commands that generate a dotwhisker plot; the intermediate object necessary in past versions is no longer needed.  Just wrap the plotting commands in braces (`{` and `}`) before piping them to `add_brackets()`!

#### Syntax changes

1. The `alpha` argument to `dwplot()` should no longer be used to change the width of confidence intervals; use `conf.int` (to be passed to `broom::tidy` via `...`) instead.
2. When `dwplot()` is passed model objects rather than a tidy data frame, the regression coefficients are now rescaled by two standard deviations of their respective variables in the analyzed data (per `by_2sd()`)  by default.  This may be changed by setting `by_2sd = FALSE`.

#### Bug fixes

1. Fixed a bug in `add_brackets()` that de-centered the brackets
2. Fixed a bug that caused `dot_args` to be ignored after plots were passed to `relabel_predictors()`
3. Fixed a bug that prevented `small_multiple()` from directly reading confidence intervals from a model.
4. Fixed a bug in `by_2sd()` now adjusts, if present, any confidence intervals in tidy data frames passed to the function.

Thanks to Steven V. Miller and Ryan Burge for bug reports, and to Ben Edwards and Jay Jacobs for inspiring `style = "distribution"`!

## Version 0.3.0

1. Rewrote the plotting functions based on the `ggstance` functions. The new `dwplot` allows cooperating with more `ggplot` functions, such as `facet_*`.
2. Drew whiskers based on the CI estimates directly from the model output. See more details in `tidy.lm` and `confint`.
3. Clarified the description of `by_2sd`.

## Version 0.2.6

1. Fixed the bug in `relabel_predictors`.

## Version 0.2.5

1. Expanded capabilities of `relabel_predictors`. `relabel_predictors` now accepts plots as well as tidy dataframes as input; that is, it may now be used both before and after calls to `dwplot`.
2. Deprecated `relabel_y_axis`.  It is easy to mistakenly mislabel variables with `relabel_y_axis`, and it has a conflict with `add_brackets` in single-model plots.
3. Provided example of using multiple shapes for multiple models in vignette.
4. `dwplot` works for `polr` projects.

## Version 0.2.4

1. Improved the presentation of `small_multiple`.

## Version 0.2.3

1. Fixed the error of variable ordering with a single model.

## Version 0.2.2

1. Fixed the error in presenting multiple models.

## Version 0.2.1

1. Fixed the error due to the update of `dplyr::group_by`
2. Fixing the errors in vignette.
3. Adding the `show_intercept` argument.
4. Shorten the version number to three digits as `devtools` suggests.

## Version 0.2.0.5

1. Fixed the error due to the update of `gridExtra`.
2. Fixed the error due to the update of `ggplot2`.

## Version 0.2.0.4

1. Fixed presenting error in multilevel models (#44)

## Version 0.2.0.3

1. Fixed the link error in `kl2007_example.Rmd`.

## Version 0.2.0.2

1. Improving the vignette.
2. The function works for `ggplot2` 2.0.0.

## Version 0.2.0.1

1. Fixed the error in the vignette.

## Version 0.2.0.0

1. Allowing directly using model objects besides `tidy` data.frame.
2. Adding two new special plotting functions: `secret_weapon` and `small_multiple`.
3. Adding two graph adjusting functions: `relabel_predictor` and `relabel_y_axis`.

More details about the new functions are available in the vignette.
