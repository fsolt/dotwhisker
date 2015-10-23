## ----fig.width = 7, fig.height = 4, warning = FALSE, message = FALSE-----
#Package preload
library(dotwhisker)
library(broom)
library(dplyr)

# run a regression compatible with tidy
m1 <- lm(mpg ~ wt + cyl + disp + gear, data = mtcars)

# draw a dot-and-whisker plot
dwplot(m1)

## ----fig.width = 7, fig.height = 4, warning = FALSE, message = FALSE-----
dwplot(m1, alpha = .01)  # using 99% CI

## ----fig.width = 7, fig.height = 4, warning = FALSE, message = FALSE-----
m2 <- update(m1, . ~ . + hp) # add another predictor
m3 <- update(m2, . ~ . + am) # and another 

dwplot(list(m1, m2, m3))

## ----fig.width = 7, fig.height = 4, warning = FALSE, message = FALSE-----
dwplot(list(m1, m2, m3)) +
     relabel_y_axis(c("Intercept", "Weight", "Cylinders", "Displacement", 
                     "Gears", "Horsepower", "Manual")) +
     theme_bw() + xlab("Coefficient Estimate") + ylab("") +
     geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
     ggtitle("Predicting Gas Mileage") +
     theme(plot.title = element_text(face="bold"),
           legend.justification=c(1, 0), legend.position=c(1, 0),
           legend.background = element_rect(colour="grey80"),
           legend.title = element_blank()) 

## ----fig.width = 7, fig.height = 4, warning = FALSE, message = FALSE-----
# regression compatible with tidy
m1_df <- tidy(m1) # create data.frame of regression results
m1_df # a tidy data.frame available for dwplot
dwplot(m1_df) #same as dwplot(m1)

## ----fig.width = 7, fig.height = 4, warning = FALSE, message = FALSE-----
# Run model on subsets of data, save results as tidy df, drop intercept, make a model variable, and relabel predictors
by_trans <- mtcars %>% group_by(am) %>%                      # group data by trans
    do(tidy(lm(mpg ~ wt + cyl + disp + gear, data = .))) %>% # run model on each grp
    filter(term != "(Intercept)") %>%                        # drop intercepts
    rename(model=am) %>%                                     # make model variable
    relabel_predictors(c(wt = "Weight",                      # relabel predictors
                     cyl = "Cylinder",
                     disp = "Displacement",
                     gear = "Gear"))

by_trans

dwplot(by_trans, dodge_size = .05) +
    theme_bw() + xlab("Coefficient Estimate") + ylab("") +
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
    ggtitle("Predicting Gas Mileage by Transmission Type") +
    theme(plot.title = element_text(face="bold"),
          legend.justification=c(0, 0), legend.position=c(0, 0),
          legend.background = element_rect(colour="grey80"),
          legend.title.align = .5) +
    scale_colour_grey(start = .3, end = .7,
                      name = "Transmission",
                      breaks = c(0, 1),
                      labels = c("Automatic", "Manual"))

## ----fig.width = 7, message = FALSE, warning = FALSE---------------------
library(dplyr)

# the ordinal regression model is not supported by tidy
m4 <- ordinal::clm(factor(gear) ~ wt + cyl + disp, data = mtcars)
m4_df <- coef(summary(m4)) %>% 
  data.frame() %>% 
  add_rownames("term") %>%
  rename(estimate = Estimate, std.error = Std..Error)
m4_df
dwplot(m4_df)

## ----fig.width = 7, message = FALSE, warning = FALSE---------------------
# Customize the input data frame
m1_df_mod <- m1_df %>%                 # the original tidy data.frame
    by_2sd(mtcars) %>%                 # rescale the coefficients
    filter(term != "(Intercept)") %>%  # omit the intercept
    arrange(term)                      # alphabetize the variables

m1_df_mod  # rescaled, intercept omitted, and variables reordered alphabetically
dwplot(m1_df_mod)

## ----fig.width = 7, message = FALSE, warning = FALSE---------------------
# Create a data.frame of marginal effects
library(mfx)
m5 <- logitmfx(formula = am ~ wt + cyl + disp, data = mtcars) 
m5_margin <- data.frame(m5$mfxest) %>% 
  add_rownames("term") %>% 
  rename(estimate = dF.dx, std.error = Std..Err.)
m5_margin
dwplot(m5_margin)

## ----fig.width = 7, fig.height = 5, warning = FALSE, message = FALSE-----
# Define order for predictors that can be grouped
reordered_vars <- c("wt", "cyl", "disp", "hp", "gear", "am")

# Generate a tidy data frame
m123_df <- rbind(tidy(m1) %>% mutate(model = "Model 1"),      # tidy results &
                 tidy(m2) %>% mutate(model = "Model 2"),      # add a variable to
                 tidy(m3) %>% mutate(model = "Model 3")) %>%  # identify model.
    filter(term != "(Intercept)") %>%                         # drop intercepts
    by_2sd(mtcars) %>%                                        # rescale coefficients
    mutate(term = factor(term, levels = reordered_vars)) %>%  # make term a factor &
    group_by(model) %>% arrange(term) %>%                     # reorder
    relabel_predictors(c(wt = "Weight",                       # relabel predictors
                         cyl = "Cylinders", 
                         disp = "Displacement", 
                         hp = "Horsepower", 
                         gear = "Gears", 
                         am = "Manual"))

# Save finalized plot to an object 
p123 <- dwplot(m123_df, dodge_size = .08) +
     relabel_y_axis(c("Weight", "Cylinders", "Displacement", 
                               "Horsepower", "Gears", "Manual")) +
     theme_bw() + xlab("Coefficient Estimate") + ylab("") +
     geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
     ggtitle("Predicting Gas Mileage") +
     theme(plot.title = element_text(face="bold"),
           legend.justification=c(1, 1), legend.position=c(1, 1),
           legend.background = element_rect(colour="grey80"),
           legend.title = element_blank()) 

# Create list of brackets (label, topmost included predictor, bottommost included predictor)
three_brackets <- list(c("Overall", "Weight", "Weight"), 
                       c("Engine", "Cylinders", "Horsepower"),
                       c("Transmission", "Gears", "Manual"))

g123 <- p123 %>% add_brackets(three_brackets)

# pdf("plot.pdf")  # to save to file (not run)
# grid.draw(g456)
# dev.off()

grid.draw(g123)    # to display

## ----fig.width = 7, fig.height = 5, warning = FALSE, message = FALSE-----
data(diamonds)

# Estimate models for many subsets of data, put results in a tidy data.frame
by_clarity <- diamonds %>% group_by(clarity) %>%
 do(tidy(lm(price ~ carat + cut + color, data = .))) %>%
 ungroup %>% rename(model = clarity)

# Deploy the secret weapon
secret_weapon(by_clarity, var="carat", alpha=.01) + 
    xlab("Estimated Coefficient (Dollars)") + ylab("Diamond Clarity") +
    ggtitle("Estimated Coefficients for Diamond Size Across Clarity Grades") +
    theme(plot.title = element_text(face="bold"))

## ----fig.width = 2.5, fig.height = 7.5, warning = FALSE, message = FALSE----
# Generate a tidy data.frame of regression results from six models
m <- list()
ordered_vars <- c("wt", "cyl", "disp", "hp", "gear", "am")
m[[1]] <- lm(mpg ~ wt, data = mtcars) 
m123456_df <- m[[1]] %>% tidy %>% by_2sd(mtcars) %>%
  mutate(model = "Model 1")
for (i in 2:6) {
  m[[i]] <- update(m[[i-1]], paste(". ~ . +", ordered_vars[i]))
  m123456_df <- rbind(m123456_df, m[[i]] %>% tidy %>% by_2sd(mtcars) %>%
    mutate(model = paste("Model", i)))
}

# Relabel predictors (they will appear as facet labels)
m123456_df <- m123456_df %>% 
  relabel_predictors(c("(Intercept)" = "Intercept",
                     wt = "Weight",
                     cyl = "Cylinders",
                     disp = "Displacement",
                     hp = "Horsepower",
                     gear = "Gears",
                     am = "Manual"))
 
# Generate a 'small multiple' plot
small_multiple(m123456_df) +
  theme_bw() + ylab("Coefficient Estimate") +
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Predicting Gas Mileage") +
  theme(plot.title = element_text(face = "bold"), legend.position = "none",
    axis.text.x  = element_text(angle = 60, hjust = 1)) 

## ----fig.width = 3.5, fig.height = 8, warning = FALSE, message = FALSE----
# Generate a tidy data.frame of regression results from five models on
# the mtcars data subset by transmission type
ordered_vars <- c("wt", "cyl", "disp", "hp", "gear")
mod <- "mpg ~ wt"

by_trans <- mtcars %>% group_by(am) %>%  # group data by transmission
  do(tidy(lm(mod, data = .))) %>%        # run model on each group
  rename(submodel = am) %>%              # make submodel variable
  mutate(model = "Model 1")              # make model variable

for (i in 2:5) {
  mod <- paste(mod, "+", ordered_vars[i])
  by_trans <- rbind(by_trans, mtcars %>% group_by(am) %>%
                   do(tidy(lm(mod, data = .))) %>%
                   rename(submodel = am) %>%
                   mutate(model = paste("Model", i)))
}

# Relabel predictors (they will appear as facet labels)
by_trans <- by_trans %>% dplyr::select(-submodel, everything(), submodel) %>% 
  relabel_predictors(c("(Intercept)" = "Intercept",
                     wt = "Weight",
                     cyl = "Cylinders",
                     disp = "Displacement",
                     hp = "Horsepower",
                     gear = "Gears"))

by_trans

small_multiple(by_trans, dodge_size = .06) +
    theme_bw() + ylab("Coefficient Estimate") +
    geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
    theme(axis.text.x  = element_text(angle = 45, hjust = 1),
          legend.position=c(0, 0), legend.justification=c(0, 0),
          legend.title = element_text(size=8),
          legend.background = element_rect(color="gray90"),
          legend.margin = unit(-4, "pt"),
          legend.key.size = unit(10, "pt")) +
    scale_colour_hue(name = "Transmission",
                     breaks = c(0, 1),
                     labels = c("Automatic", "Manual")) +
    ggtitle("Predicting Gas Mileage\nby Transmission Type")

