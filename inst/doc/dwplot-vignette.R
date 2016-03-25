## ----message = F, warning=FALSE------------------------------------------
#Package preload
library(dplyr)
library(broom)
library(dotwhisker)

data(mtcars)

# regression compatible with tidy
m1 <- lm(mpg ~ wt + cyl + disp + gear, data = mtcars)
m1_df <- tidy(m1) # create data.frame of regression results
m1_df # available for dwplot

## ------------------------------------------------------------------------
# the ordinal regression model is not supported by tidy
m2 <- ordinal::clm(factor(gear) ~ wt + cyl + disp, data = mtcars)
m2_df <- coef(summary(m2)) %>% 
  data.frame() %>% 
  add_rownames("term") %>%
  rename(estimate = Estimate, std.error = Std..Error)
m2_df

## ------------------------------------------------------------------------
# Customize the input data frame
m1_df  # the original tidy data.frame

m1_df_sd <- tidy(m1) %>% by_2sd(mtcars)   # rescale the coefficients
m1_df_sel <- filter(m1_df_sd, term != "(Intercept)") # omit intercept
m1_df_sel2 <- arrange(m1_df_sel, term) # reorder the variables


m1_df_sd    # rescaled coefficients
m1_df_sel   # rescaled and intercept omitted
m1_df_sel2  # rescaled, intercept omitted, and variables reordered alphabetically

## ----message= FALSE------------------------------------------------------
# Create a data.frame of marginal effects
library(mfx)
m3 <- logitmfx(formula = am ~ wt + cyl + disp, data = mtcars) 
m3_margin <- data.frame(m3$mfxest) %>% 
  add_rownames("term") %>% 
  rename(estimate = dF.dx, std.error = Std..Err.)
m3_margin

## ----fig.width= 7, warning= FALSE, message= FALSE------------------------
dwplot(m1_df)
dwplot(m1_df, alpha = .01)  # using 99% CI

## ----fig.width= 7, warning= FALSE, message= FALSE------------------------
dwplot(m1_df_sel2)

## ----fig.width= 7, warning= FALSE, message= FALSE------------------------
dwplot(m1_df_sel2) + 
    scale_y_discrete(breaks = 4:1, 
                      labels=c("Cylinders", "Displacement", "Gears", "Weight")) +
    theme_bw() + xlab("Standardized Coefficient") + ylab("") +
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
    ggtitle("Predicting Gas Mileage") +
    theme(plot.title = element_text(face="bold"), legend.position="none") 

## ----fig.width= 7, fig.height=5, warning=FALSE, message=FALSE------------
# Run model on subsets of data, save results as tidy df, drop intercept, and make model variable
by_trans <- mtcars %>% group_by(am) %>%
    do(tidy(lm(mpg ~ wt + cyl + disp + gear, data = .))) %>%
    filter(term != "(Intercept)") %>% rename(model=am)

by_trans

dwplot(by_trans, dodge_size = .05) +
    scale_y_discrete(breaks = 4:1, labels=c("Weight", "Cylinders", "Displacement", "Gears")) +
    theme_bw() + xlab("Coefficient Estimate") + ylab("") +
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
    ggtitle("Predicting Gas Mileage by Transmission Type") +
    theme(plot.title = element_text(face="bold"),
          legend.justification=c(0, 0), legend.position=c(0, 0),
          legend.background = element_rect(colour="grey80"),
          legend.title.align = .5) +
    scale_colour_grey(start = .4, end = .8,
                      name = "Transmission",
                      breaks = c(0, 1),
                      labels = c("Automatic", "Manual"))


## ----fig.width= 7, fig.height=7, warning=FALSE, message=FALSE------------
# Estimate three models
m4 <- lm(mpg ~ wt + cyl + disp + gear, data = mtcars) # same as m1
m5 <- update(m4, . ~ . + hp) # add another predictor
m6 <- update(m5, . ~ . + am) # and another 

# Tidy estimates, rescale, and omit intercepts
prep <- . %>% tidy() %>% by_2sd(mtcars) %>% filter(term != "(Intercept)")

m4_df <- prep(m4)
m5_df <- prep(m5)
m6_df <- prep(m6)

# Ensure all data.frames include rows for all of the predictors, in the same order
# Include NAs for any quantities not estimated in a particular model
m4_df <- rbind(m4_df, c("hp", rep(NA, times = ncol(m4_df) - 1)),
               c("am", rep(NA, times = ncol(m4_df) - 1)))
m5_df <- rbind(m5_df, c("am", rep(NA, times = ncol(m5_df) - 1)))

# Add model variable to all data frames
m4_df <- mutate(m4_df, model = "Model 4")
m5_df <- mutate(m5_df, model = "Model 5")
m6_df <- mutate(m6_df, model = "Model 6")

m456_df <- rbind(m4_df, m5_df, m6_df)

dwplot(m456_df, dodge_size = .08) +
     scale_y_discrete(breaks = 6:1, 
                      labels=c("Weight", "Cylinders", "Displacement", 
                               "Gears", "Horsepower", "Manual")) +
     theme_bw() + xlab("Coefficient Estimate") + ylab("") +
     geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
     ggtitle("Predicting Gas Mileage") +
     theme(plot.title = element_text(face="bold"),
           legend.justification=c(1, 1), legend.position=c(1, 1),
           legend.background = element_rect(colour="grey80"),
           legend.title = element_blank()) 

## ----fig.width= 7, fig.height=7, warning=FALSE, message=FALSE------------
# Reorder predictors into groups
ordered_vars <- c("wt", "cyl", "disp", "hp", "gear", "am")
m456_df <- m456_df %>% 
    mutate(term =  factor(term, levels = ordered_vars)) %>%
    group_by(model) %>% arrange(term) 

# Save finalized plot to an object (note reordered labels to match reordered predictors)
p456 <- dwplot(m456_df, dodge_size = .08) +
     scale_y_discrete(breaks = 6:1, 
                      labels=c("Weight", "Cylinders", "Displacement", 
                               "Horsepower", "Gears", "Manual")) +
     theme_bw() + xlab("Coefficient Estimate") + ylab("") +
     geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
     ggtitle("Predicting Gas Mileage") +
     theme(plot.title = element_text(face="bold"),
           legend.justification=c(1, 1), legend.position=c(1, 1),
           legend.background = element_rect(colour="grey80"),
           legend.title = element_blank()) 

# Create list of brackets (label, topmost included predictor, bottommost included predictor)
three_brackets <- list(c("Overall", "wt", "wt"), c("Engine", "cyl", "hp"),
                       c("Transmission", "gear", "am"))

g456 <- p456 %>% add_brackets(three_brackets)

grid.arrange(g456)  # to display

# to save to file (not run)
# g <- grid.arrange(g456)
# ggsave(file = "gridplot.pdf", g)

## ----fig.width= 7, fig.height=7, warning=FALSE, message=FALSE------------
data(diamonds)

# Estimate models for many subsets of data
by_clarity <- diamonds %>% group_by(clarity) %>%
    do(tidy(lm(price ~ carat + cut + color, data = .))) %>% ungroup %>% rename(model=clarity)

# Extract the results for one variable
carat_results <- by_clarity %>% filter(term=="carat") %>% dplyr::select(-term) %>%
    rename(term = model)

# Deploy the secret weapon
dwplot(carat_results) +
    xlab("Estimated Coefficient (Dollars)") + ylab("Diamond Clarity") +
    ggtitle("Estimated Coefficients for Diamond Size Across Clarity Grades") +
    theme(plot.title = element_text(face="bold"))

## ----fig.width= 2.5, fig.height=6.5, warning=FALSE, message=FALSE--------
# Estimate six models, putting the results in a list, and
# transfer the list to a tidy data frame with NA rows for excluded variables

all_vars <- c("wt", "cyl", "disp", "gear", "hp", "am") 

add_NAs <- function(m, all_vars) {
    not_in <- setdiff(all_vars, m$term)
    for (i in seq(not_in))
        m <- rbind(m, c(not_in[i], rep(NA, times = ncol(m) - 1)))
    m
}

m <- list()
m[[1]] <- lm(mpg ~ wt, data = mtcars)
m123456_df <- m[[1]] %>% prep %>% add_NAs(all_vars) %>% 
    mutate(model = "Model 1")

for (i in 2:6) {
    m[[i]] <- update(m[[i-1]], paste(". ~ . +", ordered_vars[i]))
    m123456_df <- rbind(m123456_df, m[[i]] %>% prep %>% add_NAs(all_vars) %>%
                            mutate(model = paste("Model", i)))
}

# Format the tidy data frame for a small multiple plot
m123456_df <- m123456_df %>% 
    mutate(term = factor(term, levels = ordered_vars),
           model = factor(model, levels = paste("Model", 1:6))) %>%
    rename(predictor = term, term = model) %>% 
    mutate(model = 1) %>% arrange(predictor, desc(term)) 

levels(m123456_df$predictor) <- c("Weight", "Cylinders", "Displacement",
                                  "Gears", "Horsepower", "Manual") # For facet labels

# Plot using small multiples
dwplot(m123456_df) + facet_grid(predictor~.) + coord_flip() +
    theme_bw() + xlab("Coefficient Estimate") +
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
    ggtitle("Predicting Gas Mileage") +
    theme(plot.title = element_text(face = "bold"), legend.position = "none",
          axis.text.x  = element_text(angle = 60, hjust = 1)) 


