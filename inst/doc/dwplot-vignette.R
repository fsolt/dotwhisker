## ----message = F, warning=FALSE------------------------------------------
#Package preload
library(dplyr)
library(broom)
library(ggplot2)
library(dotwhisker)

data(mtcars)

# regression compatble with tidy
m1 <- lm(mpg ~ wt + cyl + disp + vs, data = mtcars)
m1_df <- tidy(m1) # create data.frame of regression results
m1_df # available for dwplot

## ------------------------------------------------------------------------
# regression uncompatble with tidy
m2 <- ordinal::clm(factor(gear) ~ wt + cyl + disp, data = mtcars)
m2_df <- coef(summary(m2)) %>% 
  data.frame() %>% 
  add_rownames("term") %>%
  rename(estimate = Estimate, std.error = Std..Error)
m2_df

## ------------------------------------------------------------------------
# Customize the input data frame
m1_df  # the original tidy data.frame

m1_df_sd <- tidy(m1) %>% by_2sd(mtcars)   # Standardize the coefficients
m1_df_sel <- filter(m1_df_sd, term != "(Intercept)" & term != "c.vs") # Select variables
m1_df_sel2 <- arrange(m1_df_sel, term) # reorder the variables


m1_df_sd
m1_df_sel
m1_df_sel2

## ----message= FALSE------------------------------------------------------
# Create a data.frame of marginal effects
library(mfx)
m3 <- logitmfx(formula= vs ~ mpg + wt + disp, data = mtcars) 
m3_margin <- data.frame(m3$mfxest) %>% 
  add_rownames("term") %>% 
  rename(estimate = dF.dx, std.error = Std..Err.)
m3_margin

## ----fig.width= 7, warning= FALSE, message= FALSE------------------------
dwplot(m1_df)
dwplot(m1_df, interval = .01)  # using 99% CI

## ----fig.width= 7, warning= FALSE, message= FALSE------------------------
dwplot(m1_df_sel2) +
     scale_y_discrete(breaks = 4:1, 
                      labels=c("V/S", "Cylinders", "Displacement", "Weight")) +
     theme_bw() + xlab("Standardized Coefficent") + ylab("") +
     geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
     ggtitle("Predicting Gas Mileage") +
     theme(plot.title = element_text(face="bold"),
           legend.justification=c(1,0), legend.position=c(1,0),
           legend.background = element_rect(colour="grey80"),
           legend.title.align = .5) 

## ----fig.width= 7, warning=FALSE, message= FALSE-------------------------
# Comparing the estimates of the unstandardized and standardized models
m1_df <- mutate(m1_df, model = "unstandardized")
m1_df_sd <- mutate(m1_df_sd, model = "standardized") 
m1_df_sd$term <- m1_df$term
  
m1_comb <- rbind(m1_df, m1_df_sd)

dwplot(m1_comb, dodge_size = .15) +
     scale_y_discrete(breaks = 5:1, 
                      labels=c("Intercept", "Weight", "Cylinders", "Displacement", "V/S")) +
     theme_bw() + xlab("Coefficient Estimate") + ylab("") +
     geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
     ggtitle("Predicting Gas Mileage, OLS Estimates") +
     scale_colour_discrete(name = "Model",
                       labels = c("Unstandardized", "Standardized")) +
     theme(plot.title = element_text(face="bold"),
           legend.justification=c(1, 0), legend.position=c(1,0),
           legend.background = element_rect(colour="grey80"),
           legend.title.align = .5) 


## ----fig.width= 7, warning=FALSE, message=FALSE--------------------------
# Comparing the estimates of the unstandardized and standardized models
m1_df_sd <- tidy(m1) %>% by_2sd(mtcars)  
m1_df_sd
m1_df_sel

diff <- setdiff(m1_df_sd$term, m1_df_sel$term) # extract the difference of variables in two lists.

m1_df_sel <- rbind(m1_df_sel, 
                   c(diff[1], rep(NA, times = ncol(m1_df_sel) - 1)),
                   c(diff[2], rep(NA, times = ncol(m1_df_sel) - 1)))
  
m1_df_sel <- m1_df_sel[match(m1_df_sd$term, m1_df_sel$term),] #order matters

m1_df_sd <- mutate(m1_df_sd, model = "unconstrained") 
m1_df_sel <- mutate(m1_df_sel, model = "constrained")


m1_comb <- rbind(m1_df_sd, m1_df_sel)
m1_comb

dwplot(m1_comb, dodge_size = .15) +
     scale_y_discrete(breaks = 5:1, 
                      labels=c("Intercept", "Weight", "Cylinders", "Displacement", "V/S")) +
     theme_bw() + xlab("Coefficient Estimate") + ylab("") +
     geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
     ggtitle("Predicting Gas Mileage, OLS Estimates") +
     scale_colour_discrete(name = "Model",
                       labels = c("Unstandardized", "Standardized")) +
     theme(plot.title = element_text(face="bold"),
           legend.justification=c(1, 0), legend.position=c(1,0),
           legend.background = element_rect(colour="grey80"),
           legend.title.align = .5) 


## ----fig.width= 7, warning=FALSE, message=FALSE--------------------------
by_trans <- mtcars %>% group_by(gear) %>%
  do(tidy(lm(mpg ~ wt + cyl, data = .))) %>% rename(model=gear)

dwplot(by_trans, dodge_size = .15) +
     scale_y_discrete(breaks = 3:1, 
                      labels=c("Intercept", "Weight", "Cylinders")) +
     theme_bw() + xlab("Coefficient Estimate") + ylab("") +
     geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
     ggtitle("Predicting Gas Mileage, OLS Estimates") +
     scale_colour_discrete(name = "Gear Number") +
     theme(plot.title = element_text(face="bold"),
           legend.justification=c(1, 0), legend.position=c(1,0),
           legend.background = element_rect(colour="grey80"),
           legend.title.align = .5) 

