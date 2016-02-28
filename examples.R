data(mtcars)
m1 <- lm(mpg ~ wt + cyl + disp, data = mtcars)
library(broom)

m1_df <- tidy(m1) # create data.frame of regression results

# Plot regression coefficients
dwplot(m1_df)

p0 <- dwplot(m1_df) +
    scale_y_discrete(breaks = 4:1, labels=c("Intercept", "Weight", "Cylinders", "Displacement")) +
    theme_bw() + xlab("Coefficient") + ylab("") +
    geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
    theme(legend.position="none")
p0

# Plot regression coefficients from multiple models
data(mtcars)
library(broom)
library(dplyr)
by_origin <- mtcars %>% group_by(am) %>%
    do(tidy(lm(mpg ~ wt + cyl + disp, data = .))) %>% rename(model=am)

p1 <- dwplot(by_origin, dodge_size = .05) +
    scale_y_discrete(breaks = 4:1, labels=c("Intercept", "Weight", "Cylinders", "Displacement")) +
    theme_bw() + xlab("Coefficient Estimate") + ylab("") +
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
    ggtitle("Predicting Gas Mileage, OLS Estimates") +
    theme(plot.title = element_text(face="bold"),
          legend.justification=c(1,0), legend.position=c(1,0),
          legend.background = element_rect(colour="grey80"),
          legend.title.align = .5) +
    scale_colour_grey(start = .4, end = .8,
                      name = "Transmission",
                      breaks = c(0, 1),
                      labels = c("Automatic", "Manual"))
p1

two_brackets <- list(c("Engine", "cyl", "disp"), c("Not Engine", "(Intercept)", "wt"))

g <- p1 %>% add_brackets(two_brackets)

grid.arrange(g)  # to display

# to save
g <- grid.arrange(g)
ggsave(file = "gridplot.pdf", g)

# The "Secret Weapon" (plot one coefficient for many models)
data(diamonds)

by_clarity <- diamonds %>% group_by(clarity) %>%
    do(tidy(lm(price ~ carat + cut + color, data = .))) %>% rename(model=clarity)

carat_results <- by_clarity %>% filter(term=="carat") %>% select(-term) %>%
    rename(term=model)

p2 <- dwplot(carat_results) +
    xlab("Estimated Coefficient (Dollars)") + ylab("Diamond Clarity") +
    ggtitle("Estimated Coefficients for Diamond Size Across Clarity Grades") +
    theme(plot.title = element_text(face="bold"),
          legend.position = "none")
p2

library(latex2exp)
cats <- MASS::cats

p3 <- lm(Hwt ~ Sex*Bwt, data = cats) %>% tidy %>% dwplot +
    scale_y_discrete(breaks = 4:1, labels=c("Intercept", "Male", "Weight", "Male X Weight")) +
    xlab("Estimated Coefficient") + ylab("") +
    ggtitle(latex2exp("Predicting Cats' $\\heartsuit$")) +
    theme(plot.title = element_text(face="bold"),
          legend.position = "none")
p3
