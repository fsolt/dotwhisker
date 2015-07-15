#' Standardize results by multiplying by 2 standard deviations
#' Per:
#' Gelman, Andrew. 2008. Scaling regression inputs by dividing by two standard deviations.
#'      Statistics in Medicine, 27:2865–2873.

by_2sd <- function(tr, df) {
    # tr: tidy results from model
    # df: data used in model

    sdX2 <- tr$term %>% as.list %>%
        lapply(function(x) {
            dich <- unique(df[[x]]) %>% extract(!is.na(.)) %>% sort %>% identical(c(0,1))
            ifelse(!dich, 2*sd(df[[x]], na.rm=T), 1)
        }) %>% unlist
    tr$estimate %<>% multiply_by(sdX2)
    tr$std.error %<>% multiply_by(sdX2)
    tr
}
