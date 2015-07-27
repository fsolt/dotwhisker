#' Create dataset list for small multiple plots
#'
#' \code{smul} creates a list of datasets for ploting the "small multiple" plots (Kastellec and Leoni 2007, 768).
#'
#' @param df A data.frame including the variables \code{term} (names of independent variables), \code{estimate} (corresponding coefficient estimates), \code{std.error} (corresponding standard errors), and optionally \code{model} (when multiple models are desired on a single plot) such as generated those by \code{\link[broom]{tidy}}.
#' @param by A character showing based on which variable the \code{df} is splited. For a typical "small multiple" plot, the spliting criterion is usually \code{df$term}. 
#' @param compare A character indicating the new \code{model} in each splited group of estimates.
#'
#' @return The function returns a list of tidy datasets.
#'
#'
#'@references
#' Kastellec, Jonathan P. and Leoni, Eduardo L. 2007. "Using Graphs Instead of Tables in Political Science." Perspectives on Politics, 5(4):755-771.
#'
#' @import broom
#'
#' @examples
#' data("diamonds")
#' library(broom)
#' 
#' 
#' diamonds$cut[diamonds$cut == "Fair"] <- "Good"
#' 
#'  by_cut <- diamonds %>% group_by(cut) %>%
#'   do(tidy(lm(price ~ depth + table, data = .))) %>% rename(model=cut)
#'   
#' by_cut$top <- 0
#' by_cut$top[by_cut$model %in% c("Premium", "Ideal")] <- 1
#' 
#' by_cut$model <- rep(c("low-level", "high-level"), each = 3, times = 2)
#' smul_list <- smul(by_cut, "term", "top")
#'
#'
#' @export


smul <- function(df, by = "term", compare = NULL){
  
  term <- model <- NULL # not functional, just for CRAN check
  
  eval(parse(text = paste0("split_list <- split(df, df$",by,")")))
  
  split_list <- lapply(split_list, rename, term_orig = term, term = model)
  
  if(!is.null(compare)) eval(parse(text = paste0("split_list <- lapply(split_list, rename, model =", compare, ")"))) 
  
  
  return(split_list)
}
