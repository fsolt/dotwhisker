#' Multiple plot function
#' 
#' \code{multiplot} is a funciton for combining multiple ggplot objects into one page, offered by \href{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}{\emph{Cookbook for R}}.
#' 
#' @param ... One or several ggplot objects separated by comma which are intended to combine.
#' @param plotlist A list of ggplot objects.
#' @param cols A numeric value setting the number of columns in the layout
#' @param A layout matrix. This setting will overwrite the setting in the \code{cols}.
#' 
#' @return The function returns a unclassified plot file, which can be saved by regular plot devices, such as \code{pdf("<FileName>.pdf")...dev.off()}. 
#' 
#' @export
#'
#'
#'
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
