#' Plot multiple ggplot2 objects on one page
#'
#' @param plots list of ggplot2 objects
#' @param cols number of columns that the resulting plot should have
#' @param layout matrix with ncol and nrow, if left to NULL it will be deter-
#' mined automatically
#' @param title supply a title that will be printed across all columns (default:NULL)
#'
#' @examples
#' \dontrun{
#' label_list <- list(sample = "sample", replicate = "technical_replicate")
#' ## make a list of ggplot2 objects
#' pl <- lapply(seq_along(label_list),  function(i){
#'        P <-  draw_PCA_results(sceset.filt,
#'                       tsne_mg$all$V2, tsne_mg$all$V3,
#'                       exprs_values = "log10magic",
#'                       colour_by = names(label_list[i]))
#'        P <- P + ggtitle(label_list[[i]]) + theme_bw(base_size = 16)
#'      })
#'  ## plot them all on the same page
#'  MultiPlotList(pl, cols = 2, title = "tsne log10(magic)")
#' }
#' @import grid
#' @import ggplot2
#' 
#' @export
#' 
MultiPlotList <- function(plots, cols=1, layout=NULL, title = NULL) {
  
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
    grid::grid.newpage()
    if(is.null(title)){
      grid::pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    }else{
      grid::pushViewport(viewport(layout = grid.layout(nrow(layout)+1, ncol(layout),
                                                       heights=c(0.2, rep.int(1, nrow(layout))))))
    }
    
    # Make each plot, in the correct location
    if(!is.null(title)){
      grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:ncol(layout)))
    }
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      if(is.null(title)){
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }else{
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row+1,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
}
