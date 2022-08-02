#' Calculating quantile breaks, e.g. for usage with pheatmap settings
#' 
#' @description Function from Kamil Slowikowski (\url{https://slowkow.com/notes/heatmap-tutorial/}).
#' 
#' @param xs matrix or vector of numbers
#' @param n number of breaks
#' 
#' @return named vector of breaks where the names indicate the quantiles
#' and the numbers indicate the actual values where the breaks are to
#' be drawn
#' 
#' @examples
#' hm_mat <- matrix(rnorm(200), 20, 10)
#' hm_mat[c(100,101)] <- max(hm_mat) + 1000
#' 
#' mat_breaks <- quantile_breaks(hm_mat, n = 10)
#' pheatmap(hm_mat,
#'          breaks = mat_breaks,
#'          color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(mat_breaks) - 1))
#'
quantile_breaks <- function(xs, n = 10) {
  breaks <- quantile(xs, probs = seq(0, 1, length.out = n))
  breaks[!duplicated(breaks)]
}
