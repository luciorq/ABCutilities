#' Helper function for shiny scripts
#'
#' @description Sometimes, it is more convenient to allow the user to select
#' "none" instead of setting a variable to NULL. Within the scripts, however,
#' it is often easier to handle instances of NULL than of the string "none".
#' This function is supposed to help with that as it returns NULL if x is either
#' "none" or an empty string.
#'
#' @description On-the-fly NULLing of "none" or "" entries.
#'
#' @param x value to be replaced.
#'
#' @export
#'
n2n <- function(x) {
  if (is.null(x)) {
    x
  } else {
    if (length(x) <= 1) {
      if (x == "none" || x == "") {
        NULL
      } else {
        x
      }
    } else {
      x
    }
  }
}


#' Parse the faceting information
#'
#' @param row_facet name (or NULL) of the factor used for assigning the row
#'   facet
#' @param col_facet name (or NULL) of the factor used for assigning the column
#'   facet
#' @param wrap Boolean indicating whether facet_grid or facet_wrap should be
#'   used.
#' Default: FALSE will return entries fit for facet_grid.
#'
#' @return Generates a `list`` with a vector of labels that will be passed
#' to the plotting function ("metainfo") and the individual row and column
#' faceting factors ("fac_r","fac_c").
#'
#' @examples \dontrun{
#' fc_info <- choose_faceting(
#'   row_facet = input$facet_by_row,
#'   col_facet = input$facet_by_col
#' )
#' P <- scABC:::plot_reduced_dim.object(sce,
#'   X = pca$cells[, "PC1"], Y = pca$cells[, "PC2"],
#'   exprs_values = input$expr,
#'   add_cell_info = fc_info$metainfo
#' )
#' gg <- P + facet_grid(as.formula(paste(fc_info$fac_r, "~", fc_info$fac_c)))
#' }
#'
#' @export
#'
choose_faceting <- function(row_facet, col_facet, wrap = FALSE) {
  if (
    (row_facet == "none" || is.null(row_facet)) &&
      (col_facet == "none" || is.null(col_facet))
  ) {
    mi <- NULL
    return(list(metainfo = mi))
  } else {
    # decide on row facet
    if (row_facet == "none") {
      fac_r <- NULL
      if (wrap == TRUE) {
        fac_r_out <- NULL
      } else {
        fac_r_out <- "."
      }
    } else {
      fac_r <- row_facet
      fac_r_out <- row_facet
    } # decide on col facet
    if (col_facet == "none") {
      fac_c <- NULL
      if (wrap == TRUE) {
        fac_c_out <- NULL
      } else {
        fac_c_out <- "."
      }
    } else {
      fac_c <- col_facet
      fac_c_out <- col_facet
    }
    mi <- c(fac_r, fac_c)
    return(list(metainfo = mi, fac_r = fac_r_out, fac_c = fac_c_out))
  }
}




#' Select factors of interest based on some customized criteria
#'
#' @description Some of the plotting factors need to be checked for certain
#' features, such as the numbers of levels. For example, you do not want to use
#' a variable with more than 6 levels for assigning the point sizes of a certain
#' plot.
#'
#' @param in.df data.frame variant where the columns represent the different
#' variables/factors that need to be checked, e.g. \code{colData(sce)}.
#' @param max_nlevel integer specifying the maximum number of levels that are
#' allowed. Default: Inf
#' @param factor_class specify the class(es) of the factor that you either
#'   want to include or exclude, depending on the setting of `keep_class`.
#'   Default (NULL) will not check the class.
#' @param keep_class will only come into effect when \code{factor_class} is not
#' NULL; default setting (TRUE) will \strong{retain} all factors that correspond
#' to the class(es) defined by \code{factor_class}. Setting this to FALSE would
#' \strong{remove} those columns.
#'
#' @return The original data.frame without the columns that did not meet the
#' \code{max_nlevel} and/or \code{factor_class} filtering.
#'
#' @examples \dontrun{
#' names(select_factors(colData(sce), max_nlevel = 6))
#' }
#'
#' @export
#'
select_factors <- function(in.df,
                           max_nlevel = Inf,
                           factor_class = NULL,
                           keep_class = TRUE) {

  # check the levels
  tmp.df <- in.df[
    ,
    unlist(lapply(in.df, function(x) nlevels(factor(x)) <= max_nlevel))
  ]

  # check factor type if wanted
  if (!is.null(factor_class)) {
    keep_col <- unlist(lapply(tmp.df, function(x) class(x) %in% factor_class))
    if (!keep_class) {
      keep_col <- !keep_col
    }
    tmp.df <- tmp.df[keep_col]
  }
  if (dim(tmp.df)[2] == 0) {
    warning("Select_factors removed all columns from the input data.frame.")
  }

  return(tmp.df)
}
