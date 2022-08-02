#' Add user-specified colors to facet grid strips
#'
#' @description This function is useful to assign specific colors to the background of the facet_grid labels.
#'
#' @details See \url{https://stackoverflow.com/questions/53455092/r-ggplot2-change-colour-of-font-and-background-in-facet-strip} for details.
#' This routine will only work with facet_grid (not facet_wrap) and
#' \code{in_plot} should *not* have the background color stripped from it via
#' the \code{theme} setting
#'
#' @param in_plot ggplot2 object generated with facet_grid (see examples).
#' @param facet_cols vector of color names, e.g. c("red","green","blue"). Its length should correspond to the factor that was used to assign the facet_grid with.
#'
#' @examples \dontrun{
#' ## generate base plot
#' Pp <- ggplot(tmp2, aes(x = HPCA.main, y = N)) +
#'   geom_bar(stat = "identity", aes(fill = condition)) +
#'   scale_fill_manual(values = condition_cols) +
#'   facet_grid(cluster ~ .)
#'
#' color_grid_facet_bg(Pp, facet_cols = cluster_cols)
#' }
#'
#' @return This function will print the newly colored plot to the
#' graphics device.
#'
#' @import grid
#' @import ggplot2
#'
#' @export
color_grid_facet_bg <- function(in_plot, facet_cols) {
  # based on: https://stackoverflow.com/questions/53455092/r-ggplot2-change-colour-of-font-and-background-in-facet-strip
  require(grid)
  ## GRAB THE STRIP DEFINITIONS
  Pg <- ggplot_gtable(ggplot_build(in_plot))
  strips <- which(grepl("strip-", Pg$layout$name))
  fills <- facet_cols
  k <- 1

  ## RE-ASSIGN THE BACKGROUND COLOR
  ## this only works with facet_grid!
  for (i in strips) {
    j <- which(grepl("rect", Pg$grobs[[i]]$grobs[[1]]$childrenOrder))
    Pg$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k + 1
  }
  grid.draw(Pg)
}


#' Retrieve the legend of a plot
#'
#' @description This function extracts just the legend from a \code{ggplot}.
#'
#' @param plot A \code{ggplot} that was created with a legend
#' @return A \code{gtable} object holding just the legend
#' @details From \url{https://stackoverflow.com/questions/11883844/inserting-a-table-under-the-legend-in-a-ggplot2-histogram}
#'
#' @examples
#' p1 <- ggplot(mtcars, aes(mpg, disp)) +
#'   geom_line()
#' plot.mpg <- ggplot(mpg, aes(x = cty, y = hwy, colour = factor(cyl))) +
#'   geom_point(size = 2.5)
#' legend <- get_legend(plot.mpg)
#' # plot the plot
#' plot.mpg <- plot.mpg + theme(legend.position = "none")
#' # plot the legend
#' grid.draw(legend)
get_legend <- function(plot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}



#' Parse column names
#'
#' @details There are numerous ways to access the values
#' stored in the columns of a \code{data.frame}. The one
#' using a dollar sign (e.g. \code{df$this_column}) is
#' vulnerable to special characters, such as "-", spaces etc.
#' Since the default method of \code{ggplot2} is to use the
#' dollar sign approach, we need to ensure that special
#' characters, which are often part of gene names, are taken
#' care of. This function simply wraps those names into
#' single quotes.
#'
#' @param cn column name
#' @return column name enclosed in single quotes
#'
#' @export
#'
fx.parse_column_names <- function(cn) {
  if (is.null(cn)) {
    cnp <- cn
  } else {
    cnp <- paste0("`", cn, "`")
  }

  return(cnp)
}

#' Supply more than 6 somewhat meaningful shapes
#'
#' @details The ggplot2 default setting will complain about having more than 6
#' different factors assigned to shape. This function assigns 12 shapes (if
#' there's more than 6 factors in \code{shape_by}), trying to find a good order
#' that will still allow for differentiations.
#'
#' @param plot_in ggplot2 object where shape is one of the aesthetics
#' @param shape_by the factors to which shape has been mapped
#'
#' @return ggplot2 object with the shape scale set manually
#'
#' @export
#'
fx.resolve_plot_shapes <- function(plot_in, shape_by) {
  n_shapes <- length(unique(shape_by))
  plot_out <- plot_in

  if (n_shapes > 6) {
    # define our own list of shapes
    our_shps <- c(16, 0, 4, 17, 2, 15, 1, 9, 8, 12, 18, 3)
    fill_na <- n_shapes - length(our_shps)
    if (fill_na > 0) {
      our_shps <- c(our_shps, rep(NA, fill_na))
    }
    plot_out <- plot_out + scale_shape_manual(values = our_shps)
  }

  return(plot_out)
}

#' Get nice plotting color schemes for very general color variables
#'
#' @description Wrapper around \code{fx.get_palette_ABC} that checks the numbers
#' and types of coloring variables that are being used and tries to return a
#' sensible color scheme. The colors are pre-defined in \code{fx.get_palette_ABC}.
#'
#' @details This function is based on a very similar function in the scater package.
#'
#' @param plot_out ggplot2 object
#' @param colour_by vector of values that determine the coloring of \code{plot_out}
#' @param colour_by_name string indicating the title/name for \code{colour_by},
#' e.g. the name of a gene
#' @param fill Boolean, default: \code{FALSE}
#'
#' @return \code{ggplot2} object with adjusted coloring scheme
#'
#' @seealso \code{fx.get_palette_ABC}
#'
#' @export
#'
fx.resolve_plot_colors <- function(plot_out, colour_by, colour_by_name,
                                   fill = FALSE) {
  ## if the colour_by object is NULL, return the plot_out object unchanged
  if (is.null(colour_by)) {
    return(plot_out)
  }
  ## Otherwise, set a sensible colour scheme and return the plot_out object
  leg_title <- colour_by_name

  if (fill) { ## routine for fill
    if (is.numeric(colour_by)) {
      # plot_out <- plot_out + viridis::scale_fill_viridis(name = leg_title)
      plot_out <- plot_out + scale_color_gradient(
        low = "grey90",
        high = "firebrick3",
        name = leg_title
      )
    } else {
      nlevs_colour_by <- nlevels(as.factor(colour_by))
      if (nlevs_colour_by <= 14) {
        plot_out <- plot_out + scale_fill_manual(
          values = fx.get_palette_ABC("paired_pal"),
          name = leg_title
        )
      } else {
        if (nlevs_colour_by > 14 && nlevs_colour_by <= 20) {
          plot_out <- plot_out + scale_fill_manual(
            values = fx.get_palette_ABC("tableau20"),
            name = leg_title
          )
        } else {
          plot_out <- plot_out +
            viridis::scale_fill_viridis(
              name = leg_title, discrete = TRUE
            )
        }
      }
    }
  } else { ## routine for color
    if (is.numeric(colour_by)) {
      # plot_out <- plot_out + viridis::scale_color_viridis(name = leg_title)
      plot_out <- plot_out + scale_color_gradient(
        low = "grey90",
        high = "firebrick3",
        name = leg_title
      )
    } else {
      nlevs_colour_by <- nlevels(as.factor(colour_by))
      if (nlevs_colour_by <= 14) {
        plot_out <- plot_out + scale_colour_manual(
          values = fx.get_palette_ABC("paired_pal"),
          name = leg_title
        )
      } else {
        if (nlevs_colour_by > 14 && nlevs_colour_by <= 20) {
          plot_out <- plot_out + scale_colour_manual(
            values = fx.get_palette_ABC("tableau20"),
            name = leg_title
          )
        } else {
          plot_out <- plot_out +
            viridis::scale_color_viridis(
              name = leg_title, discrete = TRUE
            )
        }
      }
    }
  }
  plot_out
}


#' Color palettes
#'
#' @description This function simply supplies pre-defined color palettes.
#'
#' @details Based on \code{scater}'s defaults, but with significant changes to the
#' standard colors that were bing used
#'
#' @return vector of color names
#'
#' @seealso \code{fx.resolve_plot_colors}
fx.get_palette_ABC <- function(palette_name) {
  switch(palette_name,
    # tableau20 = c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78", "#2CA02C",
    #               "#98DF8A", "#D62728", "#FF9896", "#9467BD", "#C5B0D5",
    #               "#8C564B", "#C49C94", "#E377C2", "#F7B6D2", "#7F7F7F",
    #               "#C7C7C7", "#BCBD22", "#DBDB8D", "#17BECF", "#9EDAE5"),
    # tableau10medium = c("#729ECE", "#FF9E4A", "#67BF5C", "#ED665D",
    #                     "#AD8BC9", "#A8786E", "#ED97CA", "#A2A2A2",
    #                     "#CDCC5D", "#6DCCDA"),
    tableau10medium = c(
      "#34B20D", "#FFAE18", "#be2f00", "#73FFC3", "#0D14B2",
      "#8EB20E", "#FF81DE", "#FFFF00", "#0CCC9C", "#656BB2"
    ),
    paired_pal = c(
      "#B1E2F9", "limegreen", "grey30",
      "#FFC914", # light orange # FDBF6F
      "#0066E2", # darkish blue #1F78B4,
      "#FC71E9", # now: pink; light red #FB9A99,
      "#00E5CA", # darkish green #33A02C
      "#E31A1C", # dark red #E31A1C
      "grey83",
      "#FF7F00", # dark orange #FF7F00
      "#378C07", # lavender  #CAB2D6
      "#6A3D9A", # dark purple #6A3D9A
      "#FFFF99", # light yellow #FFFF99
      "#B15928" # brown#B15928
    ),
    # originally from RColorBrewer::brewer.pal(12,  "Paired"),
    divergent = c("#A6CEE3", "limegreen", "grey30", ),
    tableau20 = c(
      "#34B20D", "#FFAE18", "#be2f00", "#73FFC3", "#0D14B2",
      "#8EB20E", "#FF81DE", "#FFFF00", "#0CCC9C", "#656BB2", # tableau10medium
      c(
        "#FF10FC", "#3E68FF", "#8B9440", "#7F85C3", "#FF85C3",
        "#F52306", "#FFD2BD", "#25FFED", "black", "#FFEC83"
      )
    ),
    colorblind10 = c(
      "#006BA4", "#FF800E", "#ABABAB", "#595959",
      "#5F9ED1", "#C85200", "#898989", "#A2C8EC",
      "#FFBC79", "#CFCFCF"
    ),
    trafficlight = c(
      "#B10318", "#DBA13A", "#309343", "#D82526",
      "#FFC156", "#69B764", "#F26C64", "#FFDD71",
      "#9FCD99"
    ),
    purplegray12 = c(
      "#7B66D2", "#A699E8", "#DC5FBD", "#FFC0DA",
      "#5F5A41", "#B4B19B", "#995688", "#D898BA",
      "#AB6AD5", "#D098EE", "#8B7C6E", "#DBD4C5"
    ),
    bluered12 = c(
      "#2C69B0", "#B5C8E2", "#F02720", "#FFB6B0", "#AC613C",
      "#E9C39B", "#6BA3D6", "#B5DFFD", "#AC8763", "#DDC9B4",
      "#BD0A36", "#F4737A"
    ),
    greenorange12 = c(
      "#32A251", "#ACD98D", "#FF7F0F", "#FFB977",
      "#3CB7CC", "#98D9E4", "#B85A0D", "#FFD94A",
      "#39737C", "#86B4A9", "#82853B", "#CCC94D"
    ),
    cyclic = c(
      "#1F83B4", "#1696AC", "#18A188", "#29A03C", "#54A338",
      "#82A93F", "#ADB828", "#D8BD35", "#FFBD4C", "#FFB022",
      "#FF9C0E", "#FF810E", "#E75727", "#D23E4E", "#C94D8C",
      "#C04AA7", "#B446B3", "#9658B1", "#8061B4", "#6F63BB"
    )
  )
}
