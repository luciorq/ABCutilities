# ABCutilities

## ABCutilities v0.3.3 (Development version)

* Fork from original package

## ABCutilities v0.3.2 (Release date: 2019-08-27)

* added function `load_RDSdata_from_Box()` to retrieve RDS objects stored in the Box
(but any direct link to an RDS object should work)

## ABCutilities v0.3.1 (Release date: 2019-07-19)

* ggplot2 helpers: added a function to modify the background color
of facet grid strips (color_grid_facet_bg)

## ABCutilities v0.3.0 (Release date: 2019-01-13)

* ggplot2 helpers: changed default color scheme from viridis to grey-red gradient,
which tends to make more sense for scRNA-seq dimensionality reduction plots

## ABCutilities v0.2.9 (Release date: 2018-11-28)

* added perl script for automated submission to GOrilla
  * R wrapper: submit_to_gorilla()
  * high level wrapper for both GOrilla and REVIGO: GOenrichment_GOrilla_REVIGO()

## ABCutilities v0.2.8 (Release date: 2018-10-15)

* added perl script for automated submission to GOrilla
  * R wrapper: submit_to_gorilla()
  * high level wrapper for both GOrilla and REVIGO: GOenrichment_GOrilla_REVIGO()

## ABCutilities v0.2.6 (Release date: 2018-09-07)

* changed order and content of paired_pal palette

## ABCutilities v0.2.6 (Release date: 2018-08-22)

* added `quantile_breaks()` function from Kamil Slowikowski to adjust
pheatmap color schemes if needed
* added Gorilla2REVIGO function for automated submission of GOrilla results to REVIGO

## ABCutilities v.0.2.4

* changed `fx.resolve_plot_shapes` so that the original ggplot2 shapes are only overruled if there are more than 6 shapes provided
