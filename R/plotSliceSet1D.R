
#' Show the slice intersections in a plot
#'
#' @importFrom magrittr "%>%"
#'
#' @param o The 1D SliceSet to plot
plot.SliceSet1D = function(o) {
  # TODO: add an option to put the plots in columns
  d = length(o$problemSpec$dimNames)
  y.lims = range(o$slices$y)
  plots = purrr::map(1:d, function(dim) {
    pd = dplyr::filter(o$slices, d==dim)
    ggplot2::ggplot(pd, ggplot2::aes(x=x, y=y, group=fpid)) +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(limits=o$problemSpec$limits[[dim]]) +
      ggplot2::scale_y_continuous(limits=y.lims) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                     axis.title.y=ggplot2::element_blank())
  })
  plots
  dim.labels = o$problemSpec$dimNames
  if(missing(dim.labels)) {
    dim.labels = paste("x", 1:d, sep="")
  }

  labels = Map(grid::textGrob, dim.labels)
  n = length(plots)
  layout = cbind((n+1):(2*n), 1:n)
  gridExtra::grid.arrange(grobs=c(plots, labels), layout_matrix=layout, widths=c(0.1, 1))
}
