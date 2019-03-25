

#' Show the slice intersection in a plot
#'
#' @importFrom magrittr "%>%"
#'
#' @param o The SliceSet to plot
plot.SliceSet = function(o) {
  ps = o$problemSpec
  ppts = o$focusPoints
  curves = o$slices
  if(nrow(curves)==0) stop("No plane/simplex intersections found")
  curves$fpid = factor(curves$fpid) # keeps the fp color consistent
  d = ncol(ppts)
  plots = list()
  for(i in 1:(d-1)) { # d1 varies the slowest
    for(j in (i+1):d) {
      pd = curves %>% dplyr::filter(d1==i&d2==j) %>% unique()
      p = ggplot2::ggplot(pd, ggplot2::aes(group=fpid)) +
          ggplot2::geom_segment(ggplot2::aes(x=d1Min, xend=d1Max, y=d2Min, yend=d2Max, colour=fpid))
      p = p + ggplot2::scale_x_continuous(limits=ps$limits[[i]]) +
        ggplot2::scale_y_continuous(limits=ps$limits[[j]]) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                       axis.title.y=ggplot2::element_blank())
      plots = lappend(plots, p)
    }
  }

  # need axis names as well
  dim.labels = names(ppts)
  if(missing(dim.labels)) {
    dim.labels = paste("x", 1:d, sep="")
  }
  horiz.labels = Map(grid::textGrob, dim.labels[1:(d-1)])
  vert.labels = Map(grid::textGrob, dim.labels[2:d])
  layout = splom.layout(d)
  layout = rbind(max(layout, na.rm=TRUE)+1:(d-1), layout) # put the labels after
  layout = cbind(c(NA, max(layout, na.rm=TRUE)+1:(d-1)), layout)
  gridExtra::grid.arrange(grobs=c(plots, horiz.labels, vert.labels), layout_matrix=layout,
                          widths=c(0.1,rep(1,d-1)), heights=c(0.2, rep(1,d-1)))
}

# append to a list
lappend = function(lst, obj) {
  lst[[length(lst)+1]] <- obj
  return(lst)
}

# layout for grid.arrange
splom.layout = function(d) {
  layout = matrix(NA, nrow=d-1, ncol=d-1)
  iter = 1
  num = d-1
  for(c in 1:ncol(layout)) {
    nums = seq(from=iter, length.out=num)
    layout[c:(c+length(nums)-1),c] = nums
    iter = iter + length(nums)
    num = num - 1
  }
  layout
}
