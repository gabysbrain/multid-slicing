
#' plot the simplex and the intersection plane
#'
#' useful for debugging random errors but only works with simplices in 3D space
#'
#' @param tri.pts the 3 points of the triangle
#' @param focus.pt the focus point at which to take the slice
#' @param d1 \code{d1} and \code{d2} define the axes to show the slice
plot.intersect = function(tri.pts, focus.pt, d1, d2) {
  if(!requireNamespace("rgl", quietly = TRUE)) {
    stop("package rgl required")
  }
  normal = array(1, 3)
  normal[c(d1,d2)] = 0
  plane.pts = c(normal, normal %*% -focus.pt)
  simplex = rbind(as.matrix(tri.pts), t(focus.pt))
  base = matrix(c(0,0,0,0,1,0,1,1,0,1,0,0), ncol=3, byrow=TRUE)
  #base = base * 10
  #base = base - 5

  rgl::open3d()
  rgl::quads3d(xyz.coords(base), alpha=0.2)
  #geometry::tetramesh(matrix(1:4, nrow=1), simplex, clear=FALSE)
  rgl::triangles3d(tri.pts[,1],tri.pts[,2],tri.pts[,3],col='purple')
  #planes3d(a=0, b=0, c=1, d=-0.75+1e-5, col="green")
  rgl::planes3d(a=plane.pts[1], b=plane.pts[2], c=plane.pts[3], d=plane.pts[4]+1e-5, col="green")
  rgl::points3d(focus.pt[1], focus.pt[2], focus.pt[3], col="blue", size=10)
  rgl::axes3d(col="black")
  rgl::title3d(xlab="d1", ylab="d2", zlab="d3")
  # bbox3d(col="grey", alpha=0.7)
}
