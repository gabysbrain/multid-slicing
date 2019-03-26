
#' Plots a \code{Mesh} using rgl
plot.Mesh = function(m) {
  if(!requireNamespace("rgl", quietly = TRUE)) {
    stop("package rgl required")
  }
  if(length(m$problemSpec$dimNames) != 3) {
    stop("only 3D meshes supported")
  }

  base = matrix(c(0,0,0,0,1,0,1,1,0,1,0,0), ncol=3, byrow=TRUE)
  pts = m$points
  verts = t(m$simplices)

  rgl::open3d()
  rgl::quads3d(xyz.coords(base), alpha=0.2)
  #geometry::tetramesh(m$simplices, as.matrix(m$points))
  rgl::rgl.triangles(pts[verts,1], pts[verts,2], pts[verts,3], col="purple")
  #planes3d(a=0, b=0, c=1, d=-0.75+1e-5, col="green")
  #rgl::planes3d(a=plane.pts[1], b=plane.pts[2], c=plane.pts[3], d=plane.pts[4]+1e-5, col="green")
  #rgl::points3d(focus.pt[1], focus.pt[2], focus.pt[3], col="blue", size=10)
  rgl::axes3d(col="black")
  rgl::title3d(xlab="d1", ylab="d2", zlab="d3")
}
