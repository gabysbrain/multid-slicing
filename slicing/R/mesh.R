
#' Create a mesh from a set of points
#'
#' \code{convmesh} uses \code{\link[geometry]{convhulln}} to create a convex hull around a set of points
#'
#' @param points An \code{n} by \code{dim} set of points
#' @return An object of type \code{Mesh}:
#'           \code{points} The original point set
#'           \code{simplices} An \code{m} by \code{dim} matrix containing indices into \code{points} specifying the mesh
convmesh <- function(points) {
  r = list(points=points, simplices=geometry::convhulln(points))
  class(r) = "Mesh"
  r
}
