
#' Create a mesh from a set of points
#'
#' \code{convmesh} uses \code{\link[geometry]{convhulln}} to create a convex hull around a set of points
#'
#' @param points An \code{n} by \code{dim} set of points
#' @param spec A \code{ProblemSpec}. If this is not provided then it will be computed from the points
#' @param nice Whether or not to convert the problem specification to "nice" numbers
#' @return An object of type \code{Mesh}:
#'           \code{problemSpec} The \code{ProblemSpec} giving the limits of the parameters
#'           \code{points} The original point set
#'           \code{simplices} An \code{m} by \code{dim} matrix containing indices into \code{points} specifying the mesh
convmesh <- function(points, spec=NA, nice=FALSE) {
  if(is.na(spec)) {
    spec = problemSpecFromPoints(points)
    if(nice) {
      for(d in spec$dimNames) {
        spec$limits[[d]] = range(pretty(spec$limits[[d]], n=2))
      }
    }
  }
  #d = geometry::delaunayn(as.matrix(points))
  s = geometry::convhulln(as.matrix(points))
  #r = list(problemSpec=spec, points=points, simplices=geometry::surf.tri(as.matrix(points), d))
  r = list(problemSpec=spec, points=points, simplices=s)
  class(r) = "Mesh"
  r
}
