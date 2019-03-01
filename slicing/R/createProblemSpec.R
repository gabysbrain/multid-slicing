
#' A \code{ProblemSpec} specifies the names of the dimensions and the limits of those dimensions.
#'
#' @param ... A list of keyword arguments, e.g. \code{x1=c(-1,1)} means parameter x1 goes from -1 to +1
#' @return An object of type \code{ProblemSpec}:
#'           \code{dimNames}: An array of the names of the dimensions
#'           \code{limits}: A list of \code{dim=c(min,max)} with the limits
createProblemSpec = function(...) {
  args = list(...)
  ps = list(dimNames = names(args), limits=args)
  class(ps) = "ProblemSpec"
  ps
}

#' Create a \code{ProblemSpec} from the limits of named columns
#'
#' @param points A matrix-like that contains the points. If the columns aren't named then the default R data frame names will be used.
#' @return A \code{ProblemSpec} Based on the limits of the points
problemSpecFromPoints = function(points) {
  if(is.null(names(points))) {
    points = data.frame(points)
  }
  rng = data.frame(apply(points, 2, range))
  do.call(createProblemSpec, rng)
}
