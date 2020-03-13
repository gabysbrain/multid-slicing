
#' Create a number of 1D slices of a function
#'
#' @param f A function that takes a single vector parameter and returns a single scalar value
#' @param problemSpec A \code{ProblemSpec} object containing the limits of parameters
#' @param n The number of slices to create
#' @param density (optional) The resolution along each axis to create the plots
#' @return An object of class \code{SliceSet1D}:
#'           \code{problemSpec} a reference to the problem spec passed into the function
#'           \code{focusPoints} a table containing a list of focus points of \code{n} rows
#'           \code{slices} a table of slice segments (line segments) consisting of the following columns:
#'                         \begin{enumerate}
#'                           \item \code{fpid} focus point id (index into \code{focusPoints})
#'                           \item \code{d} dimension number of the slice
#'                           \item \code{x} x-coordinate of the slice segment
#'                           \item \code{y} y-coordinate of the slice segment
#'                         \end{enumerate}
sliceplorer = function(f, problemSpec, n, density=25) {
  d = length(problemSpec$dimNames)
  focus.points = sample.ProblemSpec(problemSpec, n)

  slices = purrr::map_dfr(1:n, function(fpid) {
    tmp = purrr::map_dfr(1:d, function(dim) {
      sp.dim(f, problemSpec, focus.points[fpid,], dim, density)
    })
    tmp$fpid = fpid
    tmp
  })

  ret = list(problemSpec = problemSpec,
             focusPoints = focus.points,
             slices = as.data.frame(slices))
  class(ret) = "SliceSet1D"
  ret
}

sp.dim = function(f, ps, fp, dim, density) {
  pts = matrix(rep(fp, density), nrow=density, byrow=TRUE)
  pts[,dim] = seq(ps$limits[[dim]][1], ps$limits[[dim]][2], length.out=density) # only vary the points across one dim
  purrr::map_dfr(1:density, function(i) {
    y = f(unlist(pts[i,]))
    list(d=dim, x=unlist(pts[i,dim]), y=y)
  })
}
