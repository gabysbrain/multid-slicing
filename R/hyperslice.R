
#' Create a number of 2D slices of a multi-dimensional simplical mesh
#'
#' \code{hyperslice} takes a number of slices of a 2D mesh and returns
#' a set of 2D slices as line segments. Either \code{n} or \code{focus.points}
#' must be specified
#'
#' @param mesh A mesh object (e.g. output from \code{\link{convmesh}})
#' @param n (Optional) The number of slices to take
#' @param focus.points (Optional) An \code{n} by \code{dim} matrix of points at which to take slices
#' @return An object of class \code{SliceSet}:
#'           \code{focusPoints} a table containing a list of focus points of \code{n} rows
#'           \code{slices} a table of slice segments (line segments) consisting of the following columns:
#'                         \begin{enumerate}
#'                           \item \code{fpid} focus point id (index into \code{focusPoints})
#'                           \item \code{d1} index of the first dimension (1-based)
#'                           \item \code{d2} index of the second dimension (1-based)
#'                           \item \code{x1} x-coordinate of the slice segment
#'                           \item \code{y1} y-coordinate of the slice segment
#'                           \item \code{x2} x-coordinate of the slice segment
#'                           \item \code{y2} y-coordinate of the slice segment
#'                         \end{enumerate}
hyperslice <- function(mesh, n, focus.points, use.3d.intersection=FALSE) {
  if(class(mesh) != "Mesh") {
    stop("input mesh must be of class Mesh")
  }
  if(missing(n) && missing(focus.points)) {
    stop("either n or focus.points must be specified")
  }

  d = ncol(mesh$points)

  # set up focus points
  if(missing(focus.points)) {
    if(!is.numeric(n)) {
      stop("n must be an integer")
    }
    focus.points = sample.ProblemSpec(mesh$problemSpec, n)
  } else if(any(is.na(focus.points))) {
    if(!is.numeric(n)) {
      stop("n must be an integer")
    }
    # fill in NAs with random numbers
    m = matrix(rep(focus.points, n), ncol=length(focus.points), byrow=TRUE)
    for(c in 1:ncol(m)) {
      x = runif(length(m[is.na(m[,c]),c]))
      r = mesh$problemSpec$limits[[c]]
      # scale the values to the limits of the dimension
      x = r[1] + (r[2] - r[1]) * x
      m[is.na(m[,c]),c] = x
    }
    #m[is.na(m)] = runif(length(m[is.na(m)])) # TODO: make this work with the ranges of the parameters
    focus.points = data.frame(m)
    names(focus.points) = names(mesh$points)
  } else {
    if(is.null(dim(focus.points))) { # fix vector for single row matrix
      focus.points = matrix(focus.points, nrow=1)
    }
    n = nrow(focus.points)
    focus.points = data.frame(focus.points)
    names(focus.points) = names(mesh$points)
  }
  if(ncol(focus.points) != d) {
    stop("wrong number of dims for focus.points")
  }

  dims = t(combn(d,2))
  slices = purrr::map_dfr(1:n, function(rid) { # go over all focus points
    fp = focus.points[rid,]
    res = purrr::map_dfr(1:nrow(dims), function(did) { # all pairs of dims
      dim = dims[did,]
      res2 = intersect.simplices(mesh, fp, dim[1], dim[2], use.3d.intersection)

      if(nrow(res2)>0) {
        res2$d1 = dim[1]
        res2$d2 = dim[2]
      }
      res2
    })
    if(nrow(res) > 0) {
      res$fpid = rid
    }
    res
  })

  if(nrow(slices) > 0) {
    slices = dplyr::filter_all(slices, dplyr::all_vars(!is.na(.))) # remove all the NA rows
  }
  createSliceSet(mesh$problemSpec, focus.points, as.data.frame(slices))
}

