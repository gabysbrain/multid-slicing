
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
hyperslice <- function(mesh, n, focus.points) {
  if(missing(n) && missing(focus.points)) {
    stop("either n or focus.points must be specified")
  }

  d = ncol(mesh$points)

  # set up focus points
  if(missing(focus.points)) {
    if(!is.integer(n)) {
      stop("n must be an integer")
    }
    focus.points = data.frame(sobol(n,d))
    focus.points = focus.points * 2 - 1 # everything between -1 and 1
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
  curves = plyr::adply(1:n, 1, function(rid) { # go over all focus points
    fp = focus.points[rid,]
    res = plyr::adply(dims, 1, function(d) { # all pairs of dims
      res2 = intersect.simplices(mesh, fp, d[1], d[2])
      # remove all the NA rows
      res2 = dplyr::filter_all(res2, dplyr::all_vars(!is.na(.)))
      if(nrow(res2)>0) {
        res2$d1 = d[1]
        res2$d2 = d[2]
      }
      res2
    })
    if(nrow(res) > 0) {
      res$fpid = rid
    }
    res
  })
  slices = curves[,-1]
  names(slices) = c("x1", "y1", "x2", "y2", "d1", "d2", "fpid")
  list(focusPoints=focus.points,
       slices=slices)
}

