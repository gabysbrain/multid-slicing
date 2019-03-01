
#' Generate a number of samples according to the ranges in the problem spec
#'
#' Currently generates samples based on Sobol sequences
#'
#' @param ps The \code{ProblemSpec} containing the dim names and the ranges
#' @param n How many samples to generate
#' @return A data frame with \code{n} rows for each dimension
sample.ProblemSpec = function(ps, n) {
  d = length(ps$dimNames)
  x = data.frame(randtoolbox::sobol(n, d))

  # set up the data frame
  names(x) = ps$dimNames
  for(dn in ps$dimNames) {
    r = ps$limits[[dn]]
    # scale the values to the limits of the dimension
    x[,dn] = r[1] + (r[2] - r[1]) * x[,dn]
  }
  x
}
