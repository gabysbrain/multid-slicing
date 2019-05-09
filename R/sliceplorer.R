
#' Create a number of 1D slices of a function
#'
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
