
#' Write the Mesh to disk
#'
#' @param m The mesh to write to disk
#' @param file The name of the file to save
save.mesh = function(m, file) {
  saveRDS(m, file=file)
}

#' Load a Mesh from disk
#'
#' @param file The name of the file to load
load.mesh = function(file) {
  readRDS(file)
}

save.mesh.hdf5 = function(m, file) {
  file.h5 = hdf5r::H5File$new(file, mode="w")
  file.h5[["dimnames"]] = m$problemSpec$dimNames
  file.h5[["support"]] = m$problemSpec$limits
  file.h5[["points"]] = as.matrix(m$points)
  file.h5[["simplices"]] = as.matrix(m$simplices)

  file.h5$close_all()
}

load.mesh.hdf5 = function(file) {
  file.h5 = hdf5r::H5File$new(file, mode="r+")
  dn = file.h5[["dimnames"]]
  lims = file.h5[["support"]]
  pts = file.h5[["points"]]
  s = file.h5[["simplices"]]

  ps.prime = as.list(lims[])
  names(ps.prime) = dn[]
  ps = do.call(createProblemSpec, ps.prime)

  m = list(problemSpec=ps, points=pts[,], simplices=s[,])
  class(m) = "class"

  file.h5$close_all()
  m
}
