
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
