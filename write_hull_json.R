library(jsonlite)
library(stringr)
source('server/pareto.R')

write.hull.json = function(fname, points, filter.pareto=FALSE, n=10) {
  if(nrow(points)<ncol(points)+1) {
    warning("insufficient point list")
    write_json(list(points=c(), curves=c()), fname)
    return()
  }
  simplexes = convhulln(points) # these are d-1 dimensional simplexes
  if(nrow(simplexes)==0) warning("cannot generate simplexes")
  curve.data = gen.plot.data(points, simplexes, n)
  if(filter.pareto) {
    curve.data = filter.pareto.segments(curve.data)
  }
  json = list(
    points=fix.pointdata(points),
    curves=curve.data
  )
  write_json(json, fname)
  #if(nrow(plot.data)==0) stop("No plane/simplex intersections found")
  #setup.plot.data(plot.data, filter.pareto)
}

fix.pointdata = function(pd) {
  if(!is.data.frame(pd)) {
    pd = data.frame(pd)
  }
  if(is.null(names(pd))) {
    names(pd) = str_c("x", 1:ncol(pd))
  }
  pd
}
