library(jsonlite)
library(stringr)
source('server/pareto.R')

write.hull.json = function(fname, points, filter.pareto=FALSE, n=10) {
  simplexes = convhulln(points) # these are d-1 dimensional simplexes
  if(nrow(simplexes)==0) warning("cannot generate simplexes")
  curve.data = gen.plot.data(points, simplexes, n)
  if(filter.pareto) {
    curve.data = filter.pareto(curve.data)
  }
  json = list(
    points=fix.pointdata(points),
    curves=curve.data
  )
  write_json(json, fname)
  #if(nrow(plot.data)==0) stop("No plane/simplex intersections found")
  #setup.plot.data(plot.data, filter.pareto)
}

filter.pareto = function(data) {
  d = data
  d$rid = 1:nrow(d)
  mins = d[,c("d1.min","d2.min","d1","d2","fpid","rid")]
  maxes = d[,c("d1.max","d2.max","d1","d2","fpid","rid")]
  res = data.frame(rbind(as.matrix(mins), as.matrix(maxes)))
  names(res) = c("x1","x2","d1","d2","fpid","rid")
  res[1:nrow(d),"type"] = "min"
  res[(nrow(d)+1):(2*nrow(d)),"type"] = "max"
  # find the pareto points, we need the rowids to filter data
  pr = res %>%
    group_by(d1, d2, fpid) %>%
    pareto.points(nms=c("x1","x2")) %>%
    ungroup() %>%
    as.data.frame() %>%
    select(rid, type)
  # only allow rows where both the min and max are pareto
  pareto.rows = inner_join(pr[pr$type=="min",], pr[pr$type=="max",], by="rid")$rid
  data[pareto.rows,]
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
