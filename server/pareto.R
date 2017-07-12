
library(geometry)
library(mco)
library(plyr)

# filters a dataset to only include the pareto points
pareto.points = function(data) {
  # paretoFilter is the only one that filters
  # also, it refuses to take a dataframe
  ppts = paretoFilter(as.matrix(data))
  res = data.frame(ppts)
  names(res) = names(data)
  res
}

# should be given pareto points!
delaunay.edges = function(ppts) {
  simplices = delaunayn(ppts)
  edges = adply(simplices, 1, function(x) t(combn(x,2)), .id=NULL)
  res = data.frame(edges)
  names(res) = c("p1", "p2")
  res
}
