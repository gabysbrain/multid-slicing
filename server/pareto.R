
library(geometry)
library(rPref)
library(plyr)

# filters a dataset to only include the pareto points
pareto.points = function(data) {
  # need to indicate our preferences (maximize all)
  nms = names(data)
  preff = high_
  prefs = Reduce(function(p,nm) p*preff(nm), tail(nms,n=-1), preff(nms[1]))
  psel(data, prefs)
  #res = data.frame(ppts)
  #names(res) = names(data)
  #res
}

# should only be given pareto points!
delaunay.edges = function(ppts) {
  simplices = convhulln(ppts)
  print("simplices done")
  edges = adply(simplices, 1, function(x) t(combn(x,2)), .id=NULL, .parallel=FALSE)
  print("edges done")
  res = data.frame(edges)
  names(res) = c("p1", "p2")
  res
}
