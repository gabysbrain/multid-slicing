
library(geometry)
library(rPref)
library(plyr)
library(ggplot2)
library(gridExtra)

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
  unique(res)
}

# convex hull w/ plane intersection
segment.intersects = function(fp, d1, d2, pt1, pt2) {
  # since everything on the d1xd2 plane conatains all numbers we 
  # can just cheat with 0
  fp.p = unlist(fp)
  pt1.p = unlist(pt1)
  pt2.p = unlist(pt2)
  fp.p[c(d1,d2)] = 0
  pt1.p[c(d1,d2)] = 0
  pt2.p[c(d1,d2)] = 0
  if(all(pt1.p == pt2.p)) return(NULL)
  
  t = (fp.p-pt1.p) / (pt2.p-pt1.p)
  t = t[is.finite(t)] # Infs and NaNs aren't important
  
  # only inside the segment between 0 and 1
  if(!is.null(t[1]) & all(t==t[1]) & t[1] >= 0 & t[1] <= 1) {
    pt1 + t[1] * (pt2-pt1)
  } else {
    NULL
  }
}

intersect.pts = function(data, edges, fp, d1, d2) {
  f = function(r) as.data.frame(segment.intersects(fp, d1, d2, data[r$p1,], data[r$p2,]))
  res = adply(edges, 1, f)
  if(nrow(res)>0) {
    res = res[,names(data)]
    res[!is.na(res[,1]),]
  } else {
    # need a proper empty data frame
    res = data.frame(matrix(nrow=0,ncol=length(names(data))))
    names(res) = names(data)
    res
  }
}

# plotting
gen.plot.data = function(data, edges, n) {
  d = ncol(data)
  samples = data.frame(matrix(runif(d*n), ncol=d))
  dims = t(combn(d,2))
  adply(1:n, 1, function(rid) {
    fp = samples[rid,]
    res = adply(dims, 1, function(d) {
      res2 = intersect.pts(data, edges, fp, d[1], d[2])
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
}

# append to a list
lappend <- function(lst, obj) {
  lst[[length(lst)+1]] <- obj
  return(lst)
}

# layout for grid.arrange
splom.layout = function(d) {
  layout = matrix(NA, nrow=d-1, ncol=d-1)
  iter = 1
  num = d-1
  for(c in 1:ncol(layout)) {
    nums = seq(from=iter, length.out=num)
    layout[c:(c+length(nums)-1),c] = nums
    iter = iter + length(nums)
    num = num - 1
  }
  layout
}

plot.hull.discrete = function(ppts, n=10) {
  edges = delaunay.edges(ppts)
  plot.data = gen.plot.data(ppts, edges, n)
  plot.data$fpid = factor(plot.data$fpid)
  fields = names(ppts)
  d = ncol(ppts)
  plots = list()
  for(i in 1:(d-1)) { # d1 varies the slowest
    for(j in (i+1):d) {
      p = ggplot(plot.data %>% filter(d1==i&d2==j), 
                 aes_string(x=fields[i], y=fields[j], group="fpid")) + 
            geom_point() + 
            geom_line(aes(colour=fpid)) +
            scale_x_continuous(limits=c(0,1)) +
            scale_y_continuous(limits=c(0,1)) +
            theme_bw()
      plots = lappend(plots, p)
    }
  }
  layout = splom.layout(d)
  grid.arrange(grobs=plots, layout_matrix=layout)
}