
library(geometry)
library(rPref)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(MASS)

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
# NOTE: only works in 3D
delaunay.edges = function(ppts) {
  simplices = convhulln(ppts)
  print("simplices done")
  edges = adply(simplices, 1, function(x) t(combn(x,2)), .id=NULL, .parallel=FALSE)
  print("edges done")
  res = data.frame(edges)
  names(res) = c("p1", "p2")
  unique(res)
}

# 3D convex hull w/ plane intersection
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
    pareto.points(res[!is.na(res[,1]),])
  } else {
    # need a proper empty data frame
    res = data.frame(matrix(nrow=0,ncol=length(names(data))))
    names(res) = names(data)
    res
  }
}

intersect.simplex = function(simplex, data, fp, d1, d2) {
  intersects = simplex.intersect.test(d1, d2, fp, data[simplex,])
  interesects
}

# plotting
gen.plot.data = function(data, simplexes, n) {
  d = ncol(data)
  focus.points = data.frame(matrix(runif(d*n), ncol=d))
  dims = t(combn(d,2))
  adply(1:n, 1, function(rid) { # go over all focus points
    fp = focus.points[rid,]
    res = adply(dims, 1, function(d) { # all pairs of dims
      res2 = adply(simplexes, 1, intersect.simplex, data=data, fp=fp, d1=d[1], d2=d[2])
        #intersect.pts(data, edges, fp, d[1], d[2])
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
  simplexes = convhulln(ppts) # these are d-1 dimensional simplexes
  if(nrow(simplexes)==0) warning("cannot generate simplexes")
  plot.data = gen.plot.data(ppts, simplexes, n)
  if(nrow(plot.data)==0) stop("No plane/simplex intersections found")
  plot.data$fpid = factor(plot.data$fpid)
  fields = names(ppts)
  d = ncol(ppts)
  plots = list()
  for(i in 1:(d-1)) { # d1 varies the slowest
    for(j in (i+1):d) {
      pd = plot.data %>% filter(d1==i&d2==j)
      pd = pd[order(pd[,fields[i]]),]
      p = ggplot(pd, aes_string(x=fields[i], y=fields[j], group="fpid")) + 
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

simplex.intersect.test = function(d1, d2, focus.pt, simplex) {
  n = nrow(simplex) # number of lambdas, dimensionality of the simplex+1
  r.last = simplex[n,]
  T = t(sub.rows(simplex, r.last))[,-n]
  T.inv = ginv(T) # T isn't always square
  
  # compute lambda as best we can (there will be 3 parts)
  rr = focus.pt
  rr[c(d1,d2)] = 0
  rr = rr - r.last
  rr.x = array(0,length(focus.pt))
  rr.y = array(0,length(focus.pt))
  rr.x[d1] = rr.y[d2] = 1
  # we are trying to compute T^(-1) . [x-xn,y-yn,...,z-zn]
  lambda.rest = T.inv %*% t(rr) # column vector multiplication
  lambda.x = as.vector(T.inv %*% rr.x)
  lambda.y = as.vector(T.inv %*% rr.y)
  lambda.c = as.vector(lambda.rest)
  
  # find the d1 and d2 ranges that make each lambda 0
  intersect.range = data.frame(d1.min=array(NA,n),d1.max=array(NA,n),
                               d2.min=array(NA,n),d2.max=array(NA,n))
  
  # most indices are based on solving ax + by + c = 0
  # but keeping the other lambdas between 0 and 1
  for(i in 1:(n-1)) {
    rng = NULL
    # put y=mx+b into each other lambda formula and try and get a good range
    for(j in (1:n)[-i]) { # don't intersect i with itself
      # compute the range
      new.rng = NULL
      # figure out the bounds of variable d1 that will keep b_j between 0 and 1
      if(j != n) {
        new.rng = common.cross.range(lambda.x, lambda.y, lambda.c, i, j)
      } else {
        new.rng = lastn.cross.range(lambda.x, lambda.y, lambda.c, i)
      }
      if(!is.null(rng)) # special first case
        new.rng = intersect.ranges(new.rng,rng)
      rng = new.rng
    }
    # problem is b_n = 1 - b_1 - ... - b_(n-1)
    # so this requires special work
    # if we got an intersection then we're good!
    if(all(!is.na(rng))) {
      y.rng = (-lambda.x[i] * rng - lambda.c[i])/lambda.y[i]
      intersect.range[i,"d1.min"] = rng[1]
      intersect.range[i,"d1.max"] = rng[2]
      intersect.range[i,"d2.min"] = y.rng[1]
      intersect.range[i,"d2.max"] = y.rng[2]
    }
  }
  
  # the last index (n) is based on solving 1 - sum_i (a_i x + b_i y + c) = 0
  # but keeping the other lambdas in range
  c.sum = sum(lambda.c)
  x.sum = sum(lambda.x)
  y.sum = sum(lambda.y)
  if(x.sum!=0 && y.sum!=0) {
    x.factor = -x.sum / y.sum
    c.value = (1-c.sum) / y.sum # based on solving the final lambda for 0
    rng = NULL
    for(i in 1:(n-1)) {
      cc = lambda.c[i] + lambda.y[i]*c.value
      xx = lambda.x[i] - lambda.y[i]*x.factor
      new.rng = sort(c(-cc/xx,(1-cc)/xx))
      if(!is.null(rng)) {
        new.rng = intersect.ranges(new.rng, rng)
      }
    }
    y.rng = x.factor * rng + c.value
    intersect.range[n,"d1.min"] = rng[1]
    intersect.range[n,"d1.max"] = rng[2]
    intersect.range[n,"d2.min"] = y.rng[1]
    intersect.range[n,"d2.max"] = y.rng[2]
  }
  
  intersect.range
}

common.cross.range = function(lambda.x, lambda.y, lambda.c, i, j) {
  f1 = (lambda.y[j]*lambda.c[i] - lambda.y[i]*lambda.c[j]) /
    (lambda.x[j]*lambda.y[i] - lambda.x[i]*lambda.y[j])
  f2 = (lambda.y[i] + lambda.y[j]*lambda.c[i] - lambda.y[i]*lambda.c[j]) /
    (lambda.x[j]*lambda.y[i] - lambda.x[i]*lambda.y[j])
  sort(c(f1,f2))
}

lastn.cross.range = function(lambda.x, lambda.y, lambda.c, i) {
  n = length(lambda.x)
  x.sum = 0
  c.sum = 0
  for(k in (1:n)[-i]) {
    x.sum = x.sum + lambda.x[k] - lambda.y[k]*lambda.x[i]/lambda.y[i]
    c.sum = c.sum + lambda.c[k] - lambda.y[k]*lambda.c[i]/lambda.y[i]
  }
  c.sum = 1 - c.sum
  f1 = -c.sum / x.sum
  f2 = (1-c.sum) / x.sum
  sort(c(f1,f2))
}

# subtract a row vector from every row of a matrix
sub.rows = function(mtx, v) {
  adply(mtx, 1, function(r) r - v)
}

# return the intersection of 2 ranges or NA,NA if there's no intersection
intersect.ranges = function(rng1,rng2) {
  if(any(is.na(c(rng1,rng2))))
    return(c(NA,NA))
  rng = c(max(rng1[1],rng2[1]),min(rng1[2],rng2[2]))
  # if the max and min reverse then there is no intersection
  if(rng[2]<rng[1]) 
    rng = c(NA,NA)
  rng
}
