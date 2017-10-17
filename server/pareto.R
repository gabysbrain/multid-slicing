
library(geometry)
#library(rPref)
library(plyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(randtoolbox)
#library(rgl)

EPS = 1e-9

# filters a dataset to only include the pareto points
pareto.points = function(data, nms=names(data)) {
  # TODO: need to indicate our preferences (maximize all)
  preff = high_
  prefs = Reduce(function(p,nm) p*preff(nm), tail(nms,n=-1), preff(nms[1]))
  psel(data, prefs)
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

intersect.simplices = function(simplices, data, fp, d1, d2) {
  adply(simplices, 1, function(s) simplex.intersect.test(d1, d2, fp, data[s,]))
}

# plotting
gen.plot.data = function(data, simplexes, n) {
  d = ncol(data)
  focus.points = data.frame(sobol(n,d))
  dims = t(combn(d,2))
  adply(1:n, 1, function(rid) { # go over all focus points
    fp = focus.points[rid,]
    res = adply(dims, 1, function(d) { # all pairs of dims
      res2 = intersect.simplices(simplexes, data, fp, d[1], d[2])
      # remove all the NA rows
      res2 = filter_all(res2, all_vars(!is.na(.)))
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

setup.plot.data = function(intersect.data, filter.pareto=TRUE) {
  mins = intersect.data[,c("d1.min","d2.min","d1","d2","fpid")]
  maxes = intersect.data[,c("d1.max","d2.max","d1","d2","fpid")]
  res = data.frame(rbind(as.matrix(mins), as.matrix(maxes)))
  names(res) = c("x1","x2","d1","d2","fpid")
  # need to only show points that are pareto (these are cross-connections in the convex hull)
  if(filter.pareto) {
    res %>%
      group_by(d1, d2, fpid) %>%
      pareto.points() %>%
      ungroup() %>%
      as.data.frame()
  } else {
    res
  }
}

#' Produce a set of 2D slices of the convex hull of a set of points
plot.hull.discrete = function(ppts, dim.labels=NA, n=10, filter.pareto=TRUE) {
  simplexes = convhulln(ppts) # these are d-1 dimensional simplexes
  if(nrow(simplexes)==0) warning("cannot generate simplexes")
  plot.data = gen.plot.data(ppts, simplexes, n)
  if(nrow(plot.data)==0) stop("No plane/simplex intersections found")
  plot.data = setup.plot.data(plot.data, filter.pareto)
  plot.data$fpid = factor(plot.data$fpid) # keeps the fp color consistent
  d = ncol(ppts)
  plots = list()
  for(i in 1:(d-1)) { # d1 varies the slowest
    for(j in (i+1):d) {
      pd = plot.data %>% filter(d1==i&d2==j) %>% unique() %>%
                     group_by(fpid) %>%
                     do(cbind(., data.frame(theta=clock.angle(.)))) %>%
                     arrange(fpid, theta)
      p = ggplot(pd, aes(x=x1,y=x2,group=fpid)) #+
            #geom_point(size=0.5)
      if(filter.pareto) { # see if we're dealing with polygons or not
        p = p + geom_path(aes(colour=fpid))
      } else {
        p = p + geom_polygon(aes(colour=fpid), fill=NA)
      }
      p = p + #scale_x_continuous(limits=c(0,1)) +
              #scale_y_continuous(limits=c(0,1)) +
              theme_bw() +
              theme(axis.title.x=element_blank(),
                    axis.title.y=element_blank())
      plots = lappend(plots, p)
    }
  }
  
  # need axis names as well
  if(is.na(dim.labels)) {
    dim.labels = paste("x", 1:d, sep="")
  }
  horiz.labels = Map(textGrob, dim.labels[1:(d-1)])
  vert.labels = Map(textGrob, dim.labels[2:d])
  layout = splom.layout(d)
  layout = rbind(max(layout, na.rm=TRUE)+1:(d-1), layout) # put the labels after
  layout = cbind(c(NA, max(layout, na.rm=TRUE)+1:(d-1)), layout)
  grid.arrange(grobs=c(plots, horiz.labels, vert.labels), layout_matrix=layout,
               widths=c(0.1,rep(1,d-1)), heights=c(0.2, rep(1,d-1)))
}

simplex.intersect.test = function(d1, d2, focus.pt, simplex) {
  focus.pt = as.vector(unlist(focus.pt))
  n = ncol(simplex)+1 # number of lambdas, dimensionality of the space+1
  n.lambdas = ncol(simplex) + 1
  T = rbind(t(simplex), rep(1,nrow(simplex)))
  # T may not be square so if it isn't then pad with the intersection point
  if(nrow(T) != ncol(T)) {
    T = cbind(T, c(focus.pt, 1))
    #T[d1,ncol(T)] = T[d2,ncol(T)] = -1
    #n.lambdas = n.lambdas - 1 # don't try and intersect anything with this extra point
  }
  # if T is singluar then the simplex lies in the plane we're looking at
  if(det(T)==0) {
    rows = t(combn(nrow(simplex), 2))
    res = data.frame(cbind(simplex[rows[,1], c(d1,d2)], simplex[rows[,2], c(d1,d2)]))
    names(res) = c("d1.min", "d2.min", "d1.max", "d2.max")
    return(res)
  }
  T = matrix(unlist(T), ncol=ncol(T)) # need to force T to be a matrix
  T.inv = solve(T)

  # compute lambda as best we can (there will be 3 parts)
  rr = c(unlist(focus.pt), 1)
  rr[c(d1,d2)] = 0
  rr.x = array(0,length(rr))
  rr.y = array(0,length(rr))
  rr.x[d1] = rr.y[d2] = 1
  # we are trying to compute T^(-1) . [x-xn,y-yn,...,z-zn]
  lambda.c = as.vector(T.inv %*% rr) # column vector multiplication
  lambda.x = as.vector(T.inv %*% rr.x)
  lambda.y = as.vector(T.inv %*% rr.y)

  # find the d1 and d2 ranges that make each lambda 0
  intersect.range = data.frame(d1.min=array(NA,n),d1.max=array(NA,n),
                               d2.min=array(NA,n),d2.max=array(NA,n))

  # most indices are based on solving ax + by + c = 0
  # but keeping the other lambdas between 0 and 1
  for(i in 1:n.lambdas) {
    # put y=mx+b into each other lambda formula and try and get a good range
    range = common.cross.range(lambda.x, lambda.y, lambda.c, i)
    if(!is.na(range$x[1])) {
      if(min(abs(range$x-focus.pt[d1])) > EPS) { # only if we don't hit the extra focus point
        intersect.range[i,"d1.min"] = range$x[1]
        intersect.range[i,"d1.max"] = range$x[2]
        intersect.range[i,"d2.min"] = range$y[1]
        intersect.range[i,"d2.max"] = range$y[2]
      }
    }
  }

  intersect.range
}

# plot the simplex and the intersection plane
# useful for debugging random errors
plot.intersect = function(tri.pts, focus.pt, d1, d2) {
  normal = array(1, 3)
  normal[c(d1,d2)] = 0
  plane.pts = c(normal, normal %*% -focus.pt)
  simplex = rbind(as.matrix(tri.pts), t(focus.pt))
  base = matrix(c(0,0,0,0,1,0,1,1,0,1,0,0), ncol=3, byrow=TRUE)
  #base = base * 10
  #base = base - 5
  
  open3d()
  quads3d(xyz.coords(base), alpha=0.2)
  tetramesh(matrix(1:4, nrow=1), simplex, clear=FALSE)
  #planes3d(a=0, b=0, c=1, d=-0.75+1e-5, col="green")
  planes3d(a=plane.pts[1], b=plane.pts[2], c=plane.pts[3], d=plane.pts[4]+1e-5, col="green")
  points3d(focus.pt[1], focus.pt[2], focus.pt[3], col="blue", size=10)
  axes3d(col="black")
  title3d(xlab="x", ylab="y", zlab="z")
 # bbox3d(col="grey", alpha=0.7)
}

clock.angle = function(pts) {
  centroid = colMeans(pts[,c(1,2)])
  recentered.pts = sweep(pts[,c(1,2)], 2, centroid, FUN="-")
  unlist(as.vector(mapply(atan2, recentered.pts[1], recentered.pts[2])))
}

common.cross.range = function(lambda.x, lambda.y, lambda.c, i) {
  x.rng = c(NA, NA)
  y.rng = c(NA, NA)
  # if one of the factors is 0 things need to be done differently
  if(lambda.x[i] == 0 & lambda.y[i] == 0) { # FIXME: this could be done more elegantly
    # no intersection
    x.rng = c(NA, NA)
    y.rng = c(NA, NA)
  } else if(lambda.x[i] == 0) {
    # solve ly * y + c = 0
    y = -lambda.c[i] / lambda.y[i]
    # replace y in all other formulas and solve lx * x + ly * y + lc >=0 for x
    xs = lambda.x[-i]
    cs = lambda.y[-i] * y + lambda.c[-i]
    res = -cs / xs
    if(any(xs==0) & any(cs[xs==0] < 0)) { # no intersection
      x.rng = c(NA, NA)
      y.rng = c(NA, NA)
    } else {
      xs.p = xs[is.finite(res)] # for filtering res later
      res = res[is.finite(res)]
      x.rng = c(max(res[xs.p>=0]), min(res[xs.p<0]))
      y.rng = c(y, y)
      # some bounds checking
      if(x.rng[2]-x.rng[1]<(-EPS)) { # y may be in the wrong direction
        x.rng = c(NA, NA)
        y.rng = c(NA, NA)
      }
    }
  } else if(lambda.y[i] == 0) {
    # solve lx * x + c = 0
    x = -lambda.c[i] / lambda.x[i]
    # replace x in all other formulas and solve lx * x + ly * y + lc >=0 for y
    ys = lambda.y[-i]
    cs = lambda.x[-i] * x + lambda.c[-i]
    res = -cs / ys
    if(any(ys==0) & any(cs[ys==0] < 0)) { # no intersection
      x.rng = c(NA, NA)
      y.rng = c(NA, NA)
    } else {
      res = -cs / ys
      ys.p = ys[is.finite(res)] # for filtering res later
      res = res[is.finite(res)]
      x.rng = c(x, x)
      y.rng = c(max(res[ys.p>=0]), min(res[ys.p<0]))
      # some bounds checking
      if(y.rng[2]-y.rng[1]<(-EPS)) { # y may be in the wrong direction
        x.rng = c(NA, NA)
        y.rng = c(NA, NA)
      }
    }
  } else {
    # assumes lx[i] and ly[i] are non-zero
    # solve lx * x + ly * y + c = 0 and substitute
    xs = lambda.x[-i] - lambda.y[-i] * lambda.x[i] / lambda.y[i]
    cs = lambda.c[-i] - lambda.y[-i] * lambda.c[i] / lambda.y[i]
    # need to solve xs + cs >= 0
    res = -cs / xs
    if(any(xs==0) & any(cs[xs==0] < 0)) { # no intersection
      x.rng = c(NA, NA)
      y.rng = c(NA, NA)
    } else {
      xs.p = xs[is.finite(res)] # for filtering res later
      res = res[is.finite(res)]
      x.rng = c(max(res[xs.p>=0]), min(res[xs.p<0])) # divide by negative switches the inequality
      y.rng = (-lambda.c[i] -lambda.x[i] * x.rng) / lambda.y[i]
      # some bounds checking
      if(x.rng[2]-x.rng[1]<(-EPS)) { # y may be in the wrong direction
        x.rng = c(NA, NA)
        y.rng = c(NA, NA)
      }
    }
  }
  
  list(x=x.rng, y=y.rng)
}

filter.pareto.segments = function(data) {
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
