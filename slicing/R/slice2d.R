
EPS = 1e-9

intersect.simplices = function(mesh, fp, d1, d2, use.3d.intersection=FALSE) {
  n = nrow(mesh$simplices)
  lim1 = mesh$problemSpec$limits[[mesh$problemSpec$dimNames[d1]]]
  lim2 = mesh$problemSpec$limits[[mesh$problemSpec$dimNames[d2]]]
  if(use.3d.intersection) {
    purrr::map_dfr(1:n,
           function(i) intersect.tri(mesh$points[mesh$simplices[i,],], fp, d1, d2))
  } else {
    purrr::map_dfr(1:n,
           function(i) simplex.point.intersection(mesh$points[mesh$simplices[i,],], fp, d1, d2))
  }
}

clip_row = function(r, lim1, lim2) {
  c1 = clip.seg(r$d1Min, r$d1Max, lim1[1], lim1[2])
  c2 = clip.seg(r$d2Min, r$d2Max, lim2[1], lim2[2])
  data.frame(d1Min=c1[1], d1Max=c1[2], d2Min=c2[1], d2Max=c2[2])
}

clip.seg = function(x1, x2, limMin, limMax) {
  if(x1 < limMin & x2 < limMin) {
    c(NA, NA)
  } else if(x1 > limMax & x2 > limMax) {
    c(NA, NA)
  } else {
    c(min(max(x1, limMin), limMax), min(max(x2, limMin), limMax))
  }
}

simplex.point.intersection = function(simplex, focus.pt, d1, d2) {
  n = ncol(simplex)+1 # number of lambdas to check, dimensionality of the space+1
  T = rbind(t(simplex), rep(1,nrow(simplex)))
  startCheckI = 1
  # T may not be square so if it is then pad with the intersection point
  if(nrow(T) != ncol(T)) {
    T = cbind(T, c(focus.pt, 1))
    startCheckI = ncol(T) # only consider final lambda value
    n = 1
  }
  # if T is singluar then the simplex lies in a plane
  if(det(T)==0) {
    res = data.frame(d1Min=NA, d1Max=NA, d2Min=NA, d2Max=NA)
    return(res)
  }
  T = matrix(unlist(T), ncol=ncol(T)) # need to force T to be a matrix

  # compute lambda as best we can (there will be 3 parts)
  rr = c(unlist(focus.pt), 1)
  rr[c(d1,d2)] = 0
  rr.x = array(0,length(rr))
  rr.y = array(0,length(rr))
  rr.x[d1] = rr.y[d2] = 1
  # we are trying to compute T^(-1) . [x-xn,y-yn,...,z-zn]
  lambda.c = as.vector(solve(T, rr)) # column vector multiplication
  lambda.x = as.vector(solve(T, rr.x))
  lambda.y = as.vector(solve(T, rr.y))

  # find the d1 and d2 ranges that make each lambda 0
  # most indices are based on solving ax + by + c = 0
  # but keeping the other lambdas between 0 and 1
  intersect.range = purrr::map_dfr(1:n, function(i) { # i is the index into intersect.range
    # put y=mx+b into each other lambda formula and try and get a good range
    ranges = common.cross.range(lambda.x, lambda.y, lambda.c, startCheckI+i-1)
    #ranges = common.cross.range(lambda.x, lambda.y, lambda.c, i)
    if(!is.na(ranges$x[1])) {
     # if(min(abs(ranges$x-focus.pt[d1])) > EPS) { # only if we don't hit the extra focus point
        data.frame(d1Min=ranges$x[1], d1Max=ranges$x[2], d2Min=ranges$y[1], d2Max=ranges$y[2])
     # }
    } else {
      data.frame(d1Min=NA, d1Max=NA, d2Min=NA, d2Max=NA)
    }
  })

  intersect.range
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
