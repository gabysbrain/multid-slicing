
EPS = sqrt(.Machine$double.eps)
#EPS = 1e-9

empty.slice = data.frame(p1_1=c(), p1_2=c(), p2_1=c(), p2_2=c())

intersect.simplices = function(mesh, fp, d1, d2, use.3d.intersection=FALSE) {
  n = nrow(mesh$simplices)
  pts = matrix(unlist(mesh$points), nrow=nrow(mesh$points))
  simpls = matrix(unlist(mesh$simplices), nrow=nrow(mesh$simplices))
  fp.vect = unlist(fp)
  if(use.3d.intersection) {
    purrr::map_dfr(1:n,
           function(i) intersect.tri(mesh$points[mesh$simplices[i,],], fp, d1, d2))
  } else {
    purrr::map_dfr(1:n,
           #function(i) simplex.point.intersection(pts[simpls[i,],], fp.vect, d1, d2))
           function(i) spi2(pts[simpls[i,],], fp.vect, d1, d2))
  }
}

simplex.point.intersection = function(simplex, focus.pt, d1, d2) {
  # TODO: do some argument checking

  n = length(focus.pt)+1 # number of lambdas to check, dimensionality of the space+1

  # This does T = rbind(t(simplex), rep(1,nrow(simplex))) but is sometimes faster
  T = matrix(1, ncol=n, nrow=n) # T needs to be square
  T[1:n-1,1:n-1] = t(simplex)
  T[,n] = c(focus.pt, 1)
  det.T = det(T)
  if(det.T == 0) { # extra focus point may be in the simplex
    T[-1,n] = focus.pt + EPS # add small amount to make matrix non-singular
    det.T = det(T)
  }

  # if T is still singluar then the simplex lies in the plane
  if(det.T==0) {
    rows = t(combn(nrow(simplex), 2))
    res = data.frame(cbind(simplex[rows[,1], c(d1,d2)], simplex[rows[,2], c(d1,d2)]))
    names(res) = c("p1_1", "p1_2", "p2_1", "p2_2")
    return(res)
  }

  # compute lambda as best we can (there will be 3 parts)
  rr = c(focus.pt, 1)
  rr[c(d1,d2)] = 0
  rr.x = array(0,length(rr))
  rr.y = array(0,length(rr))
  rr.x[d1] = rr.y[d2] = 1
  # we are trying to compute T^(-1) . [x-xn,y-yn,...,z-zn]
  lambda.c = solve(T, rr) # column vector multiplication
  lambda.x = solve(T, rr.x)
  lambda.y = solve(T, rr.y)

  # find the d1 and d2 ranges that make each lambda 0
  # most indices are based on solving ax + by + c = 0
  # but keeping the other lambdas between 0 and 1

  # put y=mx+b into each other lambda formula and try and get a good range
  ranges = common.cross.range(lambda.x, lambda.y, lambda.c, n) # only consider the last lambda
  if(!is.na(ranges$x[1])) {
    list(p1_1=ranges$x[1], p2_1=ranges$x[2], p1_2=ranges$y[1], p2_2=ranges$y[2])
  } else {
    empty.slice
  }
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

spi2 = function(simplex, focus.pt, d1, d2) {
  dd = dim(simplex)
  n = length(focus.pt)
  l = .Call("spi_wrapper", simplex, dd[1], dd[2], focus.pt, n, d1, d2)
  if(length(l) > 0) {
    names(l) = c("p1_1", "p1_2", "p2_1", "p2_2")
    l
  } else {
    empty.slice
  }
}
