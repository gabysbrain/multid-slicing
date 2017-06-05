
n.samples = 50000
r = 1

upper.samp = function(n) {
  d = matrix(NA, nrow=n, ncol=5)
  for(i in 1:n) {
    # from http://mathworld.wolfram.com/HyperspherePointPicking.html
    x = rep(-1, 5)
    # keep sampling until everything is positive
    while(any(x < 0)) {
      x = rnorm(5)
    }
    d[i,] = x / sqrt(sum(x*x))
  }
  d = data.frame(d)
  names(d) = c("x1", "x2", "x3", "x4", "x5")
  d
}

cartesian.samples = upper.samp(n.samples)
