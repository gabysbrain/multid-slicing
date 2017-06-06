
library(stringr)

n.samples = 50
r = 1
dim = 3

upper.samp = function(n) {
  d = matrix(NA, nrow=n, ncol=dim)
  for(i in 1:n) {
    # from http://mathworld.wolfram.com/HyperspherePointPicking.html
    x = rep(-1, dim)
    # keep sampling until everything is positive
    while(any(x < 0)) {
      x = rnorm(dim)
    }
    d[i,] = x / sqrt(sum(x*x))
  }
  d = data.frame(d)
  names(d) = str_c("x", 1:dim)
  d
}

cartesian.samples = upper.samp(n.samples)
