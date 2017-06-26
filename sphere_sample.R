
library(stringr)

n.samples = 50
r = 1
dim = 3

upper.samp = function(n,dims) {
  d = matrix(NA, nrow=n, ncol=dims)
  for(i in 1:n) {
    # from http://mathworld.wolfram.com/HyperspherePointPicking.html
    x = rep(-1, dims)
    # keep sampling until everything is positive
    while(any(x < 0)) {
      x = rnorm(dims)
    }
    d[i,] = x / sqrt(sum(x*x))
  }
  d = data.frame(d)
  names(d) = str_c("x", 1:dims)
  d
}

surface.samples = upper.samp(n.samples, dim)
