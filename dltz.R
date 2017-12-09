g = function(x) {
  sum((x-0.5)**2)
}
dltz5 = function(I,M) {
  f = function(x) {
    theta = c(pi/2 * x[1:(I-1)], (pi*(1+2*g(x)*x[I:(M-1)]))/(4*(1+g(x))))
    tmp = 1 + 100*g(x)
    tmp * c(prod(cos(theta)), unlist(Map(function(i) prod(cos(theta[i:(M-i)]))*sin(theta[M-i+1]), 2:(M-1))), sin(theta[1]))
  }
  function(x) {
    if(is.null(dim(x))) f(x)
    else t(apply(x, 1, f))
  }
}
