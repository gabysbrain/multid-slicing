library(foreach)

sample.poly = function(n, deg, samplef) {
  f = if(is.character(samplef)) {
    get(samplef)
  } else {
    samplef
  }
  
  coeffs = times(n) %do% f(deg)
  matrix(coeffs, ncol=deg+1, byrow=TRUE)
}

# returns a random polynomial in increasing order of power
ran.poly = function(deg) {
  runif(deg+1, min=-1, max=1)
}

eval.poly = function(p, x) {
  sum = 0
  for(i in 0:(length(p)-1)) {
    sum = sum + p[i+1] * x**i
  }
  sum
}

is.positive = function(p, domain) {
  roots = polyroot(p)
  real.roots = Re(roots[Im(roots)==0])
  # if real.roots is length 0 then there are problems with the comparison
  has.no.roots = if(length(real.roots) == 0) {
    TRUE
  } else {
    !any((real.roots > domain[1])&(real.roots < domain[2]))
  }
  is.pos = eval.poly(p, domain[1]) > 0 | eval.poly(p, domain[2]) > 0
  is.pos & has.no.roots
}

# positive polynomial of a particular degree
# uses rejection sampling
# returns a random polynomial in increasing order of power
ran.positive.poly = function(deg, domain=c(0, 1)) {
  while(TRUE) {
    p = ran.poly(deg)
    if(is.positive(p, domain))
      return(p)
  }
}

# returns a polynomial as a vector of coefficients in increasing order
ran.bernstein = function(deg) {
  coeffs = runif(deg+1, min=0, max=1)
  bern.poly(coeffs)
}

bern.poly = function(coeffs) {
  deg = length(coeffs)-1
  ans = c(0) # 0 polynomial
  for(v in 0:deg) {
    b = bernstein.basis(v, deg)
    ans = add.poly(ans, coeffs[v+1]*b)
  }
  ans
}

# returns a polynomial as a vector of coefficients in increasing order
# choose(n,v) * x^v * (1-x)^(n-v)
bernstein.basis = function(v, n) {
  if(v>n) {
    stop("v must be <= n")
  }
  ans = inner.bern(n-v)
  # multiplying by x^v shifts the vector over v steps
  ans = c(rep(0, v), ans)
  choose(n, v) * ans
}

# the expansion of (1-x)^i
inner.bern = function(i) {
  ans = c(1)
  if(i == 0) return(1)
  for(j in 1:i) {
    ans = add.poly(ans, -1 * c(0, ans)) # repeated multiplication is shift/add
  }
  ans
}

# add 2 polynomials together. Polynomials should be given by a vector of 
# coefficients in increasing order
add.poly = function(p1, p2) {
  # The code is easier if we ensure p1 is smaller
  if(length(p1) > length(p2)) {
    tmp = p1
    p1 = p2
    p2 = tmp
  }
  ans = p1 + p2[1:length(p1)]
  if(length(p2) > length(p1)) {
    ans = c(ans, p2[(length(p1)+1):length(p2)])
  }
  ans
}

