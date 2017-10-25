source('server/pareto.R')
source('write_hull_json.R')
source('poly_sample.R')
library(stringr)

poly.space.convhull = function(pts, fname) {
  deg = ncol(pts) - 1
  name = str_c(fname, "_", deg)
  print(name)
  write.csv(pts, file=str_c("csv/", name, ".csv"), row.names=FALSE)
  write.hull.json(str_c("json/", name, ".json"), pts)
}

# 3d sphere pareto
sphere.3d = read.csv("static/test_data/3sphere_50.csv")
sphere.3d.p = pareto.points(sphere.3d)
write.hull.json("json/sphere_3d.json", sphere.3d.p)

# cube
cube.data = expand.grid(x1=c(0,0.5), x2=c(0,0.5), x3=c(0, 0.5))
write.hull.json("json/cube.json", cube.data)

# 4D hypercube
hcube.data = expand.grid(x1=c(0,0.5), x2=c(0,0.5), x3=c(0, 0.5), x4=c(0,0.5))
write.hull.json("json/4d_cube.json", hcube.data)

# Polynomials
# We want to compute positive, bernstein, and their difference using 
# the same set of samples
max.deg = 2
n = 10000
# Bernstein polynomials
for(deg in 2:max.deg) {
  coeffs = sample.poly(n, deg, ran.poly)
  ds = data.frame(coeffs)
  nms = str_c("a_", 0:deg)
  names(ds) = nms
  ds$is.pos = apply(coeffs, 1, is.positive, domain=c(0,1))
  ds$is.bern = apply(coeffs, 1, function(x) all(bern.coeffs(x)>=0))
  
  poly.space.convhull(ds[ds$is.pos,nms], "pos_poly")
  poly.space.convhull(ds[ds$is.bern,nms], "bernstein")
  poly.space.convhull(ds[ds$is.pos&!ds$is.bern,nms], "difference")
}
