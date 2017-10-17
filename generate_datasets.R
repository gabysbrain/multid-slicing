source('server/pareto.R')
source('write_hull_json.R')
source('poly_sample.R')
library(stringr)

poly.space.convhull = function(deg, f, fname) {
  n.samples = 10000
  
  s = sample.poly(n.samples, deg, f)
  s = data.frame(s)
  names(s) = str_c("x", 1:ncol(s))
  name = str_c(fname, "_", deg)
  write.csv(s, file=str_c("csv/", name, ".csv"), row.names=FALSE)
  write.hull.json(str_c("json/", name, ".json"), s)
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

# Positive polynomials
max.deg = 5
for(deg in 2:max.deg) {
  print(str_c("pos poly deg ", deg))
  poly.space.convhull(deg, ran.positive.poly, "pos_poly")
}

# Bernstein polynomials
for(deg in 2:max.deg) {
  print(str_c("bernstein poly deg ", deg))
  poly.space.convhull(deg, ran.bernstein, "bernstein")
}
