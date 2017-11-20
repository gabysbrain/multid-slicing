source('server/pareto.R')
source('write_hull_json.R')
source('poly_sample.R')
source('klein_bottle_datagen.R')
library(stringr)

n.slices = 50
tau = (1+sqrt(5)) / 2 # golden ratio

norm.data = function(d) {
  d / max(abs(d))
}

poly.space.convhull = function(pts, fname) {
  deg = ncol(pts)
  name = str_c(fname, "_", deg)
  print(name)
  write.csv(pts, file=str_c("csv/", name, ".csv"), row.names=FALSE)
  write.hull.json(str_c("json/", name, ".json"), pts, n=n.slices)
}

# 3d sphere pareto
sphere.3d = read.csv("static/test_data/3sphere_50.csv")
sphere.3d.p = pareto.points(sphere.3d)
write.hull.json("json/sphere_3d.json", sphere.3d.p, filter.pareto=FALSE, n=n.slices)

# 5d sphere pareto
sphere.5d = read.csv("static/test_data/sphere_50.csv")
sphere.5d.p = pareto.points(sphere.5d)
write.hull.json("json/sphere_5d.json", sphere.5d.p, filter.pareto=FALSE, n=n.slices)

# tet
tet.data = data.frame(x1=c(1,1,-1,-1), x2=c(1,-1,1,-1), x3=c(1,-1,-1,1))
tet.data = norm.data(tet.data)
write.hull.json("json/tet.json", tet.data, filter.pareto=FALSE, n=n.slices)

# cube
cube.data = expand.grid(x1=c(-1,1), x2=c(-1,1), x3=c(-1,1))
write.hull.json("json/cube.json", cube.data, filter.pareto=FALSE, n=n.slices)

# octahedron
oct.data = data.frame(rbind(diag(3), -1 * diag(3)))
names(oct.data) = c("x1", "x2", "x3")
oct.data = norm.data(oct.data)
write.hull.json("json/octahedron.json", oct.data, filter.pareto=FALSE, n=n.slices)

# dodecahedron
doda.data = rbind(
  expand.grid(x1=c(-1,1), x2=c(-1,1), x3=c(-1,1)),
  expand.grid(x1=0, x2=c(-1/tau,1/tau), x3=c(-tau,tau)),
  expand.grid(x1=c(-1/tau,1/tau), x2=c(-tau,tau), x3=0),
  expand.grid(x1=c(-tau,tau), x2=0, x3=c(-1/tau,1/tau))
)
doda.data = norm.data(doda.data)
write.hull.json("json/dodecahedron.json", doda.data, filter.pareto=FALSE, n=n.slices)

# isocahedron
isoc.data = rbind(
  expand.grid(x1=0, x2=c(-1,1), x3=c(-tau,tau)),
  expand.grid(x1=c(-1,1), x2=c(-tau,tau), x3=0),
  expand.grid(x1=c(-tau,tau), x2=0, x3=c(-1,1))
)
isoc.data = norm.data(isoc.data)
write.hull.json("json/isocahedron.json", isoc.data, filter.pareto=FALSE, n=n.slices)

# 5-cell (4D simplex)
cell5.data = data.frame(
  x1=c(1,1,-1,-1,0), 
  x2=c(1,-2,1,-1,0), 
  x3=c(1,-1,-1,1,0), 
  x4=c(-1/sqrt(5), -1/sqrt(5), -1/sqrt(5), -1/sqrt(5), sqrt(5)-1/sqrt(5))
)
cell5.data = norm.data(cell5.data)
write.hull.json("json/4d_simplex.json", cell5.data, filter.pareto=FALSE, n=n.slices)

# tesseract
tess.data = expand.grid(x1=c(-1,1), x2=c(-1,1), x3=c(-1,1), x4=c(-1,1))
write.hull.json("json/4d_cube.json", tess.data, filter.pareto=FALSE, n=n.slices)

# 16-cell (4D octahedron)
cell16.data = data.frame(rbind(diag(4), -1*diag(4)))
names(cell16.data) = str_c("x", 1:4)
write.hull.json("json/16_cell.json", cell16.data, filter.pareto=FALSE, n=n.slices)

# 24-cell
cell24.data = rbind(
  expand.grid(x1=c(-1,1), x2=c(-1,1), x3=0, x4=0),
  expand.grid(x1=c(-1,1), x2=0, x3=c(-1,1), x4=0),
  expand.grid(x1=c(-1,1), x2=0, x3=0, x4=c(-1,1)),
  expand.grid(x1=0, x2=c(-1,1), x3=c(-1,1), x4=0),
  expand.grid(x1=0, x2=c(-1,1), x3=0, x4=c(-1,1)),
  expand.grid(x1=0, x2=0, x3=c(-1,1), x4=c(-1,1))
)
cell24.data = norm.data(cell24.data)
write.hull.json("json/24_cell.json", cell24.data, filter.pareto=FALSE, n=n.slices)

# 5-simplex
simplex5.data = data.frame(
  x1=c(rep(1/sqrt(15),5), -sqrt(5)/sqrt(3)),
  x2=c(rep(1/sqrt(10),4), -2*sqrt(2)/sqrt(5), 0),
  x3=c(rep(1/sqrt(6),3), -sqrt(3)/sqrt(2), 0, 0),
  x4=c(rep(1/sqrt(3),2), -2/sqrt(3), 0, 0, 0),
  x5=c(1, -1, 0, 0, 0, 0)
)
simplex5.data = norm.data(simplex5.data)
write.hull.json("json/5d_simplex.json", simplex5.data, filter.pareto=FALSE, n=n.slices)

# 5-cube
cube5.data = expand.grid(x1=c(-1,1), x2=c(-1,1), x3=c(-1,1), x4=c(-1,1), x5=c(-1,1))
write.hull.json("json/5d_cube.json", cube5.data, filter.pareto=FALSE, n=n.slices)

# 5-ortho
ortho5.data = rbind(diag(5), -1*diag(5))
names(ortho5.data) = str_c("x", 1:5)
write.hull.json("json/5d_ortho.json", ortho5.data, filter.pareto=FALSE, n=n.slices)

# klein bottle
samples = data.frame(theta=2*pi * runif(n), phi=2*pi * runif(n))
kb.data = kleinb(samples$theta, samples$phi)
kb.data = data - min(data)
kb.data = data / max(data)
write.hull.json("json/klein.json", kb.data, filter.pareto=FALSE, n=n.slices)

# Polynomials
sample.polys = function(deg, face, faceval) {
  sample.f = function(d) ran.poly(d, scale=FALSE)
  coeffs = sample.poly(n, deg, sample.f)
  coeffs[,face] = faceval
  ds = data.frame(coeffs)
  nms = str_c("a_", 0:deg)
  names(ds) = nms
  ds$is.pos = apply(coeffs, 1, is.positive, domain=c(0,1))
  ds$is.bern = apply(coeffs, 1, function(x) all(bern.coeffs(x)>=0))
  
  poly.space.convhull(ds[ds$is.pos,nms[-face]],
                      str_c("pos_poly_", face, "f", faceval))
  poly.space.convhull(ds[ds$is.bern,nms[-face]],
                      str_c("bernstein_", face, "f", faceval))
  poly.space.convhull(ds[ds$is.pos&!ds$is.bern,nms[-face]],
                      str_c("difference_", face, "f", faceval))
}

# We want to compute positive, bernstein, and their difference using 
# the same set of samples
min.deg = 3
max.deg = 5
n = 10000
# Bernstein polynomials
for(deg in min.deg:max.deg) {
  # set each face to +1 and -1
  for(face in 1:(deg+1)) {
    sample.polys(deg, face, -1)
    sample.polys(deg, face, 1)
  }
}
