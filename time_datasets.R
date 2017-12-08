source('server/pareto.R')
source('write_hull_json.R')
source('poly_sample.R')
source('klein_bottle_datagen.R')
library(stringr)

n.slices = 1000
tau = (1+sqrt(5)) / 2 # golden ratio

norm.data = function(d) {
  d / max(abs(d))
}

time.data = rbind(
  {
    # cube
    cube.data = expand.grid(x1=c(-1,1), x2=c(-1,1), x3=c(-1,1))
    time.hull.gen("json/cube.json", cube.data, n=n.slices)
  }, {
    # octahedron
    oct.data = data.frame(rbind(diag(3), -1 * diag(3)))
    names(oct.data) = c("x1", "x2", "x3")
    oct.data = norm.data(oct.data)
    time.hull.gen("json/octahedron.json", oct.data, n=n.slices)
  }, {
    # tesseract
    tess.data = expand.grid(x1=c(-1,1), x2=c(-1,1), x3=c(-1,1), x4=c(-1,1))
    time.hull.gen("json/4d_cube.json", tess.data, n=n.slices)
  }, {
    # 16-cell (4D octahedron)
    cell16.data = data.frame(rbind(diag(4), -1*diag(4)))
    names(cell16.data) = str_c("x", 1:4)
    time.hull.gen("json/16_cell.json", cell16.data, n=n.slices)
  }, {
    # 5-cube
    cube5.data = expand.grid(x1=c(-1,1), x2=c(-1,1), x3=c(-1,1), x4=c(-1,1), x5=c(-1,1))
    time.hull.gen("json/5d_cube.json", cube5.data, n=n.slices)
  }, {
    # 5-ortho
    ortho5.data = rbind(diag(5), -1*diag(5))
    names(ortho5.data) = str_c("x", 1:5)
    time.hull.gen("json/5d_ortho.json", ortho5.data, n=n.slices)
  }, {
    # klein bottle
    n = 4000
    samples = data.frame(theta=2*pi * runif(n), phi=2*pi * runif(n))
    kb.data = kleinb(samples$theta, samples$phi)
    kb.data = kb.data - min(kb.data)
    kb.data = kb.data / max(kb.data)
    time.hull.gen("json/klein.json", kb.data, n=n.slices)
  }
)

time.data = as.data.frame(time.data)
for(c in 1:ncol(time.data)) {
  time.data[,c] = unlist(time.data[,c])
}

write.csv(time.data, 'slice_timings.csv', row.names=FALSE)

