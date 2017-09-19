
library(jsonlite)
library(stringr)

source('server/pareto.R')

# number of samples and focus points
n = 50
fps = 2

kleinb = function(theta, phi, r=2, p=2, eps=1e-9) {
  x = r * (cos(theta/2)*cos(phi) - sin(theta/2)*sin(2*phi))
  y = r * (sin(theta/2)*cos(phi) + cos(theta/2)*sin(2*phi))
  z = p * cos(theta) * (1 + eps * sin(phi))
  w = p * sin(theta) * (1 + eps * sin(phi))
  cbind(x, y, z, w)
}
samples = data.frame(theta=2*pi * runif(n), phi=2*pi * runif(n))
data = kleinb(samples$theta, samples$phi)
data = data - min(data)
data = data / max(data)
#data.p = pareto.points(data)

conv.hull = convhulln(data)
plot.data = gen.plot.data(data, conv.hull, n=fps)
plot.data = plot.data[,names(plot.data)!="X1"]

# put the plot data in a nice format for jsoning
dim.pairs = unique(plot.data[,c("d1","d2")])
fpids = unique(plot.data$fpid)

json.data = lapply(1:nrow(dim.pairs), function(i) {
  d1 = dim.pairs$d1[i]
  d2 = dim.pairs$d2[i]
  d = plot.data[plot.data$d1==d1&plot.data$d2==d2,]
  dim.data = lapply(fpids, function(fpid) {
    pd = plot.data[plot.data$d1==d1&plot.data$d2==d2&plot.data$fpid==fpid,]
    pts = unique(rbind(
      setNames(pd[,c("d1.min", "d2.min")], c("x1", "x2")),
      setNames(pd[,c("d1.max", "d2.max")], c("x1", "x2"))
    ))
    list(
      fpid = fpid,
      pts = pts
    )
  })
  list(
    d1 = d1,
    d2 = d2,
    data = dim.data
  )
})

curve.data = setNames(plot.data[,names(plot.data)!="X1"], 
                      c("x1_1", "x2_1", "x1_2", "x2_2", "d1", "d2", "fpid"))

json.data = list(
  points = setNames(data.frame(data), str_c("x", 1:4)),
  curves = curve.data
)

fname = str_c("klein_bottle_", n, "_", fps, ".json")
cat(toJSON(json.data, pretty=2), file=fname)
