source('server/pareto.R')
source('server/write_hull_json.R')

# 3d sphere pareto
write.hull.json("json/sphere_3d.json", sphere.3d.p)

# cube
cube.data = expand.grid(x1=c(0,0.5), x2=c(0,0.5), x3=c(0, 0.5))
write.hull.json("json/cube.json", cube.data)

# 4D hypercube
hcube.data = expand.grid(x1=c(0,0.5), x2=c(0,0.5), x3=c(0, 0.5), x4=c(0,0.5))
write.hull.json("json/4d_cube.json", hcube.data)
