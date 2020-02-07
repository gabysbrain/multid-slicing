module Hyperslice
export hyperslice

include("types.jl")
include("util.jl")
include("IntersectTest.jl")

using ..Meshes
using ..HypersliceSets
using Sobol: SobolSeq, next!
using JSON

function hyperslice(mesh::ConvexMesh, n::UInt32=UInt32(50))
  dims = size(mesh)[2]

  # Generate the focus points
  lbs = [b[1] for b=mesh.problemSpec.vals]
  ubs = [b[2] for b=mesh.problemSpec.vals]
  s = SobolSeq(lbs, ubs)
  fps = collect([next!(s) for _ = 1:n])

  hyperslice(mesh, fps)
end

function hyperslice(mesh::ConvexMesh, fps::Array{PointND})
  slices = []
  for fp in fps
    ss = sliceDims(mesh, fp)
    append!(slices, ss)
  end
  HypersliceSet(mesh.problemSpec, slices)
end

# slices of all dimensions
function sliceDims(mesh::ConvexMesh, fp::PointND)
  slices = []
  for dd in dimComb(UInt64(size(mesh)[2]))
    ss = slice(mesh, fp, dd[1], dd[2])
    hss = [HypersliceSegment(fp, simp, dd[1], dd[2], s.p1d1, s.p1d2, s.p2d1, s.p2d2) for (simp,s) in ss]
    append!(slices, hss)
  end
  return slices
end

function slice(mesh::ConvexMesh, fp::PointND, d1::Dim, d2::Dim)
  slices = Tuple{Simplex,Intersect2D}[]
  for s in mesh
    ts = [(s,t) for t in simplexPointIntersection(s, fp, d1, d2)]
    append!(slices, ts)
  end
  return slices
end

end
