module Hyperslice

include("types.jl")
include("util.jl")

using Sobol: SobolSeq, next!

function hyperslice(mesh::Mesh, n::UInt32=UInt32(50))
  dims = size(mesh)[2]

  # Generate the focus points
  lbs = [b[1] for b=mesh.problemSpec.vals]
  ubs = [b[2] for b=mesh.problemSpec.vals]
  s = SobolSeq(lbs, ubs)
  fps = collect([next!(s) for _ = 1:n])

  hyperslice(mesh, fps)
end

function hyperslice(mesh::Mesh, fps::Array{PointND})
  slices = []
  for fp in fps
    ss = sliceDims(mesh, fp)
    append!(slices, ss)
  end
  HypersliceSet(mesh.problemSpec, slices)
end

# slices of all dimensions
function sliceDims(mesh::Mesh, fp::PointND)
  slices = []
  for dd in dimComb(UInt64(size(mesh)[2]))
    ss = slice(mesh, fp, dd[1], dd[2])
    hss = [HypersliceSegment(fp, dd[1], dd[2], s.p1d1, s.p1d2, s.p2d1, s.p2d2) for s in ss]
    append!(slices, hss)
  end
  return slices
end

function slice(mesh::Mesh, fp::PointND, d1::Dim, d2::Dim)
  # FIXME: need a flatmap
  slices = []
  for s in mesh
    append!(slices, IntersectTest.simplexPointIntersection(s, fp, d1, d2))
  end
  return slices
end

end
