module Hyperslice
export hyperslice

include("types.jl")
include("util.jl")
include("IntersectTest.jl")

using ..Mesh
using Sobol: SobolSeq, next!
using JSON

struct HypersliceSegment
  fp   :: PointND
  d1   :: Dim
  d2   :: Dim
  p1d1 :: Float64
  p1d2 :: Float64
  p2d1 :: Float64
  p2d2 :: Float64
end

function JSON.lower(hs::HypersliceSegment)
  Dict("focusPoint" => hs.fp,
       "d1" => hs.d1,
       "d2" => hs.d2,
       "p1d1" => hs.p1d1,
       "p1d2" => hs.p1d2,
       "p2d1" => hs.p2d1,
       "p2d2" => hs.p2d2
  )
end

struct HypersliceSet
  problemSpec :: ProblemSpec
  slices :: Array{HypersliceSegment}
end

function savejson(filename::String, hs::HypersliceSet)
  open(filename, "w") do io
    JSON.print(io, hs)
  end
end

function readjson(filename::String)
  j = JSON.parsefile(filename, dicttype=OrderedDict)
  ps = ProblemSpec(x[1] => Tuple(x[2]) for x in j["problemSpec"])
  slices = [HypersliceSegment(s["focusPoint"], s["d1"], s["d2"], s["p1d1"], s["p1d2"], s["p2d1"], s["p2d2"]) for s in j["slices"]]
  HypersliceSet(ps, slices)
end

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
    hss = [HypersliceSegment(fp, dd[1], dd[2], s.p1d1, s.p1d2, s.p2d1, s.p2d2) for s in ss]
    append!(slices, hss)
  end
  return slices
end

function slice(mesh::ConvexMesh, fp::PointND, d1::Dim, d2::Dim)
  slices = []
  for s in mesh
    append!(slices, simplexPointIntersection(s, fp, d1, d2))
  end
  return slices
end

end
