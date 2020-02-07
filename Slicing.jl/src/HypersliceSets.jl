module HypersliceSets
export HypersliceSegment, HypersliceSet, savejson, readjson

include("types.jl")

using JSON

struct HypersliceSegment
  fp   :: PointND
  simplex :: Simplex
  d1   :: Dim
  d2   :: Dim
  p1d1 :: Float64
  p1d2 :: Float64
  p2d1 :: Float64
  p2d2 :: Float64
end

function Base.isapprox(x::HypersliceSegment, y::HypersliceSegment)
  all([
    x.fp ≈ y.fp,
    x.simplex ≈ y.simplex,
    x.d1 == y.d1,
    x.d2 == y.d2,
    x.p1d1 ≈ y.p1d1,
    x.p1d2 ≈ y.p1d2,
    x.p2d1 ≈ y.p2d1,
    x.p2d2 ≈ y.p2d2
  ])
end

function JSON.lower(hs::HypersliceSegment)
  Dict("focusPoint" => hs.fp,
       "simplex" => hs.simplex,
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
  slices = [HypersliceSegment(s["focusPoint"], s["simplex"], s["d1"], s["d2"], s["p1d1"], s["p1d2"], s["p2d1"], s["p2d2"]) for s in j["slices"]]
  HypersliceSet(ps, slices)
end

end
