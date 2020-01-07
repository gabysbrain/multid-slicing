module HypersliceSet
export savejson, readjson

@include("types.jl")

using JSON

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

end
