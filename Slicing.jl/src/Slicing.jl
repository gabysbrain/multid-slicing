module Slicing

include("types.jl")
include("Hyperslice.jl")

function fillfps(ps::ProblemSpec, fp::Array{Union{Missing,Float64},1}, n::Int = 50)
  fpvals =
    [ismissing(x[1]) ? (x[2][1] .+ (x[2][2]-x[2][1]) .* rand(n)) : repeat([x[1]], n) for x in zip(fp, ps.vals)]
  # transpose everything
  [[fpvals[c][r] for c=1:length(fp)] for r = 1:n]
end

end # module
