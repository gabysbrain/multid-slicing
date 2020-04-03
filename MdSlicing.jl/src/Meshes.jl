module Meshes
export ConvexMesh, savehdf5, readhdf5

include("types.jl")

using HDF5

struct ConvexMesh
  problemSpec :: ProblemSpec
  points :: Array{Float64}
  simplIdx :: Array{UInt64}
end

# Loop through simplices
function Base.iterate(m :: ConvexMesh, state=1)
  if state > size(m)[1]
    return nothing
  end

  i = m.simplIdx[state,:]
  (m.points[i,:], state+1)
end

# Length of the mesh is the number of simplices
# Needed for many iteration utilities
Base.length(m :: ConvexMesh) = Base.size(m)[1]

# Size of the mesh is the number of simplices and dimensions
function Base.size(m :: ConvexMesh)
  (size(m.simplIdx)[1], size(m.points)[2])
end

function savehdf5(filename::String, m::ConvexMesh)
  dimnames = keys(m.problemSpec)
  dimranges = values(m.problemSpec)

  h5open(filename, "w") do file
    write(file, "dimnames", dimnames)
    write(file, "support", dimranges)
    write(file, "points", m.points)
    write(file, "simplices", m.simplIdx)
  end
end

function readhdf5(filename::String)
  h5open(filename, "r") do file
    dimnames = read(file, "dimnames")
    support = read(file, "support")
    points = read(file, "points")
    simplIdxs = read(file, "simplices")

    ps = ProblemSpec(x[1] => Tuple(x[2]) for x in zip(dimnames, support))
    ConvexMesh(ps, points, simplIdxs)
  end
end

end
