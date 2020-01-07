module Mesh

include("types.jl")

using HDF5

function savehdf5(filename::String, m::Mesh)
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
    Mesh(ps, points, simplIdxs)
  end
end

end
