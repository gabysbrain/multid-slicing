using Test

include("../src/types.jl")

using MdSlicing.Meshes: ConvexMesh
using MdSlicing: hyperslice
import MdSlicing.HypersliceSets
import MdSlicing.Hyperslice

QHypersliceSeg = Tuple{Array{Float64,1}, Int64, Int64, Tuple{Float64,Float64}, Tuple{Float64,Float64}}

function Base.isapprox(x::QHypersliceSeg, y::QHypersliceSeg)
  all([
    x[1] ≈ y[1],
    x[2] ≈ y[2],
    x[3] ≈ y[3],
    x[4][1] ≈ y[4][1],
    x[4][2] ≈ y[4][2],
    x[5][1] ≈ y[5][1],
    x[5][2] ≈ y[5][2]
  ])
end

function approxin(x, y::Set)
  for e in y
    if x ≈ e return true end
  end
  return false
end

function Base.isapprox(x::Set, y::Set)
  if length(x) != length(y) return false end

  # subset
  all([approxin(e, y) for e in x])
end

function Base.isapprox(x::Tuple{Float64,Float64,Float64,Float64}, y::Tuple{Float64,Float64,Float64,Float64})
  all([
    x[1] ≈ y[1]
    x[2] ≈ y[2]
    x[3] ≈ y[3]
    x[4] ≈ y[4]
  ])
end

function simplifyseg(s::HypersliceSets.HypersliceSegment)
  (s.fp, Int64(s.d1), Int64(s.d2), (s.p1d1, s.p1d2), (s.p2d1, s.p2d2))
end

function checksegs(v :: Vector{Hyperslice.Intersect2D}, exp :: Vector{QHypersliceSeg})
  v2 = Set([(x.p1d1, x.p1d2, x.p2d1, x.p2d2) for x in v])
  e2 = Set([(y[4][1], y[4][2], y[5][1], y[5][2]) for y in exp])
  @test v2 ≈ e2
end

function checksegs(v :: HypersliceSets.HypersliceSet, exp :: Vector{QHypersliceSeg})
  cv = Set(map(simplifyseg, v.slices))
  @test cv ≈ Set(exp)
end

@testset "Type sanity checks" begin
trimesh = ConvexMesh(
  ProblemSpec("x1" => (0., 1.), "x2" => (0., 1.), "x3" => (0., 1.)),
  [ 0. 0. 0. ;
    0. 1. 0. ;
    1. 0. 1.
  ],
  [ 1 2 3 ]  )

@inferred Hyperslice.slice(trimesh, [0.5, 0.5, 0.5], Dim(1), Dim(2))

end

@testset "One triangle mesh" begin

trimesh = ConvexMesh(
  ProblemSpec("x1" => (0., 1.), "x2" => (0., 1.), "x3" => (0., 1.)),
  [ 0. 0. 0. ;
    0. 1. 0. ;
    1. 0. 1.
  ],
  [ 1 2 3 ]  )

checksegs(hyperslice(trimesh, [[1.5, 1.5, 1.5]]), QHypersliceSeg[])
# TODO: define macro to compare slices from expected slices
checksegs(hyperslice(trimesh, [[0.5, 0.5, 0.5]]),
  [ ([0.5, 0.5, 0.5], 1, 2, (0.5, 0.0), (0.5, 0.5)),
    ([0.5, 0.5, 0.5], 1, 3, (0.0, 0.0), (0.5, 0.5)),
    ([0.5, 0.5, 0.5], 2, 3, (0.0, 0.5), (0.5, 0.5)) ])
checksegs(hyperslice(trimesh, [[0.5, 0.5, 0.8]]),
  [ ([0.5, 0.5, 0.8], 1, 2, (0.8, 0.0), (0.8, 0.2)),
    ([0.5, 0.5, 0.8], 1, 3, (0.0, 0.0), (0.5, 0.5)),
    ([0.5, 0.5, 0.8], 2, 3, (0.0, 0.5), (0.5, 0.5)) ])

  checksegs(Hyperslice.slice(trimesh, [1.5, 1.5, 1.5], Dim(1), Dim(2)), QHypersliceSeg[])
  checksegs(Hyperslice.slice(trimesh, [1.5, 1.5, 1.5], Dim(1), Dim(3)), QHypersliceSeg[])
  checksegs(Hyperslice.slice(trimesh, [1.5, 1.5, 1.5], Dim(2), Dim(3)), QHypersliceSeg[])
end

@testset "3D cube" begin
cube = ConvexMesh(
  ProblemSpec("x1" => (0., 1.), "x2" => (0., 1.), "x3" => (0., 1.)),
  [ 0. 0. 0. ;
    0. 0. 1. ;
    0. 1. 0. ;
    0. 1. 1. ;
    1. 0. 0. ;
    1. 0. 1. ;
    1. 1. 0. ;
    1. 1. 1.
  ],
  [ 4  3  1 ;
    4  2  1 ;
    6  2  1 ;
    6  5  1 ;
    6  4  2 ;
    6  4  8 ;
    7  3  1 ;
    7  5  1 ;
    7  4  3 ;
    7  4  8 ;
    7  6  5 ;
    7  6  8 ]
)

checksegs(Hyperslice.slice(cube, [0.5, 0.5, 0.5], Dim(1), Dim(2)),
  [ ([0.5, 0.5, 0.5], 1, 2, (0.0, 0.0), (0.5, 0.0)),
    ([0.5, 0.5, 0.5], 1, 2, (0.5, 0.0), (1.0, 0.0)),
    ([0.5, 0.5, 0.5], 1, 2, (1.0, 0.0), (1.0, 0.5)),
    ([0.5, 0.5, 0.5], 1, 2, (1.0, 0.5), (1.0, 1.0)),
    ([0.5, 0.5, 0.5], 1, 2, (0.0, 1.0), (0.5, 1.0)),
    ([0.5, 0.5, 0.5], 1, 2, (0.5, 1.0), (1.0, 1.0)),
    ([0.5, 0.5, 0.5], 1, 2, (0.0, 0.0), (0.0, 0.5)),
    ([0.5, 0.5, 0.5], 1, 2, (0.0, 0.5), (0.0, 1.0)) ]
)
checksegs(hyperslice(cube, [[0.5, 0.5, 0.5]]),
  [ ([0.5, 0.5, 0.5], 1, 2, (0.0, 0.0), (0.5, 0.0)),
    ([0.5, 0.5, 0.5], 1, 2, (0.5, 0.0), (1.0, 0.0)),
    ([0.5, 0.5, 0.5], 1, 2, (1.0, 0.0), (1.0, 0.5)),
    ([0.5, 0.5, 0.5], 1, 2, (1.0, 0.5), (1.0, 1.0)),
    ([0.5, 0.5, 0.5], 1, 2, (0.0, 1.0), (0.5, 1.0)),
    ([0.5, 0.5, 0.5], 1, 2, (0.5, 1.0), (1.0, 1.0)),
    ([0.5, 0.5, 0.5], 1, 2, (0.0, 0.0), (0.0, 0.5)),
    ([0.5, 0.5, 0.5], 1, 2, (0.0, 0.5), (0.0, 1.0)),
    ([0.5, 0.5, 0.5], 1, 3, (0.0, 0.0), (0.5, 0.0)),
    ([0.5, 0.5, 0.5], 1, 3, (0.5, 0.0), (1.0, 0.0)),
    ([0.5, 0.5, 0.5], 1, 3, (1.0, 0.0), (1.0, 0.5)),
    ([0.5, 0.5, 0.5], 1, 3, (1.0, 0.5), (1.0, 1.0)),
    ([0.5, 0.5, 0.5], 1, 3, (0.0, 1.0), (0.5, 1.0)),
    ([0.5, 0.5, 0.5], 1, 3, (0.5, 1.0), (1.0, 1.0)),
    ([0.5, 0.5, 0.5], 1, 3, (0.0, 0.0), (0.0, 0.5)),
    ([0.5, 0.5, 0.5], 1, 3, (0.0, 0.5), (0.0, 1.0)),
    ([0.5, 0.5, 0.5], 2, 3, (0.0, 0.0), (0.5, 0.0)),
    ([0.5, 0.5, 0.5], 2, 3, (0.5, 0.0), (1.0, 0.0)),
    ([0.5, 0.5, 0.5], 2, 3, (1.0, 0.0), (1.0, 0.5)),
    ([0.5, 0.5, 0.5], 2, 3, (1.0, 0.5), (1.0, 1.0)),
    ([0.5, 0.5, 0.5], 2, 3, (0.0, 1.0), (0.5, 1.0)),
    ([0.5, 0.5, 0.5], 2, 3, (0.5, 1.0), (1.0, 1.0)),
    ([0.5, 0.5, 0.5], 2, 3, (0.0, 0.0), (0.0, 0.5)),
    ([0.5, 0.5, 0.5], 2, 3, (0.0, 0.5), (0.0, 1.0)) ]
)

end
