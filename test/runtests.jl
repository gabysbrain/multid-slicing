
using Test

@testset "Hyperslice solver" begin include("TestSolver.jl") end
@testset "Simplex/point intersection" begin include("TestSimplexPointIntersection.jl") end
@testset "Additional focus point sampling" begin include("TestSampling.jl") end
@testset "Utilities" begin include("TestUtil.jl") end
@testset "Top-level hyperslice function" begin include("TestHyperslice.jl") end
