
using Test

@testset "Simplex/point intersection" begin include("TestSimplexPointIntersection.jl") end
@testset "Additional focus point sampling" begin include("TestSampling.jl") end
@testset "Utilities" begin include("TestUtil.jl") end
