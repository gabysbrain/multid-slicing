using Test

include("../src/types.jl")

using MdSlicing

@testset "random values are within problem spec range" begin
  ps = ProblemSpec("x1" => (-2., 2.), "x2" => (0., 1.), "x3" => (25., 50.))

  fps = Slicing.fillfps(ps, [missing, 0., 0.], 500)
  @test all([x[1] >= ps.vals[1][1] for x in fps])
  @test all([x[1] <= ps.vals[1][2] for x in fps])

  fps = Slicing.fillfps(ps, [0., missing, 0.], 500)
  @test all([x[2] >= ps.vals[2][1] for x in fps])
  @test all([x[2] <= ps.vals[2][2] for x in fps])

  fps = Slicing.fillfps(ps, [0., 0., missing], 500)
  @test all([x[3] >= ps.vals[3][1] for x in fps])
  @test all([x[3] <= ps.vals[3][2] for x in fps])
end

@testset "properly handles fully-specified fp" begin
  ps = ProblemSpec("x1" => (-2., 2.), "x2" => (0., 1.), "x3" => (25., 50.))

  fps = Slicing.fillfps(ps, [0., 0., 0.], 50)
  @test length(fps) == 1
end
