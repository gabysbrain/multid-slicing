using Test

include("../src/IntersectTest.jl")

include("../src/types.jl")

planarTriangle = [
  1.0 1.0 1.0 ;
  3.0 1.0 1.0 ;
  2.0 2.0 1.0
]

offAxisTriangle = [
  0.14904467 0.9743324 0.16870685 ;
  0.98993366 0.1410158 0.01207846 ;
  0.01350696 0.3065440 0.95176064
]

offAxisTriangle2 = [
  0.0 0.0 0.0 ;
  0.0 1.0 0.0 ;
  1.0 0.0 1.0
]

@testset "3d-embedded triangle in plane (intersection)" begin
  res = simplexPointIntersection(planarTriangle, [1.0, 1.0, 1.0], Dim(1), Dim(2))

  resExp = [
    Intersect2D(1.0, 1.0, 3.0, 1.0),
    Intersect2D(1.0, 1.0, 2.0, 2.0),
    Intersect2D(3.0, 1.0, 2.0, 2.0)
  ]

  @test res == resExp
end

@testset "3d-embedded triangle in plane (non-intersection)" begin
  res = simplexPointIntersection(planarTriangle, [1.0, 1.0, 0.0], Dim(1), Dim(2))

  resExp = []
  #expect_that(nrow(res), equals(0))
  @test res == resExp
end

@testset "off-axis triangle - dims 1,2 (intersection)" begin
  res = simplexPointIntersection(offAxisTriangle2, [0.5, 0.5, 0.8], Dim(1), Dim(2))
  resExp = intersectTri(offAxisTriangle2, [0.5, 0.5, 0.8], Dim(1), Dim(2))

  @test length(res) == 1
  # FIXME: need insstance for rtoldefault with Intersect2D
  @test_broken res[1] ≈ resExp[1]
end

@testset "off-axis triangle - dims 1,3 (intersection)" begin
  res = simplexPointIntersection(offAxisTriangle2, [0.5, 0.5, 0.8], Dim(1), Dim(3))
  resExp = intersectTri(offAxisTriangle2, [0.5, 0.5, 0.8], Dim(1), Dim(3))
  #print(res)

  @test length(res) == 1
  # FIXME: need insstance for rtoldefault with Intersect2D
  @test_broken res[1] ≈ resExp[1]
end

@testset "point inside simplex" begin
  res = simplexPointIntersection(offAxisTriangle2, [0.5, 0.5, 0.5], Dim(1), Dim(3))
  resExp = intersectTri(offAxisTriangle2, [0.5, 0.5, 0.5], Dim(1), Dim(3))

  @test length(res) == 1
  # FIXME: need insstance for rtoldefault with Intersect2D
  @test_broken res[1] ≈ resExp[1]
end
