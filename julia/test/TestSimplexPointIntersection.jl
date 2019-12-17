using Test

push!(LOAD_PATH, "src")

using IntersectTest

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
  res = IntersectTest.simplexPointIntersection(planarTriangle, [1.0, 1.0, 1.0], 1, 2)

  resExp = [
    IntersectTest.Intersect2D(1.0, 1.0, 3.0, 1.0),
    IntersectTest.Intersect2D(1.0, 1.0, 2.0, 2.0),
    IntersectTest.Intersect2D(3.0, 1.0, 2.0, 2.0)
  ]

  @test res == resExp
end

@testset "3d-embedded triangle in plane (non-intersection)" begin
  res = IntersectTest.simplexPointIntersection(planarTriangle, [1.0, 1.0, 0.0], 1, 2)

  resExp = []
  #expect_that(nrow(res), equals(0))
  @test res == resExp
end

@testset "off-axis triangle - dims 1,2 (intersection)" begin
  res = IntersectTest.simplexPointIntersection(offAxisTriangle2, [0.5, 0.5, 0.8], 1, 2)
  resExp = IntersectTest.intersectTri(offAxisTriangle2, [0.5, 0.5, 0.8], 1, 2)

  @test length(res) == 1
  @test res[1] ≈ resExp[1]
end

@testset "off-axis triangle - dims 1,3 (intersection)" begin
  res = IntersectTest.simplexPointIntersection(offAxisTriangle2, [0.5, 0.5, 0.8], 1, 3)
  resExp = IntersectTest.intersectTri(offAxisTriangle2, [0.5, 0.5, 0.8], 1, 3)
  #print(res)

  @test length(res) == 1
  @test res[1] ≈ resExp[1]
end

@testset "point inside simplex" begin
  res = IntersectTest.simplexPointIntersection(offAxisTriangle2, [0.5, 0.5, 0.5], 1, 3)
  resExp = IntersectTest.intersectTri(offAxisTriangle2, [0.5, 0.5, 0.5], 1, 3)

  @test length(res) == 1
  @test res[1] ≈ resExp[1]
end
