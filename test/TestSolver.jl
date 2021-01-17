using Test

include("../src/IntersectTest.jl")

include("../src/types.jl")

@testset "address error from focus point 1.5, 1.5, 1.5 with single triangle mesh" begin

lx = [-100663297., 100663297., 100663297., -67108864.]
ly = [-1., 1., 0., 0.]
lc = [150994945., -150994946., -150994944., 100663296.]

@test ccrY0(lx, ly, lc, Dim(4)) == []
@test ccr(lx, ly, lc, Dim(4)) == []

end

@testset "address problem with first simplex for 3D sphere mesh" begin

  lx = [0.5765708843703083, 0.21828465814030382, -1.5049122768452694, 0.7100567343346572]
  ly = [-0.9714406736017811, -0.7988509382303219, 0.9398460751671794, 0.8304455366649236]
  lc = [0.0, -0.0, -0.0, 1.0]

  # FIXME: need to use approx and instance for rtoldefault
  @test_broken ccrXY(lx, ly, lc, Dim(4)) ≈ [Intersect2D(-0.8312942, -0.4933905, -0.4902460, -0.7849979)]
  @test_broken ccr(lx, ly, lc, Dim(4)) ≈ [Intersect2D(-0.8312942, -0.4933905, -0.4902460, -0.7849979)]
end
