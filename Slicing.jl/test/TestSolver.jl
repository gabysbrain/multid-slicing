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
