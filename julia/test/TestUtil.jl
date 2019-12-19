using Test

push!(LOAD_PATH, "src")

include("../src/util.jl")

@testset "3D test" begin
   res = dimComb(Dim(3))

   @test res == Set([(1,2),(1,3), (2,3)])
end

@testset "5D test" begin
   res = dimComb(Dim(5))

   resExp = Set([(1,2), (1,3), (1,4), (1,5), (2,3), (2,4), (2,5), (3, 4), (3, 5), (4, 5)])
   @test setdiff(res, resExp) == Set()
   @test res == resExp
end
