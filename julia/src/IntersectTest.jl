module IntersectTest
export simplexPointIntersection, intersectTri

using LinearAlgebra

struct Intersect2D
  p1d1 :: Float64
  p1d2 :: Float64
  p2d1 :: Float64
  p2d2 :: Float64
end

PointND = Array{Float64,1}

function simplexPointIntersection(simplex, fp::PointND, d1::Int, d2::Int)
end

# TODO: force triangle to be a triangle in 3D space
function intersectTri(triangle, fp::PointND, d1::Int, d2::Int)
  # compute the normal to the slicing plane
  n = ones(3)
  n[d1] = n[d2] = 0.0

  # figure out the point projection
  i1 = dot(n, (fp - triangle[1,:])) / dot(n, (triangle[2,:] - triangle[1,:]))
  i2 = dot(n, (fp - triangle[1,:])) / dot(n, (triangle[3,:] - triangle[1,:]))
  i3 = dot(n, (fp - triangle[2,:])) / dot(n, (triangle[3,:] - triangle[2,:]))
  p1 = triangle[1,:] + i1 * (triangle[2,:] - triangle[1,:])
  p2 = triangle[1,:] + i2 * (triangle[3,:] - triangle[1,:])
  p3 = triangle[2,:] + i3 * (triangle[3,:] - triangle[2,:])

  pts = filter(x -> isfinite(x[1]), [p1, p2, p3])
  if length(pts) == 0
    return nothing
  else
    return Intersect2D(pts[1][d1], pts[1][d2], pts[2][d1], pts[2][d2])
  end
end

end
