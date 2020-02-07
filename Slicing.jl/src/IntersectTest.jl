
using LinearAlgebra
include("types.jl")

function isplanar(simplex::Simplex, fp::PointND, d1::Dim, d2::Dim)
  checkdims = [x for x in 1:length(fp) if x!=d1 && x!=d2]
  # need to check if the remaining dims are equal to the focus point
  all([all(simplex[:,d] .== fp[d]) for d in checkdims])
end

# TODO: ensure simplex and fp are compatible
function simplexPointIntersection(simplex::Simplex, fp::PointND, d1::Dim, d2::Dim)
  n = Dim(length(fp) + 1) # number of lambdas to check, dimensionality of the space + 1

  # Set up matrix for barycentric computation
  # Append a set of ones to the simplex and incorporate the focus point
  # to create a square matrix
  T = ones(Float64,(n,n)) # TODO: use square matrix type if exists
  T[1:n-1,1:n-1] = simplex'
  T[1:n-1,n] = fp

  if det(T) == 0 # extra focus point may be in the plane of the
                 # simplex or might be numerical problems
    if isplanar(simplex, fp, d1, d2)
      # if the simplex lies in the plane then just return the simplex as
      # a 2D projection of line segments
      nrow = size(simplex, 1)
      # all pairs of vertices in the simplex
      verts = filter(x -> x[1] != x[2], collect(Iterators.product(1:(nrow-1), 2:nrow)))
      v2d = [
        Intersect2D(simplex[pts[1], d1], simplex[pts[1], d2],
                    simplex[pts[2], d1], simplex[pts[2], d2])
        for pts = verts
      ]
      return v2d
    else
      T[1:n-1,n] = fp .+ (rand(n-1)*1e-9) # offset slightly to make matrix non-singluar
    end
  end

  # compute lambda as best we can (there will be 3 parts)
  rr = vcat(fp, 1.)
  rr[d1] = rr[d2] = 0
  rry = zeros(length(rr))
  rrx = zeros(length(rr))
  rrx[d1] = rry[d2] = 1.
  # We are trying to compute T^(-1) . [x-xn, y-yn,...,z-zn]
  Tlu = factorize(T)
  lc = Tlu \ rr # This way is more stable than inverting once
  lx = Tlu \ rrx
  ly = Tlu \ rry

  # find the d1 and d2 ranges that make each lambda 0
  # most indices are based on solving ax + by + c = 0
  # but keeping the other lambdas between 0 and 1

  # put y=mx+b into each other lambda formula and try and get a good range
  ccr(lx, ly, lc, n) # only consider the last lambda
end

## The common-cross-range functions
function ccr(lx::LambdaND, ly::LambdaND, lz::LambdaND, i::Dim)
  if lx[i] == 0 && ly[i] == 0
    return []
  elseif lx[i] == 0
    ccrX0(lx, ly, lz, i)
  elseif ly[i] == 0
    ccrY0(lx, ly, lz, i)
  else
    ccrXY(lx, ly, lz, i)
  end
end

function ccrX0(lx::LambdaND, ly::LambdaND, lc::LambdaND, i::Dim)
  # solve ly * y + c = 0
  y = -lc[i] / ly[i]
  # replace y in all other formulas and solve lx * x + ly * y + lc >=0 for x
  xs = lx[1:end .!= i]
  cs = ly[1:end .!= i] * y + lc[1:end .!= i]
  res = -cs ./ xs

  if any(xs .== 0) && any(cs[xs .== 0] .< 0) # no intersection
    return []
  else
    xsp = xs[map(isfinite,res)] # for filtering res later
    res = res[map(isfinite,res)]
    xrng = (maximum(res[xsp .>= 0]), minimum(res[xsp .< 0]))
    # some final bounds checking
    if xrng[2] - xrng[1] < -EPS
      return []
    end
    return [Intersect2D(xrng[1], y, xrng[2], y)]
  end
end

function ccrY0(lx::LambdaND, ly::LambdaND, lc::LambdaND, i::Dim)
  # solve ly * y + c = 0
  x = -lc[i] / lx[i]
  # replace x in all other formulas and solve lx * x + ly * y + lc >=0 for y
  ys = ly[1:end .!= i]
  cs = lx[1:end .!= i] * x + lc[1:end .!= i]
  res = -cs ./ ys

  if any(ys .== 0) && any(cs[ys .== 0] .< 0) # no intersection
    return []
  else
    ysp = ys[map(isfinite,res)] # for filtering res later
    res = res[map(isfinite,res)]
    yrng = (maximum(res[ysp .>= 0]), minimum(res[ysp .< 0]))
    # some final bounds checking
    if yrng[2] - yrng[1] < -EPS
      return []
    end
    return [Intersect2D(x, yrng[1], x, yrng[2])]
  end
end

# assumes lx[i] and ly[i] are non-zero
function ccrXY(lx::LambdaND, ly::LambdaND, lc::LambdaND, i::Dim)
  # solve lx * x + ly * y + c = 0 and substitute
  xs = lx[1:end .!= i] - ly[1:end .!= i] .* lx[i] ./ ly[i]
  cs = lc[1:end .!= i] - ly[1:end .!= i] .* lc[i] ./ ly[i]
  res = -cs ./ xs # solving for xs + cs >= 0

  if any(xs .== 0) && any(cs[xs .== 0] .< 0) # no intersection
    return []
  else
    xsp = xs[map(isfinite,res)] # for filtering res later
    res = res[map(isfinite,res)]
    xrng = (maximum(res[xsp .>= 0]), minimum(res[xsp .< 0]))
    yrng = (-lc[i] .- lx[i] .* xrng) ./ ly[i]
    # some final bounds checking
    if xrng[2] - xrng[1] < -EPS
      return []
    end
    return [Intersect2D(xrng[1], yrng[1], xrng[2], yrng[2])]
  end
end



# TODO: force triangle to be a triangle in 3D space
function intersectTri(triangle, fp::PointND, d1::Dim, d2::Dim)
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
    return []
  else
    return [Intersect2D(pts[1][d1], pts[1][d2], pts[2][d1], pts[2][d2])]
  end
end
