
function dimComb(dims::Dim)
  Set(filter(x -> x[1] < x[2], collect(Iterators.product(Dim(1):(dims-1), Dim(2):dims))))
end
