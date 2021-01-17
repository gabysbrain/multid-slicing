module Sliceplorer
export sliceplorer

include("types.jl")

## Sliceplorer definition stuff
using Sobol

function func1d(f, pt, d)
  function(x)
    xx = copy(pt)
    xx[d] = x
    f(xx)
  end
end

function func_samples(f, rng, pt, d)
  f1d = func1d(f, pt, d)
  samples = range(rng[1], stop=rng[2], length=51) # 50 samples for now
  [(x, f1d(x)) for x = samples]
end

function sample_dim(f, rng, fps, d)
  [func_samples(f, rng, fp, d) for fp = fps]
end

# TODO: add type signature
function sliceplorer(f, spec, n=50)
  # Create focus points for slicing
  dmins = [x[1] for (_,x) in spec]
  dmaxs = [x[2] for (_,x) in spec]
  seq = SobolSeq(dmins, dmaxs)
  focuspoints = hcat([next!(seq) for i = 1:n])

  output = Sliceplorer()
  for (i,d) in enumerate(keys(spec))
    output[d] = sample_dim(f, spec[d], focuspoints, i)
  end
  output
end

end # module

