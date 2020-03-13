
## Types

struct ProblemSpec1D
  ranges::Array{Tuple{String,Float64,Float64}}
  f::Function
end

## Polynomial definitions

# The 2 functions from Farzad
function poly1(x, y, z, w)
  -32 * w^3 * x * z^3
    + 44 * w^5 * x * z^3
    - 16 * w^7 * x * z^3
    + 4 * w^9 * x * z^3
    + 16 * w^3 * y * z^3
    - 100 * w^5 * y * z^3
    + 128 * w^7 * y * z^3
    - 44 * w^9 * y * z^3
    - 56 * w^5 * x * y * z^3
    + 48 * w^7 * x * y * z^3
    - 16 * w^9 * x * y * z^3
    - 56 * w^5 * y^2 * z^3
    + 48 * w^7 * y^2 * z^3
    - 16 * w^9 * y^2 * z^3
    + 32 * w^3 * x * z^5
    + 16 * w^7 * x * z^5
    - 64 * w^9 * x * z^5
    + 16 * w^11 * x * z^5
    + 16 * w^3 * x^2 * z^5
    - 64 * w^5 * x^2 * z^5
    + 96 * w^7 * x^2 * z^5
    - 64 * w^9 * x^2 * z^5
    + 16 * w^11 * x^2 * z^5
    - 16 * w^3 * y * z^5
    + 144 * w^5 * y * z^5
    - 128 * w^7 * y * z^5
    - 16 * w^9 * y * z^5
    + 16 * w^11 * y * z^5
    + 16 * w^3 * x * y * z^5
    + 48 * w^5 * x * y * z^5
    - 16 * w^7 * x * y * z^5
    + 80 * w^9 * x * y * z^5
    - 32 * w^11 * x * y * z^5
    + 112 * w^5 * y^2 * z^5
    - 112 * w^7 * y^2 * z^5
    + 144 * w^9 * y^2 * z^5
    - 48 * w^11 * y^2 * z^5
    - 44 * w^5 * x * z^7
    - 16 * w^7 * x * z^7
    + 80 * w^11 * x * z^7
    - 20 * w^13 * x * z^7
    + 8 * w^5 * x^2 * z^7
    - 32 * w^7 * x^2 * z^7
    + 48 * w^9 * x^2 * z^7
    - 32 * w^11 * x^2 * z^7
    + 8 * w^13 * x^2 * z^7
    - 44 * w^5 * y * z^7
    - 64 * w^7 * y * z^7
    + 144 * w^9 * y * z^7
    - 64 * w^11 * y * z^7
    + 28 * w^13 * y * z^7
    - 48 * w^5 * x * y * z^7
    + 48 * w^7 * x * y * z^7
    - 192 * w^9 * x * y * z^7
    + 48 * w^11 * x * y * z^7
    - 56 * w^5 * y^2 * z^7
    + 80 * w^7 * y^2 * z^7
    - 240 * w^9 * y^2 * z^7
    + 80 * w^11 * y^2 * z^7
    - 8 * w^13 * y^2 * z^7
    + 16 * w^7 * x * z^9
    + 64 * w^9 * x * z^9
    - 80 * w^11 * x * z^9
    + 64 * w^7 * y * z^9
    - 80 * w^9 * y * z^9
    + 64 * w^11 * y * z^9
    - 48 * w^13 * y * z^9
    - 16 * w^7 * x * y * z^9
    + 112 * w^9 * x * y * z^9
    - 16 * w^11 * x * y * z^9
    + 16 * w^13 * x * y * z^9
    - 16 * w^7 * y^2 * z^9
    + 112 * w^9 * y^2 * z^9
    - 16 * w^11 * y^2 * z^9
    + 16 * w^13 * y^2 * z^9
    - 4 * w^9 * x * z^11
    - 16 * w^11 * x * z^11
    + 20 * w^13 * x * z^11
    - 4 * w^9 * y * z^11
    - 16 * w^11 * y * z^11
    + 20 * w^13 * y * z^11
    - 16 * w^11 * x * y * z^11
    - 8 * w^13 * x * y * z^11
    - 16 * w^11 * y^2 * z^11
    - 8 * w^13 * y^2 * z^11
end

function poly2(x, y, z, w)
  20 * w * x * z^3
    - 48 * w^3 * x * z^3
    + 28 * w^5 * x * z^3
    + 8 * w * x^2 * z^3
    - 16 * w^3 * x^2 * z^3
    + 8 * w^5 * x^2 * z^3
    + 20 * w * y * z^3
    - 20 * w^5 * y * z^3
    + 8 * w * x * y * z^3
    - 16 * w^3 * x * y * z^3
    - 8 * w^5 * y^2 * z^3
    - 16 * w * x * z^5
    + 64 * w^3 * x * z^5
    - 64 * w^5 * x * z^5
    + 16 * w^7 * x * z^5
    + 16 * w * x^2 * z^5
    + 16 * w^3 * x^2 * z^5
    - 80 * w^5 * x^2 * z^5
    + 48 * w^7 * x^2 * z^5
    - 16 * w * y * z^5
    - 80 * w^3 * y * z^5
    + 80 * w^5 * y * z^5
    + 16 * w^7 * y * z^5
    + 16 * w * x * y * z^5
    + 16 * w^3 * x * y * z^5
    - 48 * w^5 * x * y * z^5
    + 32 * w^7 * x * y * z^5
    + 32 * w^5 * y^2 * z^5
    - 16 * w^7 * y^2 * z^5
    - 4 * w * x * z^7
    - 80 * w^3 * x * z^7
    + 144 * w^5 * x * z^7
    - 16 * w^7 * x * z^7
    - 44 * w^9 * x * z^7
    - 112 * w^3 * x^2 * z^7
    + 240 * w^5 * x^2 * z^7
    - 144 * w^7 * x^2 * z^7
    + 16 * w^9 * x^2 * z^7
    - 4 * w * y * z^7
    + 64 * w^3 * y * z^7
    - 64 * w^7 * y * z^7
    + 4 * w^9 * y * z^7
    - 112 * w^3 * x * y * z^7
    + 192 * w^5 * x * y * z^7
    - 80 * w^7 * x * y * z^7
    + 16 * w^9 * x * y * z^7
    - 48 * w^5 * y^2 * z^7
    + 64 * w^7 * y^2 * z^7
    + 64 * w^3 * x * z^9
    - 64 * w^5 * x * z^9
    - 128 * w^7 * x * z^9
    + 128 * w^9 * x * z^9
    + 16 * w^3 * x^2 * z^9
    - 80 * w^5 * x^2 * z^9
    + 112 * w^7 * x^2 * z^9
    - 48 * w^9 * x^2 * z^9
    + 16 * w^3 * y * z^9
    - 16 * w^5 * y * z^9
    + 16 * w^7 * y * z^9
    - 16 * w^9 * y * z^9
    + 16 * w^3 * x * y * z^9
    - 48 * w^5 * x * y * z^9
    + 16 * w^7 * x * y * z^9
    - 48 * w^9 * x * y * z^9
    + 32 * w^5 * y^2 * z^9
    - 96 * w^7 * y^2 * z^9
    - 44 * w^5 * x * z^11
    + 144 * w^7 * x * z^11
    - 100 * w^9 * x * z^11
    + 56 * w^5 * x^2 * z^11
    - 112 * w^7 * x^2 * z^11
    + 56 * w^9 * x^2 * z^11
    - 44 * w^5 * y * z^11
    + 44 * w^9 * y * z^11
    + 48 * w^5 * x * y * z^11
    - 48 * w^7 * x * y * z^11
    + 56 * w^9 * x * y * z^11
    - 8 * w^5 * y^2 * z^11
    + 64 * w^7 * y^2 * z^11
    - 16 * w^7 * x * z^13
    + 16 * w^9 * x * z^13
    + 32 * w^7 * y * z^13
    - 32 * w^9 * y * z^13
    - 16 * w^7 * x * y * z^13
    - 16 * w^7 * y^2 * z^13
end

function poly3(x)
  sum(x .* x)
end

poly1def = ProblemSpec1D(
  [("x", -1., 1.), ("y", -1., 1.), ("z", -1., 1.), ("w", -1., 1.)],
  x -> poly1(x[1], x[2], x[3], x[4]))

poly4def = ProblemSpec1D(
  [("x", -1., 1.), ("y", -1., 1.)],
  x -> poly1(x[1], x[2], 2000., 1000.))


poly2def = ProblemSpec1D(
  [("x", -1., 1.), ("y", -1., 1.), ("z", -1., 1.), ("w", -1., 1.)],
  x -> poly2(x[1], x[2], x[3], x[4]))

poly3def = ProblemSpec1D(
  [("x", -1., 1.), ("y", -1., 1.), ("z", -1., 1.), ("w", -1., 1.)],
  poly3)

## Sliceplorer definition stuff
spec = poly3def
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
  samples = range(rng[2], stop=rng[3], length=51) # 50 samples for now
  [(x, f1d(x)) for x = samples]
end

function sample_dim(f, rng, fps, d)
  [func_samples(f, rng, fp, d) for fp = fps]
end

function sliceplorer_samps(spec, n=500, ranges=spec.ranges)
  # Create focus points for slicing
  dmins = [x[2] for x in ranges]
  dmaxs = [x[3] for x in ranges]
  seq = SobolSeq(dmins, dmaxs)
  focuspoints = hcat([next!(seq) for i = 1:n])

  [sample_dim(spec.f, ranges[d], focuspoints, d) for d=eachindex(ranges)]
end

# Now we visualize the result
using Compose
using ColorTypes
using Plots

function plot_dim(dimname, curve_samps, y_rng)
  xs = [v[1] for v = curve_samps[1]]
  ys = [[vv[2] for vv = v] for v = curve_samps]
  plot(xs, hcat(ys...),
       #linealpha=0.6, linecolor=:black, linewidth=0.5,
       linealpha=1.0, linecolor=:black, linewidth=0.5,
       legend=false,
       xlabel=dimname)
       #ylims=y_rng, xlabel=dimname)
end

function sliceplorer_plot(spec, n=500, ranges=spec.ranges)
  samps = sliceplorer_samps(spec, n, ranges)

  # figure out the min/max y values for the scale
  y_vals = [x[2] for x = hcat(hcat(values(samps)...)...)]
  y_rng = (minimum(y_vals), maximum(y_vals))

  plots = [plot_dim(ranges[d][1], samps[d], y_rng) for d = keys(samps)]
  plot(plots..., layout=(length(plots), 1))
end

## The GUI stuff

using Interact

function plot_ui(spec)
  # The main plot
  plt = Observable(sliceplorer_plot(spec, 500))

  # Sliders for each dimension
  sliders = [dim_slider(r[1], r[2], r[3]) for r = spec.ranges]
  # turn sliders into an observable dict
  oo = map((x...) -> collect(x), sliders[1], sliders[2:length(sliders)]...)
  sv = map((vs, ns) -> [(x[1],x[2][1],x[2][2]) for x = zip(ns,vs)], oo, [x[1] for x=spec.ranges])
  #sv = map((v, n) -> Dict(zip(n,v)), oo, [x[1] for x=spec.ranges])

  # Slider for slice count
  ns = slider(1:1000, 500)
  nslider = Widget{:nslider}(OrderedDict(:label => "Slices", :widget => ns), output=observe(ns))
  @layout! nslider hbox(:label, :widget, observe(_))

  # Button to trigger the updates
  update = button("Update")

  # Update the plot when the button is clicked
  map!(t -> sliceplorer_plot(spec, nslider[], sv[]), plt, update)

  # The final layout of all the widgets
  vbox(plt, Interact.hline(), hbox(sliders...), nslider, update)
end

function dim_slider(dn, xmn, xmx)
  #d = OrderedDict(:label => dn, :slider => rangepicker(range(xmn, stop=xmx, length=101), readout=false))
  d = OrderedDict(:label => dn, :lb => widget(-1.), :ub => widget(1.))
  o = Observable((-1., 1.))
  map!((mn,mx) -> (mn, mx), o, d[:lb], d[:ub])
  w = Widget{:dimslider}(d, output=observe(o))
  @layout! w hbox(:label, :lb, :ub)
end
