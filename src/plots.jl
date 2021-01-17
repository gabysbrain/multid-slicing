#using Compose
#using ColorTypes
using Plots

include("types.jl")

# Sliceplorer plots
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

# function sliceplorer_plot(f, spec, n=500, ranges=spec)
  # samps = sliceplorer_samps(f, spec, n, ranges)

  # # figure out the min/max y values for the scale
  # y_vals = [x[2] for x = hcat(hcat(values(samps)...)...)]
  # y_rng = (minimum(y_vals), maximum(y_vals))

  # plots = [plot_dim(ranges[d][1], samps[d], y_rng) for d = keys(samps)]
  # plot(plots..., layout=(length(plots), 1))
# end

function plot_sp(data :: Sliceplorer)
  y_vals = [x[2] for x = hcat(hcat(hcat(values(data)...)...)...)]
  y_rng = (minimum(y_vals), maximum(y_vals))

  plots = [plot_dim(d, data[d], y_rng) for d = keys(data)]
  plot(plots..., layout=(length(plots), 1))
end

