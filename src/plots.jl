#using Compose
#using ColorTypes
using VegaLite
using DataFrames

include("types.jl")

# Converts sliceplorer data to the DataFrame needed for vegalite
function vl_data(data::Sliceplorer)
  output = nothing
  for (dimname,vals) in data
    dfs = [ DataFrame(dim=dimname, 
                      fpid=i, 
                      x=[vv[1] for vv in v],
                      y=[vv[2] for vv in v])
           for (i,v) in enumerate(vals) ]
    df = reduce(vcat, dfs)
    if output === nothing
      output = df
    else
      output = vcat(output, df)
    end
  end
  output
end

function plot(data::Sliceplorer)
  # Vegalite needs something like a dataframe
  df = vl_data(data);
  df |>
  @vlplot(
    mark = {:line, opacity=0.4, stroke="black"},
    row=:dim, # facet by dimension
    y=:y,
    x=:x,
    detail=:fpid
  )
end

