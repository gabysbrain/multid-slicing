
library(jug)
library(magrittr)

source('server/pareto.R')

jug() %>%
  post(path="/pareto", function(req, res, err) {
    #print(req$body)
    # TODO: error handling
    raw.data = req$body
    print(raw.data)
    dconn = textConnection(raw.data)
    data = read.csv(dconn)
    close(dconn)
    print(data)
    
    #data = d2
    pareto.pts = pareto.points(data)
    edges = delaunay.edges(pareto.pts)
    json.data = list(
      paretoPoints = pareto.pts,
      simplexEdges = edges
    )
    
    # send back the result
    res$json(json.data)
  }) %>%
  simple_error_handler() %>%
  serve_it()

