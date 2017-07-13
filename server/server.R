
library(jug)
library(magrittr)

source('server/pareto.R')

jug() %>%
  cors(allow_origin="http://localhost:3000") %>%
  #cors() %>%
  post(path="/pareto", function(req, res, err) {
    # TODO: error handling
    raw.data = gsub("%0A", "\n", req$body)
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
    print("done!")

    # send back the result
    res$json(json.data)
  }) %>%
  simple_error_handler() %>%
  serve_it()

