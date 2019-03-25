context("test-hyperslice.R")

test.mesh.1 = list(
  problemSpec = list(
    dimNames = c("x1", "x2", "x3"),
    limits = list(x1=c(0,1), x2=c(0,1), x3=c(0,1))
  ),
  points = data.frame(x1=c(0.0, 0.0, 1.0), x2=c(0.0, 1.0, 0.0), x3=c(0.0, 0.0, 1.0)),
  simplices = rbind(c(1, 2, 3))
)

test.cube.3d = convmesh(data.frame(x1=c(0, 0, 0, 0, 1, 1, 1, 1),
                                   x2=c(0, 0, 1, 1, 0, 0, 1, 1),
                                   x3=c(0, 1, 0, 1, 0, 1, 0, 1)),
                        nice=TRUE)

test_that("function checks for focus points", {
  expect_error(hyperslice(test.mesh.1), "either n or focus.points must be specified", fixed=TRUE)
})

test_that("integer check for n", {
  expect_error(hyperslice(test.mesh.1, n="bc"), "n must be an integer", fixed=TRUE)
})

test_that("single slice of a single triangle", {
  res = hyperslice(test.mesh.1, focus.points=c(0.5, 0.5, 0.8))
  #print(res)
  exp.slices.1 = list(
    focusPoints = data.frame(x1=0.5, x2=0.5, x3=0.8),
    slices = data.frame(
      d1Min=c(0.8, 0.0, 0.0),
      d1Max=c(0.8, 0.5, 0.5),
      d2Min=c(0.0, 0.0, 0.5),
      d2Max=c(0.2, 0.5, 0.5),
      d1=c(1, 1, 2),
      d2=c(2, 3, 3),
      fpid=c(1, 1, 1)
    )
  )
  class(exp.slices.1) = "SliceSet"
  expect_equal(res, exp.slices.1)
})

test_that("hyperslice correctly combines intersect.simplices", {
  r.hs = hyperslice(test.cube.3d, focus.points=rep(0.5, 3))

  is.1x2 = intersect.simplices(test.cube.3d, rep(0.5, 3), 1, 2) %>% dplyr::filter(!is.na(d1Min))
  is.1x3 = intersect.simplices(test.cube.3d, rep(0.5, 3), 1, 3) %>% dplyr::filter(!is.na(d1Min))
  is.2x3 = intersect.simplices(test.cube.3d, rep(0.5, 3), 2, 3) %>% dplyr::filter(!is.na(d1Min))

  expect_equal(r.hs$slices %>% dplyr::filter(d1==1, d2==2) %>% dplyr::select(d1Min, d1Max, d2Min, d2Max), is.1x2)
  expect_equal(r.hs$slices %>% dplyr::filter(d1==1, d2==3) %>% dplyr::select(d1Min, d1Max, d2Min, d2Max), is.1x3)
  expect_equal(r.hs$slices %>% dplyr::filter(d1==2, d2==3) %>% dplyr::select(d1Min, d1Max, d2Min, d2Max), is.2x3)
})
