context("test-intersect-simplices cube")

test.cube.3d = convmesh(expand.grid(x1=c(0,1), x2=c(0,1), x3=c(0,1)),
                        nice=TRUE)

test_that("4 simplices have no intersection", {
  res = intersect.simplices(test.cube.3d, rep(0.5, 3), 1, 2)

  expect_equal(nrow(res), nrow(test.cube.3d$simplices)-4) # 4 simplices not hit (top and bottom ones)
})


test_that("correct intersection points", {
  res = intersect.simplices(test.cube.3d, rep(0.5, 3), 1, 2)

  # cut NA rows
  res = dplyr::filter(res, !is.na(d1Min))
  expect_equal(nrow(res), 8) # 8 intersection segments
  res.exp = data.frame(
    d1Min = c(0.5, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.5),
    d1Max = c(1.0, 0.5, 1.0, 1.0, 0.0, 0.0, 0.5, 1.0),
    d2Min = c(0.0, 0.0, 0.0, 0.5, 0.5, 0.0, 1.0, 1.0),
    d2Max = c(0.0, 0.0, 0.5, 1.0, 1.0, 0.5, 1.0, 1.0)
  )

  expect_equal(dplyr::tbl_df(res), dplyr::tbl_df(res.exp))
})

test_that("repeated calls are deterministic", {
  exp = intersect.simplices(test.cube.3d, c(0.75,0.25,0.75), 2, 3, use.3d.intersection = FALSE) # these dims and fp were less stable
  for(i in 1:10) {
    test = intersect.simplices(test.cube.3d, c(0.75,0.25,0.75), 2, 3, use.3d.intersection = FALSE)
    expect_equal(test, exp)
  }
})
