context("test-intersect-simplices cube")

test.cube.3d = convmesh(expand.grid(x1=c(0,1), x2=c(0,1), x3=c(0,1)),
                        nice=TRUE)

declared.cube = list(
  problemSpec = list(
    dimNames = c("x1", "x2", "x3"),
    limits = list(x1=c(-1,1), x2=c(-1,1), x3=c(-1, 1))
  ),
  points = data.frame(x1=c(-1, 1, -1, 1, -1, 1, -1, 1), x2=c(1, 1, -1, -1, 1, 1, -1, -1), x3=c(1, 1, 1, 1, -1, -1, -1, -1)),
  simplices = matrix(c(
    1, 2, 3,
    2, 4, 3,
    5, 6, 7,
    6, 7, 8,
    1, 3, 7,
    3, 5, 7,
    3, 4, 7,
    4, 7, 8,
    2, 4, 8,
    2, 6, 8,
    1, 2, 5,
    2, 5, 6), ncol=3, byrow=TRUE)
)
class(declared.cube) = "Mesh"

test_that("4 simplices have no intersection", {
  res = intersect.simplices(test.cube.3d, rep(0.5, 3), 1, 2)

  expect_equal(nrow(res), nrow(test.cube.3d$simplices)-4) # 4 simplices not hit (top and bottom ones)
})


test_that("correct intersection points", {
  res = intersect.simplices(test.cube.3d, rep(0.5, 3), 1, 2)

  # cut NA rows
  res = dplyr::filter(res, !is.na(p1_1))
  expect_equal(nrow(res), 8) # 8 intersection segments
  res.exp = data.frame(
    p1_1 = c(0.5, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.5),
    p2_1 = c(1.0, 0.5, 1.0, 1.0, 0.0, 0.0, 0.5, 1.0),
    p1_2 = c(0.0, 0.0, 0.0, 0.5, 0.5, 0.0, 1.0, 1.0),
    p2_2 = c(0.0, 0.0, 0.5, 1.0, 1.0, 0.5, 1.0, 1.0)
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

test_that("declared cube (from python port testing) matches with 3d intersection", {
  res = intersect.simplices(declared.cube, c(0, 0, 0), 1, 2, use.3d.intersection = FALSE)
  res.exp = intersect.simplices(declared.cube, c(0, 0, 0), 1, 2, use.3d.intersection = TRUE)

  # cut NA rows
  res = dplyr::filter(res, !is.na(p1_1))
  res.exp = dplyr::filter(res.exp, !is.na(p1_1))
  print(res)
  print(res.exp)
  expect_equal(nrow(res), 8) # 8 intersection segments

  expect_equal(dplyr::tbl_df(res), dplyr::tbl_df(res.exp))
})
