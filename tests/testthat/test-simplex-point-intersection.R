context("test-simplex-point-intersection.R")


planar.triangle = matrix(c(
  1,1,1,
  3,1,1,
  2,2,1),
  ncol=3, byrow=TRUE)

off.axis.triangle = matrix(c(
  0.14904467, 0.9743324, 0.16870685,
  0.98993366, 0.1410158, 0.01207846,
  0.01350696, 0.3065440, 0.95176064),
  ncol=3, byrow=TRUE)

off.axis.triangle.2 = matrix(c(
  0.0, 0.0, 0.0,
  0.0, 1.0, 0.0,
  1.0, 0.0, 1.0),
  ncol=3, byrow=TRUE)

test_that("3d-embedded triangle in plane (intersection)", {
  res <- simplex.point.intersection(planar.triangle, c(1.0, 1.0, 1.0), 1, 2)

  res.exp = data.frame(
    d1Min = c(1,1,3),
    d1Max = c(3,2,2),
    d2Min = c(1,1,1),
    d2Max = c(1,2,2)
  )

  expect_equal(dplyr::tbl_df(res), dplyr::tbl_df(res.exp))
})

test_that("3d-embedded triangle in plane (non-intersection)", {
  res <- simplex.point.intersection(planar.triangle, c(1.0, 1.0, 0.0), 1, 2)
  #print(res)

  expect_that(nrow(res), equals(0))
})

test_that("off-axis triangle - dims 1,2 (intersection)", {
  res <- simplex.point.intersection(off.axis.triangle.2, c(0.5, 0.5, 0.8), 1, 2)
  res.exp <- intersect.tri(off.axis.triangle.2, c(0.5,0.5,0.8), 1, 2)
  #print(res)
  #print(res.exp)

  expect_equal(dplyr::tbl_df(res), dplyr::tbl_df(res.exp))
})

test_that("off-axis triangle - dims 1,3 (intersection)", {
  res <- simplex.point.intersection(off.axis.triangle.2, c(0.5,0.5,0.8), 1, 3)
  res.exp <- intersect.tri(off.axis.triangle.2, c(0.5,0.5,0.8), 1, 3)
  #print(res)

  expect_equal(dplyr::tbl_df(res), dplyr::tbl_df(res.exp))
})

test_that("point inside simplex", {
  res <- simplex.point.intersection(off.axis.triangle.2, c(0.5,0.5,0.5), 1, 3)
  res.exp <- intersect.tri(off.axis.triangle.2, c(0.5,0.5,0.5), 1, 3)
  #print(res)

  expect_equal(dplyr::tbl_df(res), dplyr::tbl_df(res.exp))
})
