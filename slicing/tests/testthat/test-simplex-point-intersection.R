context("test-simplex-point-intersection.R")


simple.simplex = matrix(c(
  2,2,1,
  2,1,2,
  1,1,0,
  3,1,0),
  ncol=3, byrow=TRUE)

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

test_that("tet with 2D plane", {
  res <- simplex.point.intersection(simple.simplex, c(0.0, 1.5, 0.0), 1, 3)
  # expect_equal(res, data.frame(d1.min=c(NA, 1.5, 2.0, 1.5), # columns
  #                              d2.min=c(NA, 0.5, 1.5, 0.5),
  #                              d1.max=c(NA, 2.5, 2.5, 2.0),
  #                              d2.max=c(NA, 0.5, 0.5, 1.5)))
  expect_equal(as.vector(res$d1.min), c(NA, 1.5, 2.0, 1.5))
  expect_equal(as.vector(res$d2.min), c(NA, 0.5, 1.5, 0.5))
  expect_equal(as.vector(res$d1.max), c(NA, 2.5, 2.5, 2.0))
  expect_equal(as.vector(res$d2.max), c(NA, 0.5, 0.5, 1.5))
})

test_that("3d-embedded triangle in plane (intersection)", {
  res <- simplex.point.intersection(planar.triangle, c(1.0, 1.0, 1.0), 1, 2)

  expect_that(nrow(res), equals(3))

  expect_equal(as.vector(res$d1.min), c(1,1,3))
  expect_equal(as.vector(res$d1.max), c(3,2,2))
  expect_equal(as.vector(res$d2.min), c(1,1,1))
  expect_equal(as.vector(res$d2.max), c(1,2,2))
})

test_that("3d-embedded triangle in plane (non-intersection)", {
  res <- simplex.point.intersection(planar.triangle, c(1.0, 1.0, 1.0), 1, 2)

  expect_that(nrow(res), equals(1))
  expect_equal(as.vector(unlist(res)), rep(NA, 4))
})

test_that("off-axis triangle - dims 1,2 (intersection)", {
  res <- simplex.point.intersection(off.axis.triangle.2, c(0.5, 0.5, 0.8), 1, 2)
  res.exp <- intersect.tri(off.axis.triangle.2, c(0.5,0.5,0.8), 1, 2)
  #print(res)
  #print(res.exp)

  expect_that(nrow(res), equals(1))
  expect_equal(as.vector(unlist(res[1,])), as.vector(unlist(res.exp[1,])))
})

test_that("off-axis triangle - dims 1,3 (intersection)", {
  res <- simplex.point.intersection(off.axis.triangle.2, c(0.5,0.5,0.8), 1, 3)
  res.exp <- intersect.tri(off.axis.triangle.2, c(0.5,0.5,0.8), 1, 3)
  #print(res)

  expect_that(nrow(res), equals(1))
  expect_equal(as.vector(unlist(res[1,])), as.vector(unlist(res.exp[1,])))
})

test_that("point inside simplex", {
  res <- simplex.point.intersection(off.axis.triangle.2, c(0.5,0.5,0.5), 1, 3)
  res.exp <- intersect.tri(off.axis.triangle.2, c(0.5,0.5,0.5), 1, 3)
  #print(res)

  expect_that(nrow(res), equals(1))
  expect_equal(as.vector(unlist(res[1,])), as.vector(unlist(res.exp[1,])))
})
