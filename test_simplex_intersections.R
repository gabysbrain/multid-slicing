debugSource('server/pareto.R')

library(testthat)

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

test_that("tet with 2D plane", {
  res <- simplex.intersect.test(1, 3, c(0.0, 1.5, 0.0), simple.simplex)
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
  res <- simplex.intersect.test(1, 2, c(1.0, 1.0, 1.0), planar.triangle)
  
  expect_that(nrow(res), equals(3))

  expect_equal(as.vector(res$d1.min), c(1,1,3))
  expect_equal(as.vector(res$d1.max), c(3,2,2))
  expect_equal(as.vector(res$d2.min), c(1,1,1))
  expect_equal(as.vector(res$d2.max), c(1,2,2))
})

test_that("3d-embedded triangle in plane (non-intersection)", {
  res <- simplex.intersect.test(1, 2, c(1.0, 1.0, 0.0), planar.triangle)

  expect_that(nrow(res), equals(4))
  expect_equal(as.vector(unlist(res)), rep(NA, 4*4))
})

test_that("s2", {
  res <- simplex.intersect.test(1, 3, c(0.0719, 0.6409, 0.4756), off.axis.triangle)
  print(res)
  
  expect_that(nrow(res), equals(4))
  expect_equal(as.vector(unlist(res)), rep(NA, 3*4))
})
