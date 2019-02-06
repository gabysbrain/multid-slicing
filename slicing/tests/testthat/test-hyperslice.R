context("test-hyperslice.R")

test.mesh.1 = list(
  points = data.frame(x1=c(0.0, 0.0, 1.0), x2=c(0.0, 1.0, 0.0), x3=c(0.0, 0.0, 1.0)),
  simplices = rbind(c(1, 2, 3))
)

exp.slices.1 = list(
  focusPoints = data.frame(x1=0.5, x2=0.5, x3=0.5),
  simplices = data.frame(
    fpid=c(1, 1, 1),
    d1=c(1, 1, 2),
    d2=c(1, 3, 3),
    x1=c(0, 0, 0),
    y1=c(0, 0, 0),
    x2=c(0, 0, 0),
    y2=c(0, 0, 0)
  )
)

test_that("function checks for focus points", {
  expect_error(hyperslice(test.mesh.1), "either n or focus.points must be specified", fixed=TRUE)
})

test_that("integer check for n", {
  expect_error(hyperslice(test.mesh.1, n="bc"), "n must be an integer", fixed=TRUE)
})

test_that("single slice works", {
  res = hyperslice(test.mesh.1, focus.points=c(0.5, 0.5, 0.5))
  expect_identical(res, exp.slices.1)
})
