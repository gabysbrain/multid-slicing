context("test-hyperslice.R")

test.mesh.1 = list(
  problemSpec = list(
    dimNames = c("x1", "x2", "x3"),
    limits = list(x1=c(0,1), x2=c(0,1), x3=c(0,1))
  ),
  points = data.frame(x1=c(0.0, 0.0, 1.0), x2=c(0.0, 1.0, 0.0), x3=c(0.0, 0.0, 1.0)),
  simplices = rbind(c(1, 2, 3))
)

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
      d2Min=c(0.8, 0.5, 0.5),
      d1Max=c(0.0, 0.0, 0.5),
      d2Max=c(0.2, 0.5, 0.5),
      d1=c(1, 1, 2),
      d2=c(2, 3, 3),
      fpid=c(1, 1, 1)
    )
  )
  class(exp.slices.1) = "SliceSet"
  expect_equal(res, exp.slices.1)
})
