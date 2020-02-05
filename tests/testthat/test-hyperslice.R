context("test-hyperslice.R")

check.slice.row = function(test, exp) {
  p1.test = c(test$p1_1, test$p1_2)
  p1.exp = c(exp$p1_1, exp$p1_2)
  p2.test = c(test$p2_1, test$p2_2)
  p2.exp = c(exp$p2_1, exp$p2_2)
  # put test and exp so p1_1 is "left" of p2_1
  if(test$p1_1 > test$p2_1 || (test$p1_1 == test$p2_1 && test$p1_2 > test$p2_2)) {
    tmp = p1.test
    p1.test = p2.test
    p2.test = tmp
  }
  if(exp$p1_1 > exp$p2_1 || (exp$p1_1 == exp$p2_1 && exp$p1_2 > exp$p2_2)) {
    tmp = p1.exp
    p1.exp = p2.exp
    p2.exp = tmp
  }

  # check for equality
  tol = sqrt(.Machine$double.eps)
  p1.diff = p1.test - p1.exp
  p2.diff = p2.test - p2.exp
  sqrt(t(p1.diff) %*% p1.diff) < tol & sqrt(t(p2.diff) %*% p2.diff) < tol
}

test.mesh.1 = list(
  problemSpec = list(
    dimNames = c("x1", "x2", "x3"),
    limits = list(x1=c(0,1), x2=c(0,1), x3=c(0,1))
  ),
  points = data.frame(x1=c(0.0, 0.0, 1.0), x2=c(0.0, 1.0, 0.0), x3=c(0.0, 0.0, 1.0)),
  simplices = rbind(c(1, 2, 3))
)
class(test.mesh.1) = "Mesh"

test.cube.3d = convmesh(expand.grid(x1=c(0,1), x2=c(0,1), x3=c(0,1)), nice=TRUE)

test_that("function checks for focus points", {
  expect_error(hyperslice(test.mesh.1), "either n or focus.points must be specified", fixed=TRUE)
})

test_that("integer check for n", {
  expect_error(hyperslice(test.mesh.1, n="bc"), "n must be an integer", fixed=TRUE)
})

test_that("focus point outside triangle returns nothing", {
  res = hyperslice(test.mesh.1, focus.points=c(1.5, 1.5, 1.5))

  expect_equal(nrow(res$slices), 0)
})

test_that("single slice of a single triangle", {
  res = hyperslice(test.mesh.1, focus.points=c(0.5, 0.5, 0.8))
  #print(res)
  exp.slices.1 = list(
    problemSpec=test.mesh.1$problemSpec,
    focusPoints = data.frame(x1=0.5, x2=0.5, x3=0.8),
    slices = data.frame(
      p1_1=c(0.8, 0.0, 0.0),
      p1_2=c(0.0, 0.0, 0.5),
      p2_1=c(0.8, 0.5, 0.5),
      p2_2=c(0.2, 0.5, 0.5),
      d1=c(1, 1, 2),
      d2=c(2, 3, 3),
      fpid=c(1, 1, 1)
    )
  )
  class(exp.slices.1) = "SliceSet"
  print(res$slices)
  print(exp.slices.1$slices)
  expect_equal(res, exp.slices.1)
})

test_that("hyperslice correctly combines intersect.simplices", {
  r.hs = hyperslice(test.cube.3d, focus.points=rep(0.5, 3))

  is.1x2 = intersect.simplices(test.cube.3d, rep(0.5, 3), 1, 2) %>% dplyr::filter(!is.na(p1_1))
  is.1x3 = intersect.simplices(test.cube.3d, rep(0.5, 3), 1, 3) %>% dplyr::filter(!is.na(p1_1))
  is.2x3 = intersect.simplices(test.cube.3d, rep(0.5, 3), 2, 3) %>% dplyr::filter(!is.na(p1_1))

  exp.1x2 = r.hs$slices %>% dplyr::filter(d1==1, d2==2) %>% dplyr::select(p1_1, p1_2, p2_1, p2_2)
  exp.1x3 = r.hs$slices %>% dplyr::filter(d1==1, d2==3) %>% dplyr::select(p1_1, p1_2, p2_1, p2_2)
  exp.2x3 = r.hs$slices %>% dplyr::filter(d1==2, d2==3) %>% dplyr::select(p1_1, p1_2, p2_1, p2_2)

  expect_equal(is.1x2, exp.1x2)
  expect_equal(is.1x3, exp.1x3)
  expect_equal(is.2x3, exp.2x3)
})

test_that("all 3d slices of a cube are the same as standard slicing algo", {
  n.slices = 10
  test.slices = hyperslice(test.cube.3d, n=n.slices, use.3d.intersection = FALSE)$slices
  exp.slices = hyperslice(test.cube.3d, n=n.slices, use.3d.intersection = TRUE)$slices # use the "true" 3d intersection algo
  #print(test.slices)
  #print(exp.slices)
  #expect_equal(test.slices, exp.slices)

  expect_equal(nrow(test.slices), nrow(exp.slices))
  for(r in 1:nrow(test.slices)) {
    expect_true(check.slice.row(test.slices[r,], exp.slices[r,]), info=list(test=test.slices[r,], exp=exp.slices[r,]))
  }
})

test_that("filling in focus points respects the bounds of the mesh", {
  test.cube = convmesh(expand.grid(x1=c(-2,-1), x2=c(-2,-1), x3=c(-2,-1)), nice=TRUE)
  s = hyperslice(test.cube, focus.points=c(-1.5,-1.5,NA), n=20) # 20 examples should be enough...
  fps = s$focusPoints$x3
  expect_true(all(fps <= -1)) # TODO: clean up tests to show which vector values fail the test
  expect_true(all(fps >= -2))
})
