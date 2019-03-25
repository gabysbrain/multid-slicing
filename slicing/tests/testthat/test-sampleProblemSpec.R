context("test-sampleProblemSpec.R")

# TODO: convert to quickcheck

test_that("results should have the same dimension names", {
  ps = createProblemSpec(sdagh=c(-1,1), sfhjgsd=c(0, 1), sdgcsd=c(25, 50))
  x = sample.ProblemSpec(ps, 3)
  expect_equal(names(x), ps$dimNames)
})

test_that("results should have the proper number of samples", {
  ps = createProblemSpec(sdagh=c(-1,1), sfhjgsd=c(0, 1), sdgcsd=c(25, 50))
  x = sample.ProblemSpec(ps, 3)
  expect_equal(nrow(x), 3)
})

test_that("results are in the proper range", {
  ps = createProblemSpec(sdagh=c(-1,1), sfhjgsd=c(0, 1), sdgcsd=c(25, 50))
  x = sample.ProblemSpec(ps, 3)

  expect_true(all(x[,"sdagh"] >= ps$limits$sdagh[1]))
  expect_true(all(x[,"sdagh"] <= ps$limits$sdagh[2]))

  expect_true(all(x[,"sfhjgsd"] >= ps$limits$sfhjgsd[1]))
  expect_true(all(x[,"sfhjgsd"] <= ps$limits$sfhjgsd[2]))

  expect_true(all(x[,"sdgcsd"] >= ps$limits$sdgcsd[1]))
  expect_true(all(x[,"sdgcsd"] <= ps$limits$sdgcsd[2]))
})

test_that("-2 to +2 properly centers the range", {
  ps = createProblemSpec(x1=c(-2, 2), x2=c(-2, 2))
  x = sample.ProblemSpec(ps, 1)
  print(x)
  expect_equal(x[1, 1], 0.0)
  expect_equal(x[1, 2], 0.0)
})
