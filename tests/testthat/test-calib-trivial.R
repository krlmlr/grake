context('Trivial')

test_that('Calibrating a unit matrix against a unit vector', {
  N <- 10
  X <- diag(N)
  d <- rep(1, N)
  totals <- rep(1, N)
  g <- calibWeights(X, d, totals)
  expect_equal(g, totals)
})

test_that('Calibrating a unit matrix against a unit vector with random totals', {
  N <- 10
  X <- diag(N)
  d <- rep(1, N)
  totals <- runif(N)
  g <- calibWeights(X, d, totals)
  expect_equal(g, totals)
})

test_that('Calibrating a unit matrix against a unit vector with bounds, failing', {
  N <- 10
  X <- diag(1:10)
  d <- rep(1, N)
  totals <- rep(10, N)
  bounds <- c(0.9, 1.1)

  expect_warning(
    g <- calibWeights(X, d, totals, method = "logit", bounds = bounds),
    "no convergence")
  expect_equal(g, NULL)
})
