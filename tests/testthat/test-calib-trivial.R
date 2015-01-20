context('Trivial')

set.seed(20150120)

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

  for (method in eval(formals(calibWeights)$method)) {
    g <- calibWeights(X, d, totals, method = method)
    expect_equal(g, totals, info = method)
  }
})
