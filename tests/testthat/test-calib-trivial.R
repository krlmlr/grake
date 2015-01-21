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

test_that('Calibrating a unit matrix against a vector with random totals', {
  N <- 10
  X <- diag(N)
  d <- rep(1, N)
  totals <- runif(N)

  for (method in eval(formals(calibWeights)$method)) {
    g <- calibWeights(X, d, totals, method = method)
    expect_equal(g, totals, info = method)
  }
})


test_that('Test non-convergence', {
  N <- 10
  X <- diag(N)
  X[1,2] <- 1
  d <- rep(1, N)
  totals <- c(2, rep(1, N - 1))

  for (method in eval(formals(calibWeights)$method)) {
    if (method == "linear") {
      g <- calibWeights(X, d, totals, method = method)
      expect_equal(as.vector(g %*% X), totals, info = method)
      expect_true(any(g < 0))
    } else {
      expect_warning(
        g <- calibWeights(X, d, totals, method = method),
        "no convergence")
      expect_equal(g, NULL)
    }
  }
})
