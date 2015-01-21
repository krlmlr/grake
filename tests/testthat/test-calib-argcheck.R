context('Argument checks')

test_that('NA values are rejected', {
  N <- 10
  X <- diag(N)
  d <- rep(1, N)
  totals <- rep(1, N)
  expect_error(calibWeights(NA, d, totals), "missing value")
  expect_error(calibWeights(X, NA, totals), "missing value")
  expect_error(calibWeights(X, d, NA), "missing value")
  expect_error(calibWeights(X, d, totals, q = NA), "missing value")
})
