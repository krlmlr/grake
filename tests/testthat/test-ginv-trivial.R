context('ginv: Trivial')

set.seed(20150121)

test_that('Compare ginv results', {
  rows <- 13L
  cols <- 19L
  X <- matrix(runif(rows * cols), rows, cols)
  expect_equal(MASS::ginv(X), cginv(X))
  expect_equal(MASS::ginv(X, tol = 1), cginv(X, tol = 1))
  expect_equal(MASS::ginv(X, tol = 1), cginv(X, tol = 1))
  X <- matrix(complex(real = runif(rows * cols), imag = runif(rows * cols)),
              rows, cols)
  expect_equal(MASS::ginv(X), cginv(X))
})
