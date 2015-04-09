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

test_that('ginv error messages', {
  expect_error(cginv(array(1:24, dim = 2:4)), "matrix")
  expect_error(cginv(array(letters[1:6], dim = 2:3)), "numeric or complex")
})
