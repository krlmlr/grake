# ---------------------------------------
# Author: Kirill Müller
#         ETH Zurich
# Original ginv implementation from MASS
# ---------------------------------------

#' Generalized Inverse of a Matrix using a custom SVD
#'
#' The \code{gginv} function creates a function that
#' calculates the Moore-Penrose generalized inverse of a matrix X using a
#' fixed tolerance value and a custom
#' implementation for computing the singular value decomposition.
#'
#' The \code{svd} argument is expected to adhere to the interface of
#' \code{base::\link[base]{svd}}. It will be called as \code{svd(x)} (with the
#' \code{nu} and \code{nv} arguments unset) and is expected to return a named
#' list with components \code{d}, \code{u} and \code{v}.
#'
#' @inheritParams MASS::ginv
#' @param svd A function that computes the singular value decomposition of a
#'   matrix
#'
#' @return A MP generalized inverse matrix for X.
#'
#' @seealso \code{\link[MASS]{ginv}}, \code{\link[base]{svd}}
#'
#' @author Adapted implementation from the \code{MASS} package.
#'
#' @export
gginv <- function(tol = sqrt(.Machine$double.eps), svd = base::svd) {
  env <- new.env(parent = baseenv())
  env$svd <- svd
  ret <- eval(bquote({
    function(X) {
      if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X))) {
        stop("'X' must be a numeric or complex matrix")
      }
      if (!is.matrix(X)) {
        X <- as.matrix(X)
      }
      Xsvd <- svd(X)
      d <- Xsvd$d
      u <- Xsvd$u
      if (is.complex(X)) u <- Conj(u)
      v <- Xsvd$v

      Positive <- which(d > max(.(tol) * d[[1L]], 0))
      if (length(Positive) == length(d)) {
        v %*% ((1 / d) * t(u))
      } else if (length(Positive) == 0) {
        array(0, dim(X)[2:1])
      } else {
        v[, Positive, drop = FALSE] %*%
          ((1 / d[Positive]) * t(u[, Positive, drop = FALSE]))
      }
    }
  }))

  environment(ret) <- env
  ret
}
