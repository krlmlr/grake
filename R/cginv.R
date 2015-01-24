# ---------------------------------------
# Author: Kirill Müller
#         ETH Zurich
# Original ginv implementation from MASS
# ---------------------------------------

#' Generalized Inverse of a Matrix using a custom SVD
#'
#' Calculates the Moore-Penrose generalized inverse of a matrix X using a custom
#' implementation for computing the singular value decomposition.
#'
#' The \code{svd} parameter is expected to adhere to the interface of
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
#' @author Authors of the \code{MASS} package
#'
#' @export
cginv <- function(X, tol = sqrt(.Machine$double.eps), svd = NULL) {
  if (is.null(svd)) {
    svd <- base::svd
  }

  if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X))) {
    stop("'X' must be a numeric or complex matrix")
  }
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  Xsvd <- svd(X)
  if (is.complex(X)) {
    Xsvd$u <- Conj(Xsvd$u)
  }
  Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
  if (all(Positive)) {
    Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))
  } else if (!any(Positive)) {
    array(0, dim(X)[2L:1L])
  } else {
    Xsvd$v[, Positive, drop = FALSE] %*%
      ((1/Xsvd$d[Positive]) * t(Xsvd$u[, Positive, drop = FALSE]))
  }
}
