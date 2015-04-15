\name{NEWS}
\title{News for Package 'grake'}
\encoding{UTF-8}
\section{Changes in grake version 0.0-2 (2015-04-15)}{
  \itemize{
    \item New functions \code{cginv} (generalized inverse with a pluggable \code{svd} implementation)
      and \code{gginv} (construct a \code{ginv} function given an \code{svd} implementation)
    \item New argument \code{ginv} to \code{calibWeights} allows using a custom \code{ginv} function,
      and in some cases even a function that only inverts quadratic non-singular matrices
      such as \code{solve}
  }
}
\section{Changes in grake version 0.0-1 (2014-11-26)}{
  \itemize{
    \item Initial setup
    \item Imported functions \code{calibWeights} and \code{calibVars} and \code{eusilc} data from
      \code{laeken} package
    \item Added \code{testthat} infrastructure and trivial tests
    \item Testing with Snap-CI, passes \code{R CMD check}
  }
}
