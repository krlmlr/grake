v0.0-2 (2015-04-15)
===

- New functions `cginv` (generalized inverse with a pluggable `svd` implementation)
  and `gginv` (construct a `ginv` function given an `svd` implementation)
- New argument `ginv` to `calibWeights` allows using a custom `ginv` function,
  and in some cases even a function that only inverts quadratic non-singular matrices
  such as `solve`

v0.0-1 (2014-11-26)
===

- Initial setup
- Imported functions `calibWeights` and `calibVars` and `eusilc` data from
  `laeken` package
- Added `testthat` infrastructure and trivial tests
- Testing with Snap-CI, passes `R CMD check`
