# grake 0.0-4 (2016-04-13)

- Update package documentation.
- Rename `calibWeights()` to `dss()`.
- Don't include default bounds in formals, assign in function instead.


# grake 0.0-3 (2016-04-13)

- Remove `eps` argument to `calibWeights()`.
- Use `gginv()` instead of `MASS::ginv()`.
- Remove `cginv()`.
- The function returned by `gginv()` now uses a hard-coded tolerance and has been somewhat optimized.
- Remove `calibVars()` (#8).
- Update documentation.
- Use `rflow`.
- Update wercker configuration.


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
