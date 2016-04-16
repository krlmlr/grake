# grake 0.1-1 (2016-04-16)

- g-weights are now returned even in case of non-convergence.
- New argument `attributes` to `dss()`, if `TRUE` attributes are added to the result.
- Update `autoroxy` code so that package documentation can be created.


# grake 0.1 (2016-04-13)

- Initial GitHub release.
- Functions `dss()` and `gginv()`, with tests.
- `dss()` implements generalized raking as descried by Deville et al. (1993).
  The implementation requires a function that inverts a matrix. `gginv()`
  creates such a function (given a tolerance and a function that computes
  a singular value decomposition), but `solve` can also be used in some cases.
