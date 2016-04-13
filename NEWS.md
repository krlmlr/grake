# grake 0.1 (2016-04-13)

- Initial GitHub release.
- Functions `dss()` and `gginv()`, with tests.
- `dss()` implements generalized raking as descried by Deville et al. (1993).
  The implementation requires a function that inverts a matrix. `gginv()`
  creates such a function (given a tolerance and a function that computes
  a singular value decomposition), but `solve` can also be used in some cases.
