set.seed(123)
N <- 1000
M <- 60
x <- runif(N) * M

d <- rep(runif(1), N)

rs <-
data.frame(x2 = cut(x, seq(0, M, by = 2), right = FALSE),
           x5 = cut(x, seq(0, M, by = 5), right = FALSE),
           x10 = cut(x, seq(0, M, by = 10), right = FALSE))
rs$id <- seq_along(rs$x2)

x2c <- data.frame(x2 = kimisc::ofactor(levels(rs$x2)))
x2c$N <- 2

x5c <- data.frame(x5 = kimisc::ofactor(factor(levels(rs$x5))))
x5c$N <- 5

x10c <- data.frame(x10 = kimisc::ofactor(factor(levels(rs$x10))))
x10c$N <- 10

problem <- MultiLevelIPF::fitting_problem(
  rs,
  field_names = MultiLevelIPF::special_field_names("id", "id", "", "N"),
  individual_controls = list(x2c, x5c, x10c),
  group_controls = list())

flat <- MultiLevelIPF::flatten_ml_fit_problem(problem, model_matrix_type = "combined", verbose = TRUE)

svd((flat$ref_sample * flat$weights) %*% t(flat$ref_sample))$d

g <- sampling::calib(t(flat$ref_sample), flat$weights, flat$target_values, method = "linear")
summary(flat$ref_sample %*% (g * flat$weights) - flat$target_values)

g <- survey::grake(t(flat$ref_sample), flat$weights, population = flat$target_values, calfun = survey::cal.linear, bounds = c(-Inf, Inf), maxit = 10, verbose = FALSE, epsilon = 1e-6)
summary(flat$ref_sample %*% (g * flat$weights) - flat$target_values)

g <- grake::calibWeights(t(flat$ref_sample), flat$weights, totals = flat$target_values, method = "linear", eps = 2 * .Machine$double.eps)
summary(flat$ref_sample %*% (g * flat$weights) - flat$target_values)
