test_that("get_ratio returns the ratio of two scenarios", {
  fit <- lm(log(dist) ~ speed, data = cars)
  res <- simloglm(fit, scenario = list(speed = c(5, 20)),
                  nsim_est = 500, verbose = FALSE)

  r <- get_ratio(res)
  expect_s3_class(r, "ratio_simloglm")
  expect_length(r$ratio, 1L)
  expect_length(r$quantiles, 2L)
  expect_equal(unname(r$ratio),
               unname(exp(coef(fit)[2] * (20 - 5))),
               tolerance = 1e-10)
  expect_true(r$quantiles[1] < r$ratio && r$ratio < r$quantiles[2])
})

test_that("the sigma^2 / 2 correction cancels in a ratio", {
  # exp(X2 b + s2/2) / exp(X1 b + s2/2) = exp((X2 - X1) b), draw by draw.
  res <- simloglm(lm(log(dist) ~ speed, data = cars),
                  scenario = list(speed = c(5, 20)),
                  nsim_est = 300, verbose = FALSE)

  median_ratio <- res$median[, 2] / res$median[, 1]
  mean_ratio <- res$mean[, 2] / res$mean[, 1]
  expect_equal(median_ratio, mean_ratio, tolerance = 1e-12)

  expect_equal(get_ratio(res, which_qoi = "median")$ratio,
               get_ratio(res, which_qoi = "mean")$ratio,
               tolerance = 1e-10)
  expect_equal(get_ratio(res, which_qoi = "median")$quantiles,
               get_ratio(res, which_qoi = "mean")$quantiles,
               tolerance = 1e-10)
})

test_that("a log-log ratio recovers the elasticity", {
  fit <- lm(log(dist) ~ log(speed), data = cars)
  res <- simloglm(fit, scenario = list(speed = c(10, 11)),
                  nsim_est = 300, verbose = FALSE)

  # A 10% increase in x multiplies the median of y by 1.1^beta.
  expect_equal(unname(get_ratio(res)$ratio),
               unname(1.1^coef(fit)[2]),
               tolerance = 1e-10)
})

test_that("get_ratio needs at least two scenarios", {
  res <- simloglm(lm(log(dist) ~ speed, data = cars),
                  nsim_est = 100, verbose = FALSE)
  expect_error(get_ratio(res), "two scenarios")
  expect_error(get_ratio(list(a = 1)), "simloglm")
})

test_that("which_scenarios picks the right pair", {
  fit <- lm(log(dist) ~ speed, data = cars)
  res <- simloglm(fit, scenario = list(speed = c(5, 10, 20)),
                  nsim_est = 200, verbose = FALSE)

  expect_equal(unname(get_ratio(res, which_scenarios = c(1, 3))$ratio),
               unname(exp(coef(fit)[2] * (20 - 5))),
               tolerance = 1e-10)
})
