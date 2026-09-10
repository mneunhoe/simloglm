test_that("get_first_difference returns the difference of two scenarios", {
  fit <- lm(log(dist) ~ speed, data = cars)
  res <- simloglm(fit, scenario = list(speed = c(5, 20)),
                  nsim_est = 500, verbose = FALSE)

  fd <- get_first_difference(res)
  expect_s3_class(fd, "first_difference_simloglm")
  expect_length(fd$fd, 1L)
  expect_length(fd$quantiles, 2L)
  expect_equal(unname(fd$fd),
               unname(diff(as.vector(res$median_point_estimate))),
               tolerance = 1e-12)
  expect_equal(unname(fd$baseline),
               unname(as.vector(res$median_point_estimate)[1]),
               tolerance = 1e-12)
  expect_true(fd$quantiles[1] < fd$fd && fd$fd < fd$quantiles[2])
})

test_that("unlike a ratio, a first difference depends on the qoi", {
  res <- simloglm(lm(log(dist) ~ speed, data = cars),
                  scenario = list(speed = c(5, 20)),
                  nsim_est = 300, verbose = FALSE)

  expect_false(isTRUE(all.equal(get_first_difference(res, "median")$fd,
                                get_first_difference(res, "mean")$fd)))
})

test_that("get_first_difference needs at least two scenarios", {
  res <- simloglm(lm(log(dist) ~ speed, data = cars),
                  nsim_est = 100, verbose = FALSE)
  expect_error(get_first_difference(res), "two scenarios")
  expect_error(get_first_difference(list(a = 1)), "simloglm")
})
