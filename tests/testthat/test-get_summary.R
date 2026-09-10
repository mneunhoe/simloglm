test_that("get_summary returns a point estimate and quantiles per scenario", {
  res <- simloglm(lm(log(dist) ~ speed, data = cars),
                  scenario = list(speed = c(5, 10, 20)),
                  nsim_est = 200, verbose = FALSE)

  s <- get_summary(res)
  expect_s3_class(s, "summary_simloglm")
  expect_length(s$point_estimate, 3L)
  expect_equal(dim(s$quantiles), c(2L, 3L))
  expect_equal(colnames(s$quantiles), colnames(res$median))
  expect_true(all(s$quantiles[1, ] < s$quantiles[2, ]))

  s_mean <- get_summary(res, which_qoi = "mean")
  expect_true(all(s_mean$point_estimate > s$point_estimate))
})

test_that("get_summary respects alpha", {
  res <- simloglm(lm(log(dist) ~ speed, data = cars),
                  scenario = list(speed = c(5, 20)),
                  nsim_est = 500, verbose = FALSE)

  wide <- get_summary(res, alpha = 0.01)$quantiles
  narrow <- get_summary(res, alpha = 0.2)$quantiles
  expect_true(all(wide[1, ] <= narrow[1, ]))
  expect_true(all(wide[2, ] >= narrow[2, ]))
})

test_that("get_summary rejects other objects", {
  expect_error(get_summary(list(a = 1)), "simloglm")
})
