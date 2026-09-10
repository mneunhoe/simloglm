test_that("plot.simloglm draws without error", {
  res <- simloglm(lm(log(dist) ~ speed, data = cars),
                  scenario = list(speed = c(5, 10, 20)),
                  nsim_est = 200, verbose = FALSE)

  f <- tempfile(fileext = ".png")
  grDevices::png(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)

  expect_silent(plot(res))
  expect_silent(plot(res, which_qoi = "mean", alpha = 0.1))
  expect_silent(plot(res, twosided = FALSE))
  expect_error(plot(res, which_qoi = "nonsense"), "which_qoi")
})

test_that("plot.simloglm returns the plotted values invisibly", {
  res <- simloglm(lm(log(dist) ~ speed, data = cars),
                  scenario = list(speed = c(5, 20)),
                  nsim_est = 200, verbose = FALSE)

  f <- tempfile(fileext = ".png")
  grDevices::png(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)

  out <- plot(res)
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 2L)
  expect_named(out, c("scenario", "point_estimate", "lower", "upper"))
})
