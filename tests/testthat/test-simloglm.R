NSIM <- 200

cars_fit <- lm(log(dist) ~ speed, data = cars)

loglog_fit <- lm(log(dist) ~ log(speed), data = cars)

cars_grp <- transform(cars, grp = factor(rep(c("a", "b"), 25)))
factor_fit <- lm(log(dist) ~ speed + grp, data = cars_grp)

quad_fit <- lm(log(dist) ~ speed + I(speed^2), data = cars)
poly_fit <- lm(log(dist) ~ poly(speed, 2), data = cars)

test_that("the documented happy path returns matrices of the right shape", {
  res <- simloglm(cars_fit,
                  scenario = list(speed = c(5, 20)),
                  nsim_est = NSIM, verbose = FALSE)

  expect_s3_class(res, "simloglm")
  expect_true(is.matrix(res$median))
  expect_true(is.matrix(res$mean))
  expect_equal(dim(res$median), c(NSIM, 2L))
  expect_equal(dim(res$mean), c(NSIM, 2L))
  expect_equal(colnames(res$median), colnames(res$mean))
  expect_length(res$median_point_estimate, 2L)
  expect_length(res$mean_point_estimate, 2L)
  expect_true(all(res$median > 0))
  # The arithmetic mean exceeds the geometric mean by exp(sigma^2 / 2) > 1.
  expect_true(all(res$mean > res$median))
})

test_that("a log-log model works with a scenario on the original scale", {
  res <- simloglm(loglog_fit,
                  scenario = list(speed = c(5, 20)),
                  nsim_est = NSIM, verbose = FALSE)

  expect_equal(dim(res$median), c(NSIM, 2L))
  # Point estimate must equal exp(b0 + b1 * log(speed)).
  b <- coef(loglog_fit)
  expect_equal(as.vector(res$median_point_estimate),
               as.vector(exp(b[1] + b[2] * log(c(5, 20)))),
               tolerance = 1e-10)
})

test_that("a log-log model works without a scenario", {
  res <- simloglm(loglog_fit, nsim_est = NSIM, verbose = FALSE)

  expect_equal(dim(res$median), c(NSIM, 1L))
  # The default sets speed to its arithmetic mean on the original scale.
  b <- coef(loglog_fit)
  expect_equal(as.vector(res$median_point_estimate),
               as.vector(exp(b[1] + b[2] * log(mean(cars$speed)))),
               tolerance = 1e-10)
})

test_that("a factor covariate works with and without a scenario", {
  res <- simloglm(factor_fit,
                  scenario = list(speed = c(5, 20), grp = c("a", "b")),
                  nsim_est = NSIM, verbose = FALSE)
  expect_equal(dim(res$median), c(NSIM, 2L))

  # colMeans() used to error outright on the factor column.
  res_default <- simloglm(factor_fit, nsim_est = NSIM, verbose = FALSE)
  expect_equal(dim(res_default$median), c(NSIM, 1L))
  b <- coef(factor_fit)
  # Numerics at their mean, the factor at its reference level.
  expect_equal(as.vector(res_default$median_point_estimate),
               as.vector(exp(b[1] + b[2] * mean(cars$speed))),
               tolerance = 1e-10)
})

test_that("I(x^2) and poly() terms work with and without a scenario", {
  for (fit in list(quad_fit, poly_fit)) {
    res <- simloglm(fit, scenario = list(speed = c(5, 20)),
                    nsim_est = NSIM, verbose = FALSE)
    expect_equal(dim(res$median), c(NSIM, 2L))
    expect_equal(as.vector(res$median_point_estimate),
                 as.vector(exp(predict(fit, newdata = data.frame(speed = c(5, 20))))),
                 tolerance = 1e-10)

    res_default <- simloglm(fit, nsim_est = NSIM, verbose = FALSE)
    expect_equal(dim(res_default$median), c(NSIM, 1L))
  }
})

test_that("the no-scenario default sets every variable to its mean", {
  res <- simloglm(cars_fit, nsim_est = NSIM, verbose = FALSE)

  expect_equal(dim(res$median), c(NSIM, 1L))
  b <- coef(cars_fit)
  expect_equal(as.vector(res$median_point_estimate),
               as.vector(exp(b[1] + b[2] * mean(cars$speed))),
               tolerance = 1e-10)
})

test_that("scenario values are recycled to a common length", {
  res <- simloglm(factor_fit,
                  scenario = list(speed = c(5, 10, 20), grp = "a"),
                  nsim_est = NSIM, verbose = FALSE)
  expect_equal(dim(res$median), c(NSIM, 3L))
})

test_that("invalid scenarios are rejected with an informative message", {
  expect_error(
    simloglm(cars_fit, scenario = list(speeed = 5), nsim_est = NSIM, verbose = FALSE),
    "speeed"
  )
  expect_error(
    simloglm(factor_fit, scenario = list(speed = c(5, 10, 20, 30), grp = c("a", "b")),
             nsim_est = NSIM, verbose = FALSE),
    "length"
  )
})

test_that("non-positive values entering a log() are caught early", {
  expect_error(
    simloglm(loglog_fit, scenario = list(speed = c(0, 20)),
             nsim_est = NSIM, verbose = FALSE),
    "log"
  )
})

test_that("the observed value approach works", {
  res <- simloglm(cars_fit,
                  scenario = list(speed = c(5, 20)),
                  observed_value_approach = TRUE,
                  nsim_est = NSIM, verbose = FALSE)

  expect_true(is.matrix(res$median))
  expect_equal(dim(res$median), c(NSIM, 2L))
  expect_equal(dim(res$mean), c(NSIM, 2L))
  expect_length(res$median_point_estimate, 2L)
  expect_true(all(res$mean > res$median))
})

test_that("the observed value approach works without a scenario", {
  res <- simloglm(cars_fit, observed_value_approach = TRUE,
                  nsim_est = NSIM, verbose = FALSE)
  expect_equal(dim(res$median), c(NSIM, 1L))
})

test_that("each observed-value scenario gets its own column", {
  set.seed(11)
  res <- simloglm(cars_fit,
                  scenario = list(speed = c(5, 20)),
                  observed_value_approach = TRUE,
                  nsim_est = NSIM, verbose = FALSE)
  # The loop used to discard everything but the last scenario.
  expect_false(isTRUE(all.equal(res$median[, 1], res$median[, 2])))
  expect_true(all(res$median[, 2] > res$median[, 1]))
})

test_that("the observed-value shortcut agrees with an explicit rebuild", {
  # A model where the scenario variable enters through a transformation and an
  # interaction, so both the shortcut and the general rebuild get exercised.
  cases <- list(
    # Shortcut: every term touched by the scenario involves only scenario
    # variables, so the affected columns of X become constant.
    list(fit = cars_fit,   scenario = list(speed = c(5, 20))),
    list(fit = loglog_fit, scenario = list(speed = c(5, 20))),
    list(fit = quad_fit,   scenario = list(speed = c(5, 20))),
    list(fit = poly_fit,   scenario = list(speed = c(5, 20))),
    list(fit = factor_fit, scenario = list(speed = c(5, 20), grp = c("a", "b"))),
    # General rebuild: speed:grp reaches a variable outside the scenario.
    list(fit = lm(log(dist) ~ speed * grp, data = cars_grp),
         scenario = list(speed = c(5, 20))),
    list(fit = lm(log(dist) ~ log(speed) * grp, data = cars_grp),
         scenario = list(speed = c(5, 20)))
  )

  for (case in cases) {
    set.seed(7)
    res <- simloglm(case$fit, scenario = case$scenario,
                    observed_value_approach = TRUE,
                    nsim_est = 50, verbose = FALSE)
    set.seed(7)
    ref <- observed_value_reference(case$fit, case$scenario, nsim_est = 50)
    expect_equal(unname(res$median), ref$median, tolerance = 1e-10)
    expect_equal(unname(res$mean), ref$mean, tolerance = 1e-10)
  }
})

test_that("the observed value approach supports a multiplicative shift", {
  set.seed(3)
  res <- simloglm(cars_fit,
                  multiplier = list(speed = c(1, 1.1)),
                  observed_value_approach = TRUE,
                  nsim_est = NSIM, verbose = FALSE)

  expect_equal(dim(res$median), c(NSIM, 2L))
  # multiplier = 1 must reproduce the untouched sample.
  set.seed(3)
  base <- simloglm(cars_fit, observed_value_approach = TRUE,
                   nsim_est = NSIM, verbose = FALSE)
  expect_equal(unname(res$median[, 1]), unname(base$median[, 1]), tolerance = 1e-10)
  expect_true(all(res$median[, 2] > res$median[, 1]))
})

test_that("a user supplied list works in place of an lm object", {
  obj <- lm_to_obj(cars_fit)
  res <- simloglm(obj, X = cbind(1, c(5, 20)), nsim_est = NSIM, verbose = FALSE)

  expect_equal(dim(res$median), c(NSIM, 2L))
  expect_equal(as.vector(res$median_point_estimate),
               as.vector(exp(cbind(1, c(5, 20)) %*% coef(cars_fit))),
               tolerance = 1e-10)

  expect_error(simloglm(obj[c("beta_hat", "n")], X = cbind(1, 5), verbose = FALSE),
               "sigma_hat")
})

test_that("the slow path converges to the fast path", {
  set.seed(99)
  slow <- simloglm(cars_fit, scenario = list(speed = c(5, 20)),
                   fast = FALSE, nsim_est = 300, nsim_fund = 4000,
                   verbose = FALSE)
  set.seed(99)
  fast <- simloglm(cars_fit, scenario = list(speed = c(5, 20)),
                   nsim_est = 300, verbose = FALSE)

  expect_equal(dim(slow$median), dim(fast$median))
  expect_equal(colMeans(slow$median), colMeans(fast$median), tolerance = 0.05)
  expect_equal(colMeans(slow$mean), colMeans(fast$mean), tolerance = 0.05)
})

test_that("predicted values are returned on the slow path", {
  res <- simloglm(cars_fit, scenario = list(speed = c(5, 20)),
                  fast = FALSE, predicted_values = TRUE,
                  nsim_est = 20, nsim_fund = 30, verbose = FALSE)

  expect_length(res$predicted_values, 2L)
  expect_equal(dim(res$predicted_values[[1]]), c(30L, 20L))
  expect_true(all(res$predicted_values[[1]] > 0))
})

test_that("logged_dv = FALSE skips the exponentiation", {
  fit <- lm(dist ~ speed, data = cars)
  res <- simloglm(fit, scenario = list(speed = c(5, 20)), logged_dv = FALSE,
                  nsim_est = NSIM, verbose = FALSE)

  expect_equal(as.vector(res$median_point_estimate),
               as.vector(predict(fit, newdata = data.frame(speed = c(5, 20)))),
               tolerance = 1e-10)
  expect_equal(res$median, res$mean)
})

test_that("logged_dv defaults to TRUE even when the formula looks unlogged", {
  # A dependent variable logged before the fit is indistinguishable from one
  # that was not, so the guess must never silently change the answer.
  df <- transform(cars, logdist = log(dist))
  fit <- lm(logdist ~ speed, data = df)

  expect_message(
    res <- simloglm(fit, scenario = list(speed = 5), nsim_est = 20),
    "does not look logged"
  )
  expect_equal(as.vector(res$median_point_estimate),
               as.vector(exp(predict(fit, newdata = data.frame(speed = 5)))),
               tolerance = 1e-10)
})

test_that("logged_dv = NULL asks for the formula to be used", {
  expect_message(
    res <- simloglm(lm(dist ~ speed, data = cars), scenario = list(speed = 5),
                    nsim_est = 20, logged_dv = NULL),
    "logged_dv was set to FALSE"
  )
  expect_false(res$logged_dv)

  expect_message(
    simloglm(cars_fit, scenario = list(speed = 5), nsim_est = 20,
             logged_dv = NULL),
    "logged_dv was set to TRUE"
  )
})

test_that("verbose = FALSE silences messages and progress bars", {
  expect_silent(simloglm(cars_fit, nsim_est = NSIM, verbose = FALSE))
  expect_silent(simloglm(cars_fit, observed_value_approach = TRUE,
                         nsim_est = NSIM, verbose = FALSE))
})

test_that("sim_param draws the right multivariate t distribution", {
  obj <- lm_to_obj(cars_fit)
  set.seed(5)
  p <- sim_param(nsim = 20000, beta_hat = obj$beta_hat,
                 unscaled_vcov = obj$unscaled_vcov, sigma_hat = obj$sigma_hat,
                 n = obj$n, k = obj$k)

  expect_equal(dim(p$betas), c(20000L, 2L))
  expect_equal(colnames(p$betas), names(obj$beta_hat))
  expect_equal(colMeans(p$betas), obj$beta_hat, tolerance = 0.02)
  expect_equal(cov(p$betas), vcov(cars_fit), tolerance = 0.05)
  # The inverse gamma draws have mean sigma_hat^2 * (n - k) / (n - k - 2).
  expect_equal(mean(p$sigma),
               obj$sigma_hat^2 * (obj$n - obj$k) / (obj$n - obj$k - 2),
               tolerance = 0.02)
})

test_that("multipliers are rejected where they make no sense", {
  expect_error(
    simloglm(cars_fit, multiplier = list(speed = c(1, 1.1)), nsim_est = 20,
             verbose = FALSE),
    "observed_value_approach"
  )
  expect_error(
    simloglm(factor_fit, multiplier = list(grp = c(1, 2)),
             observed_value_approach = TRUE, nsim_est = 20, verbose = FALSE),
    "numeric"
  )
  expect_error(
    simloglm(cars_fit, scenario = list(speed = 5), multiplier = list(speed = 1.1),
             observed_value_approach = TRUE, nsim_est = 20, verbose = FALSE),
    "either a scenario or a multiplier"
  )
  expect_error(
    simloglm(loglog_fit, multiplier = list(speed = c(1, -1)),
             observed_value_approach = TRUE, nsim_est = 20, verbose = FALSE),
    "must be positive"
  )
})

test_that("a mismatched model matrix is rejected", {
  obj <- lm_to_obj(cars_fit)
  expect_error(simloglm(obj, X = cbind(1, 5, 3), nsim_est = 20, verbose = FALSE),
               "3 columns")
})
