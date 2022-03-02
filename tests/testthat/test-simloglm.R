test_that("simulate() works with a lm object", {
  expect_equal(length(simloglm(lm(
    log(dist) ~ speed, data = cars
  ))$geometric_mean), 1000)
})

test_that("simulate() works with a lm object and a list scenario", {
  expect_equal(nrow(simloglm(
    lm(log(dist) ~ speed, data = cars), scenario = list(speed = c(5, 10))
  )$geometric_mean), 1000)
})


