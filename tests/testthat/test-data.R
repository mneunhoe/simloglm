test_that("the bundled datasets have the documented dimensions", {
  expect_equal(dim(shepherd_you_2020), c(3074L, 90L))
  expect_equal(dim(hollibaugh_rothenberg_2018), c(9382L, 70L))
})
