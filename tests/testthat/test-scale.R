test_that("scale maps lower and upper bounds", {
  expect_equal(app_env$scale(0), 0)
  expect_equal(app_env$scale(40), 100)
})

test_that("scale returns nearest lookup value", {
  expect_equal(app_env$scale(1), 9.430647, tolerance = 1e-6)
  expect_equal(app_env$scale(39), 90.369397, tolerance = 1e-6)
})

test_that("scale handles values outside lookup range by nearest endpoint", {
  expect_equal(app_env$scale(-10), 0)
  expect_equal(app_env$scale(100), 100)
})
