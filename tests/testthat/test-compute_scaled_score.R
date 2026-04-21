test_that("compute_scaled_score returns NA vector when no columns are provided", {
  df <- data.frame(Q1 = c("1", "2", "3"), stringsAsFactors = FALSE)

  expect_equal(app_env$compute_scaled_score(df, NULL), rep(NA_real_, 3))
  expect_equal(app_env$compute_scaled_score(df, character(0)), rep(NA_real_, 3))
})

test_that("compute_scaled_score ignores unknown columns", {
  df <- data.frame(Q1 = c("1", "2"), stringsAsFactors = FALSE)

  expect_equal(app_env$compute_scaled_score(df, c("UNKNOWN")), rep(NA_real_, 2))
})

test_that("compute_scaled_score computes scaled output for valid numeric values", {
  df <- data.frame(
    Q1 = c("1", "2", "4"),
    Q2 = c("2", "3", "4"),
    stringsAsFactors = FALSE
  )

  result <- app_env$compute_scaled_score(df, c("Q1", "Q2"))

  expect_length(result, 3)
  expect_true(all(!is.na(result)))
  expect_equal(result[1], app_env$scale(3), tolerance = 1e-6)
  expect_equal(result[2], app_env$scale(5), tolerance = 1e-6)
  expect_equal(result[3], app_env$scale(8), tolerance = 1e-6)
})

test_that("compute_scaled_score coerces non-numeric values and handles partial NA", {
  df <- data.frame(
    Q1 = c("a", "2", ""),
    Q2 = c("1", "b", "3"),
    stringsAsFactors = FALSE
  )

  result <- app_env$compute_scaled_score(df, c("Q1", "Q2"))

  expect_equal(result[1], app_env$scale(1), tolerance = 1e-6)
  expect_equal(result[2], app_env$scale(2), tolerance = 1e-6)
  expect_equal(result[3], app_env$scale(3), tolerance = 1e-6)
})

test_that("compute_scaled_score maps Swedish Likert labels to numeric values", {
  df <- data.frame(
    Q1 = c("Aldrig", " Sällan ", "IBLAND", "ofta", "Alltid"),
    stringsAsFactors = FALSE
  )

  result <- app_env$compute_scaled_score(df, "Q1")

  expect_equal(result, vapply(0:4, app_env$scale, numeric(1)), tolerance = 1e-6)
})

test_that("compute_scaled_score handles mixed Swedish labels and numeric strings", {
  df <- data.frame(
    Q1 = c("Aldrig", "Ofta", "2"),
    Q2 = c("4", " 1 ", "Alltid"),
    stringsAsFactors = FALSE
  )

  result <- app_env$compute_scaled_score(df, c("Q1", "Q2"))

  expect_equal(result[1], app_env$scale(4), tolerance = 1e-6)
  expect_equal(result[2], app_env$scale(4), tolerance = 1e-6)
  expect_equal(result[3], app_env$scale(6), tolerance = 1e-6)
})

test_that("compute_scaled_score ignores unknown Swedish-like labels as NA", {
  df <- data.frame(
    Q1 = c("Kanske", "Ofta"),
    Q2 = c("Alltid", "Vet ej"),
    stringsAsFactors = FALSE
  )

  result <- app_env$compute_scaled_score(df, c("Q1", "Q2"))

  expect_equal(result[1], app_env$scale(4), tolerance = 1e-6)
  expect_equal(result[2], app_env$scale(3), tolerance = 1e-6)
})
