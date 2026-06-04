test_that("get_likert_numeric_data converts Swedish labels and numeric strings", {
  df <- data.frame(
    Q1 = c("Aldrig", "Ofta", "4"),
    Q2 = c("1", "Alltid", "Sällan"),
    stringsAsFactors = FALSE
  )

  result <- app_env$get_likert_numeric_data(df, c("Q1", "Q2"))

  expect_equal(result$Q1, c(0, 3, 4))
  expect_equal(result$Q2, c(1, 4, 1))
})

test_that("build_statistics_bundle returns scaled summary and psychometric outputs", {
  items_df <- data.frame(
    Q1 = c(0, 1, 1, 2, 3, 3, 4, 4),
    Q2 = c(0, 1, 2, 2, 3, 3, 4, 4),
    Q3 = c(0, 0, 1, 2, 2, 3, 3, 4),
    stringsAsFactors = FALSE
  )
  scores <- c(10, 20, 25, 35, 45, 55, 70, 80)

  bundle <- app_env$build_statistics_bundle(scores = scores, items_df = items_df)

  expect_equal(bundle$scaled_summary$n, 8L)
  expect_true(is.finite(bundle$scaled_summary$mean))
  expect_equal(bundle$reliability$metric, c("Cronbach's alpha", "McDonald's omega"))
  expect_true(all(is.finite(bundle$reliability$value)))
  expect_equal(nrow(bundle$item_total), 3L)
  expect_true(all(is.finite(bundle$item_total$item_total_correlation)))
})

test_that("build_statistics_bundle handles missing scaled scores", {
  items_df <- data.frame(
    Q1 = c(1, 2, 3, 4),
    Q2 = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  bundle <- app_env$build_statistics_bundle(scores = NULL, items_df = items_df)

  expect_equal(bundle$scaled_summary$n, 0L)
  expect_true(all(bundle$reliability$items == 2L))
  expect_equal(nrow(bundle$item_total), 2L)
})

test_that("calculate_ttest_pvalue_from_summary returns finite p-value", {
  x <- c(10, 12, 14, 16, 18, 20)

  p <- app_env$calculate_ttest_pvalue_from_summary(
    x = x,
    ref_mean = 15,
    ref_sd = 4,
    ref_n = 120
  )

  expect_true(is.finite(p))
  expect_gte(p, 0)
  expect_lte(p, 1)
})

test_that("calculate_ttest_pvalue_from_summary returns NA for invalid inputs", {
  p <- app_env$calculate_ttest_pvalue_from_summary(
    x = c(10),
    ref_mean = 15,
    ref_sd = 4,
    ref_n = 120
  )

  expect_true(is.na(p))
})
