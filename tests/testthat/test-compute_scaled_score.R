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

  # 2 valid items per row, max = 8; adjusted score = raw / 8 * 40
  expect_length(result, 3)
  expect_true(all(!is.na(result)))
  expect_equal(result[1], app_env$scale(3 / 8 * 40), tolerance = 1e-6)
  expect_equal(result[2], app_env$scale(5 / 8 * 40), tolerance = 1e-6)
  expect_equal(result[3], app_env$scale(8 / 8 * 40), tolerance = 1e-6)
})

test_that("compute_scaled_score coerces non-numeric values and handles partial NA", {
  df <- data.frame(
    Q1 = c("a", "2", ""),
    Q2 = c("1", "b", "3"),
    stringsAsFactors = FALSE
  )

  result <- app_env$compute_scaled_score(df, c("Q1", "Q2"))

  # 1 valid item per row, max = 4; adjusted score = raw / 4 * 40
  expect_equal(result[1], app_env$scale(1 / 4 * 40), tolerance = 1e-6)
  expect_equal(result[2], app_env$scale(2 / 4 * 40), tolerance = 1e-6)
  expect_equal(result[3], app_env$scale(3 / 4 * 40), tolerance = 1e-6)
})

test_that("compute_scaled_score maps Swedish Likert labels to numeric values", {
  df <- data.frame(
    Q1 = c("Aldrig", " Sällan ", "IBLAND", "ofta", "Alltid"),
    stringsAsFactors = FALSE
  )

  result <- app_env$compute_scaled_score(df, "Q1")

  # 1 valid item per row, max = 4; adjusted score = raw / 4 * 40
  expect_equal(result, vapply(c(0, 10, 20, 30, 40), app_env$scale, numeric(1)), tolerance = 1e-6)
})

test_that("compute_scaled_score handles mixed Swedish labels and numeric strings", {
  df <- data.frame(
    Q1 = c("Aldrig", "Ofta", "2"),
    Q2 = c("4", " 1 ", "Alltid"),
    stringsAsFactors = FALSE
  )

  result <- app_env$compute_scaled_score(df, c("Q1", "Q2"))

  # 2 valid items per row, max = 8; adjusted score = raw / 8 * 40
  expect_equal(result[1], app_env$scale(4 / 8 * 40), tolerance = 1e-6)
  expect_equal(result[2], app_env$scale(4 / 8 * 40), tolerance = 1e-6)
  expect_equal(result[3], app_env$scale(6 / 8 * 40), tolerance = 1e-6)
})

test_that("compute_scaled_score ignores unknown Swedish-like labels as NA", {
  df <- data.frame(
    Q1 = c("Kanske", "Ofta"),
    Q2 = c("Alltid", "Vet ej"),
    stringsAsFactors = FALSE
  )

  result <- app_env$compute_scaled_score(df, c("Q1", "Q2"))

  # 1 valid item per row, max = 4; adjusted score = raw / 4 * 40
  expect_equal(result[1], app_env$scale(4 / 4 * 40), tolerance = 1e-6)
  expect_equal(result[2], app_env$scale(3 / 4 * 40), tolerance = 1e-6)
})

test_that("compute_scaled_score proportionally adjusts for missing values", {
  # 16 out of 20 possible (5 valid items) = 80%, same as 32/40 (10 items)
  df_partial <- data.frame(
    Q1 = c("4", "4"),
    Q2 = c("4", "4"),
    Q3 = c("4", "4"),
    Q4 = c("4", "4"),
    Q5 = c("4", "NA"),
    Q6 = c("-", "na"),
    Q7 = c("", ""),
    Q8 = c("NA", "NA"),
    Q9 = c("-", "-"),
    Q10 = c("na", "na"),
    stringsAsFactors = FALSE
  )

  result_partial <- app_env$compute_scaled_score(
    df_partial,
    c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10")
  )

  # Row 1: 5 valid items each = 4, sum = 20, adjusted = 20/20 * 40 = 40
  expect_equal(result_partial[1], app_env$scale(40), tolerance = 1e-6)

  # Row 2: 4 valid items each = 4, sum = 16, adjusted = 16/16 * 40 = 40
  expect_equal(result_partial[2], app_env$scale(40), tolerance = 1e-6)

  # 16/20 (5 items) should produce the same score as 32/40 (10 items) = 80%
  df_full <- data.frame(
    lapply(1:10, function(i) rep("4", 1)),
    stringsAsFactors = FALSE
  )
  names(df_full) <- paste0("Q", 1:10)
  df_full$Q1 <- "3"; df_full$Q2 <- "3"; df_full$Q3 <- "2"; df_full$Q4 <- "2"
  df_full$Q5 <- "2"; df_full$Q6 <- "2"; df_full$Q7 <- "2"; df_full$Q8 <- "2"
  df_full$Q9 <- "2"; df_full$Q10 <- "2"  # sum = 22 out of 40

  df_half <- df_full[, 1:5, drop = FALSE]
  df_half$Q5 <- "-"; df_half$Q4 <- "-"; df_half$Q3 <- "-"  # leave Q1=3, Q2=3 valid

  result_full  <- app_env$compute_scaled_score(df_full,  paste0("Q", 1:10))
  result_half  <- app_env$compute_scaled_score(df_half,  paste0("Q", 1:5))
  # Q1=3, Q2=3: sum=6, n_valid=2, adjusted=6/8*40=30
  # full: sum=22, n_valid=10, adjusted=22/40*40=22 -> different proportion
  # Verify the proportional result for the half case explicitly
  expect_equal(result_half[1], app_env$scale(6 / 8 * 40), tolerance = 1e-6)
})

# --- Proportional scaling equivalence ---

test_that("same proportion of max yields the same scaled score regardless of item count", {
  # 80% of max with 10 items: 32/40
  df_10 <- data.frame(
    lapply(1:10, function(i) "4"),
    stringsAsFactors = FALSE
  )
  names(df_10) <- paste0("Q", 1:10)
  # Override two items to get sum 32 (8 items * 4 = 32, 2 items * 0 but set to 0 by missing)
  # Easier: all 4s except two items NA -> 8 valid * 4 = 32 -> 32/32*40 = 40, not 80%.
  # Use explicit values: 10 items scoring 3.2 each can't be done with integers.
  # Best approach: 8 items at 4 and 2 items at 0 => 32 valid out of 8 items max... no.
  # Let's use: 10 items, each = 4 except two = 0: sum=32, n_valid=10, adjusted=32/40*40=32.
  df_10$Q9 <- "0"; df_10$Q10 <- "0"  # sum = 32 out of 40

  # 80% of max with 5 items: 5 items * 4 = 20 max; 80% = 16; use 4 items=4, 1 item=0
  df_5 <- data.frame(Q1="4", Q2="4", Q3="4", Q4="4", Q5="0", stringsAsFactors=FALSE)

  # 80% of max with 2 items: 2 items * 4 = 8 max; 80% = 6.4 (non-integer, skip)
  # Use 5 items where 3 are missing: effective 2 items both at 4 => 8/8*40=40 (100%, skip)

  score_10 <- app_env$compute_scaled_score(df_10, paste0("Q", 1:10))
  score_5  <- app_env$compute_scaled_score(df_5,  c("Q1", "Q2", "Q3", "Q4", "Q5"))

  expect_equal(score_10[1], app_env$scale(32), tolerance = 1e-6)
  expect_equal(score_5[1],  app_env$scale(32), tolerance = 1e-6)
  expect_equal(score_10[1], score_5[1], tolerance = 1e-6)
})

test_that("32/40 (10 items) and 16/20 (5 items) produce identical scaled scores", {
  # Full set: 10 items, 8 score 4 and 2 score 0 -> sum=32
  df_full <- data.frame(
    Q1="4", Q2="4", Q3="4", Q4="4", Q5="4",
    Q6="4", Q7="4", Q8="4", Q9="0", Q10="0",
    stringsAsFactors = FALSE
  )

  # Half set: 5 items, 4 score 4 and 1 scores 0 -> sum=16; 5 missing items irrelevant
  df_half <- data.frame(
    Q1="4", Q2="4", Q3="4", Q4="4", Q5="0",
    Q6="-", Q7="NA", Q8="na", Q9="", Q10="-",
    stringsAsFactors = FALSE
  )

  score_full <- app_env$compute_scaled_score(df_full, paste0("Q", 1:10))
  score_half <- app_env$compute_scaled_score(df_half, paste0("Q", 1:10))

  expect_equal(score_full[1], app_env$scale(32), tolerance = 1e-6)
  expect_equal(score_half[1], app_env$scale(32), tolerance = 1e-6)
  expect_equal(score_full[1], score_half[1], tolerance = 1e-6)
})

test_that("all-missing row returns NA", {
  df <- data.frame(
    Q1 = c("4", "NA", "-"),
    Q2 = c("2", "",   "na"),
    stringsAsFactors = FALSE
  )

  result <- app_env$compute_scaled_score(df, c("Q1", "Q2"))

  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_true(is.na(result[3]))
})

test_that("missing value tokens (NA, na, -, blank) are excluded from item count", {
  # One item with value 2 plus four missing tokens -> should equal scale(2/4*40) = scale(20)
  tokens <- c("NA", "na", "-", "")
  for (token in tokens) {
    df <- data.frame(Q1 = "2", Q2 = token, stringsAsFactors = FALSE)
    result <- app_env$compute_scaled_score(df, c("Q1", "Q2"))
    expect_equal(result[1], app_env$scale(20), tolerance = 1e-6,
                 label = paste0("token: '", token, "'"))
  }
})

test_that("score increases monotonically with proportion of max across item counts", {
  make_row <- function(n_items, sum_val) {
    # n_items valid items, each 0 except first = sum_val (assumes sum_val <= 4)
    vals <- c(as.character(sum_val), rep("0", n_items - 1))
    df <- as.data.frame(
      setNames(as.list(vals), paste0("Q", seq_len(n_items))),
      stringsAsFactors = FALSE
    )
    app_env$compute_scaled_score(df, paste0("Q", seq_len(n_items)))[1]
  }

  # Proportions: 0/4 < 1/4 < 2/4 < 3/4 < 4/4 for 1 item
  scores <- vapply(0:4, function(v) make_row(1, v), numeric(1))
  expect_true(all(diff(scores) > 0))

  # Same monotonicity with 5 items (varying first item 0-4, rest 0)
  scores5 <- vapply(0:4, function(v) make_row(5, v), numeric(1))
  expect_true(all(diff(scores5) > 0))

  # Equal proportions across different item counts give same score
  # proportion 0.5: 2 out of 4 (1 item) vs 10 out of 20 (5 items)
  s1 <- make_row(1, 2)   # 2/4 = 50% -> scale(20)
  s5 <- make_row(5, 2)   # 2/20 = 10%... no: sum=2, n=5, max=20, adj=2/20*40=4
  # These differ in proportion, so compare explicitly:
  expect_equal(s1, app_env$scale(2 / 4  * 40), tolerance = 1e-6)
  expect_equal(s5, app_env$scale(2 / 20 * 40), tolerance = 1e-6)
})
