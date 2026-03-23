compute_scaled_score <- function(df, likert_cols = NULL) {
  n <- nrow(df)

  if (is.null(likert_cols) || length(likert_cols) == 0) {
    return(rep(NA_real_, n))
  }

  likert_cols <- likert_cols[likert_cols %in% names(df)]
  if (length(likert_cols) == 0) {
    return(rep(NA_real_, n))
  }

  likert_data <- df[, likert_cols, drop = FALSE]

  likert_numeric <- data.frame(lapply(likert_data, function(x) {
    suppressWarnings(as.numeric(x))
  }), stringsAsFactors = FALSE)

  raw_sums <- rowSums(likert_numeric, na.rm = TRUE)

  # Raw sums should be 0-40 (10 questions * 4 points max)
  vapply(raw_sums, function(score) {
    if (is.na(score)) return(NA_real_)
    scale(score)
  }, numeric(1))
}
