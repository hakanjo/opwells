convert_likert_value <- function(x) {
  value_chr <- trimws(as.character(x))

  swedish_map <- c(
    "aldrig" = "0",
    "sällan" = "1",
    "ibland" = "2",
    "ofta" = "3",
    "alltid" = "4"
  )

  mapped_values <- unname(swedish_map[tolower(value_chr)])
  matched <- !is.na(mapped_values)

  value_chr[matched] <- mapped_values[matched]

  suppressWarnings(as.numeric(value_chr))
}

get_likert_numeric_data <- function(df, likert_cols = NULL) {
  n <- nrow(df)

  if (is.null(likert_cols) || length(likert_cols) == 0) {
    return(data.frame(row.names = seq_len(n)))
  }

  likert_cols <- likert_cols[likert_cols %in% names(df)]
  if (length(likert_cols) == 0) {
    return(data.frame(row.names = seq_len(n)))
  }

  likert_data <- df[, likert_cols, drop = FALSE]
  data.frame(
    lapply(likert_data, convert_likert_value),
    row.names = seq_len(n),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

compute_scaled_score <- function(df, likert_cols = NULL) {
  n <- nrow(df)

  if (is.null(likert_cols) || length(likert_cols) == 0) {
    return(rep(NA_real_, n))
  }

  likert_cols <- likert_cols[likert_cols %in% names(df)]
  if (length(likert_cols) == 0) {
    return(rep(NA_real_, n))
  }

  likert_numeric <- get_likert_numeric_data(df, likert_cols)

  raw_sums <- rowSums(likert_numeric, na.rm = TRUE)
  n_valid <- rowSums(!is.na(likert_numeric))
  # Adjust for missing items: proportionally scale to the full 0-40 range so
  # that, e.g., 16/20 (5 valid items) equals 32/40 (10 valid items) = 80%.
  unname(vapply(seq_len(nrow(df)), function(i) {
    if (n_valid[i] == 0L) return(NA_real_)
    adjusted_score <- raw_sums[i] / (n_valid[i] * 4) * 40
    scale(adjusted_score)
  }, numeric(1)))
}
