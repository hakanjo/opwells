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
  # Raw sums should be 0-40 (10 questions * 4 points max)
  unname(vapply(raw_sums, function(score) {
    if (is.na(score)) return(NA_real_)
    scale(score)
  }, numeric(1)))
}
