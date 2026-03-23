compute_scaled_score <- function(df, likert_cols = NULL) {
  # If no columns specified, return df unchanged
  if (is.null(likert_cols) || length(likert_cols) == 0) {
    return(df)
  }
  
  # Ensure likert_cols are valid column names in df
  likert_cols <- likert_cols[likert_cols %in% names(df)]
  if (length(likert_cols) == 0) {
    return(df)
  }
  
  # Extract likert responses as numeric
  likert_data <- df[, likert_cols, drop = FALSE]
  
  # Convert to numeric, coercing non-numeric to NA
  likert_numeric <- data.frame(lapply(likert_data, function(x) {
    suppressWarnings(as.numeric(x))
  }), stringsAsFactors = FALSE)
  
  # Sum per row, excluding NA values
  raw_sums <- rowSums(likert_numeric, na.rm = TRUE)
  
  # Apply scale function to each sum
  # Raw sums should be 0-40 (10 questions * 4 points max)
  scaled_scores <- sapply(raw_sums, function(score) {
    if (is.na(score)) {
      return(NA_real_)
    }
    scale(score)
  })
  
  # Add scaled score column to df
  df$scaled_score <- scaled_scores
  
  return(df)
}
