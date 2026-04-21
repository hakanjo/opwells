format_stat_number <- function(x, digits = 2) {
  ifelse(is.na(x), "", format(round(x, digits), nsmall = digits, trim = TRUE))
}

summarize_scaled_scores <- function(scores) {
  x <- suppressWarnings(as.numeric(scores))
  x <- x[!is.na(x)]

  if (!length(x)) {
    return(data.frame(
      n = 0L,
      mean = NA_real_,
      sd = NA_real_,
      range_2_3_low = NA_real_,
      range_2_3_high = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    n = length(x),
    mean = mean(x),
    sd = stats::sd(x),
    range_2_3_low = as.numeric(stats::quantile(x, probs = 1 / 6, na.rm = TRUE, names = FALSE)),
    range_2_3_high = as.numeric(stats::quantile(x, probs = 5 / 6, na.rm = TRUE, names = FALSE)),
    stringsAsFactors = FALSE
  )
}

prepare_psychometric_items <- function(items_df) {
  if (!is.data.frame(items_df) || ncol(items_df) == 0 || nrow(items_df) == 0) {
    return(NULL)
  }

  keep_cols <- vapply(items_df, function(col) {
    col_num <- suppressWarnings(as.numeric(col))
    any(!is.na(col_num)) && stats::sd(col_num, na.rm = TRUE) > 0
  }, logical(1))

  filtered <- items_df[, keep_cols, drop = FALSE]
  if (ncol(filtered) < 2) {
    return(NULL)
  }

  complete_rows <- stats::complete.cases(filtered)
  filtered <- filtered[complete_rows, , drop = FALSE]
  if (nrow(filtered) < 2) {
    return(NULL)
  }

  filtered
}

compute_cronbach_alpha <- function(items_df) {
  prepared <- prepare_psychometric_items(items_df)
  if (is.null(prepared)) {
    return(list(value = NA_real_, n = 0L, items = 0L))
  }

  item_vars <- vapply(prepared, stats::var, numeric(1))
  total_scores <- rowSums(prepared)
  total_var <- stats::var(total_scores)
  item_count <- ncol(prepared)

  if (!is.finite(total_var) || total_var <= 0 || item_count < 2) {
    return(list(value = NA_real_, n = nrow(prepared), items = item_count))
  }

  alpha <- (item_count / (item_count - 1)) * (1 - sum(item_vars) / total_var)
  list(value = alpha, n = nrow(prepared), items = item_count)
}

compute_mcdonald_omega <- function(items_df) {
  prepared <- prepare_psychometric_items(items_df)
  if (is.null(prepared)) {
    return(list(value = NA_real_, n = 0L, items = 0L))
  }

  cor_mat <- stats::cor(prepared)
  fit <- tryCatch(
    stats::factanal(covmat = cor_mat, factors = 1, rotation = "none", n.obs = nrow(prepared)),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(list(value = NA_real_, n = nrow(prepared), items = ncol(prepared)))
  }

  loadings <- as.numeric(fit$loadings[, 1])
  uniquenesses <- as.numeric(fit$uniquenesses)

  if (!length(loadings) || any(!is.finite(loadings)) || any(!is.finite(uniquenesses))) {
    return(list(value = NA_real_, n = nrow(prepared), items = ncol(prepared)))
  }

  omega_total <- (sum(loadings) ^ 2) / ((sum(loadings) ^ 2) + sum(uniquenesses))
  list(value = omega_total, n = nrow(prepared), items = ncol(prepared))
}

compute_item_total_correlations <- function(items_df) {
  prepared <- prepare_psychometric_items(items_df)
  if (is.null(prepared)) {
    return(data.frame(
      item = character(0),
      item_total_correlation = numeric(0),
      n = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  correlations <- lapply(seq_along(prepared), function(idx) {
    item_name <- names(prepared)[idx]
    item_values <- prepared[[idx]]
    other_items <- prepared[, -idx, drop = FALSE]

    total_without_item <- rowSums(other_items)
    corr <- stats::cor(item_values, total_without_item)

    data.frame(
      item = item_name,
      item_total_correlation = as.numeric(corr),
      n = nrow(prepared),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, correlations)
}

build_statistics_bundle <- function(scores = NULL, items_df = NULL) {
  alpha <- compute_cronbach_alpha(items_df)
  omega <- compute_mcdonald_omega(items_df)

  list(
    scaled_summary = summarize_scaled_scores(scores),
    reliability = data.frame(
      metric = c("Cronbach's alpha", "McDonald's omega"),
      value = c(alpha$value, omega$value),
      n = c(alpha$n, omega$n),
      items = c(alpha$items, omega$items),
      stringsAsFactors = FALSE
    ),
    item_total = compute_item_total_correlations(items_df)
  )
}

format_statistics_for_display <- function(stats_bundle) {
  scaled_summary <- stats_bundle$scaled_summary
  if (nrow(scaled_summary) > 0) {
    scaled_summary$mean <- format_stat_number(scaled_summary$mean)
    scaled_summary$sd <- format_stat_number(scaled_summary$sd)
    scaled_summary$range_2_3_low <- format_stat_number(scaled_summary$range_2_3_low)
    scaled_summary$range_2_3_high <- format_stat_number(scaled_summary$range_2_3_high)
  }

  reliability <- stats_bundle$reliability
  if (nrow(reliability) > 0) {
    reliability$value <- format_stat_number(reliability$value, digits = 3)
  }

  item_total <- stats_bundle$item_total
  if (nrow(item_total) > 0) {
    item_total$item_total_correlation <- format_stat_number(item_total$item_total_correlation, digits = 3)
  }

  list(
    scaled_summary = scaled_summary,
    reliability = reliability,
    item_total = item_total
  )
}