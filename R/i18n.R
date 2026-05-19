i18n_default_language <- "sv"
i18n_supported_languages <- c("sv", "en")

i18n_locale_path <- file.path("data", "locales.yml")

i18n_resolve_locale_path <- function(path = i18n_locale_path) {
  candidates <- c(path)

  for (idx in seq_len(sys.nframe())) {
    frame <- sys.frame(idx)
    ofile <- frame$ofile
    if (is.null(ofile)) {
      next
    }

    script_dir <- dirname(normalizePath(ofile, winslash = "/", mustWork = FALSE))
    repo_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
    candidates <- c(
      candidates,
      file.path(script_dir, path),
      file.path(repo_root, path)
    )
  }

  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) {
    return(path)
  }

  existing[[1]]
}

i18n_load_locales <- function(path = i18n_locale_path) {
  resolved_path <- i18n_resolve_locale_path(path)

  if (!file.exists(resolved_path)) {
    stop(sprintf("Locale file not found: %s", resolved_path), call. = FALSE)
  }

  locales <- yaml::read_yaml(resolved_path)
  if (!is.list(locales) || !length(locales)) {
    stop("Locale file is empty or malformed.", call. = FALSE)
  }

  missing_langs <- setdiff(i18n_supported_languages, names(locales))
  if (length(missing_langs)) {
    stop(
      sprintf(
        "Locale file is missing supported languages: %s",
        paste(missing_langs, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  locales
}

i18n_locales <- i18n_load_locales()

i18n_normalize_language <- function(lang) {
  if (!is.character(lang) || !length(lang) || is.na(lang)) {
    return(i18n_default_language)
  }

  normalized <- tolower(trimws(lang[[1]]))
  if (!(normalized %in% i18n_supported_languages)) {
    return(i18n_default_language)
  }

  normalized
}

i18n_t <- function(lang, key, ...) {
  lang <- i18n_normalize_language(lang)

  template <- i18n_locales[[lang]][[key]]
  if (is.null(template)) {
    template <- i18n_locales[[i18n_default_language]][[key]]
  }

  if (is.null(template)) {
    return(key)
  }

  dots <- list(...)
  if (!length(dots)) {
    return(template)
  }

  do.call(sprintf, c(list(template), dots))
}

i18n_t_markdown <- function(lang, key, ...) {
  text <- i18n_t(lang, key, ...)
  htmltools::HTML(commonmark::markdown_html(text))
}

i18n_format_number <- function(x, lang, digits = 2) {
  lang <- i18n_normalize_language(lang)
  
  # Format the number with the specified number of digits
  formatted <- format(round(x, digits), nsmall = digits, trim = TRUE)
  
  # For Swedish, replace period with comma as decimal separator
  if (lang == "sv") {
    formatted <- gsub("\\.", ",", formatted)
  }
  
  formatted
}