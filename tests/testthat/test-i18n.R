extract_i18n_keys <- function(file_path) {
  text <- paste(readLines(file_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  extract_with_pattern <- function(pattern) {
    matches <- gregexpr(pattern, text, perl = TRUE)
    found <- regmatches(text, matches)[[1]]
    if (!length(found)) {
      return(character(0))
    }

    sub(pattern, "\\1", found, perl = TRUE)
  }

  keys_from_tr <- extract_with_pattern('tr\\(\\s*"([^"]+)"')
  keys_from_i18n_t <- extract_with_pattern('i18n_t\\([^\\)]*"([^"]+)"')

  unique(c(keys_from_tr, keys_from_i18n_t))
}

test_that("i18n locales keep same keys across languages", {
  locales <- app_env$i18n_locales

  expect_true(all(c("sv", "en") %in% names(locales)))

  sv_keys <- sort(names(locales$sv))
  en_keys <- sort(names(locales$en))

  missing_in_en <- setdiff(sv_keys, en_keys)
  missing_in_sv <- setdiff(en_keys, sv_keys)

  expect(
    length(missing_in_en) == 0,
    paste("Missing English keys:", paste(missing_in_en, collapse = ", "))
  )

  expect(
    length(missing_in_sv) == 0,
    paste("Missing Swedish keys:", paste(missing_in_sv, collapse = ", "))
  )
})

test_that("all referenced i18n keys exist in both locales", {
  scan_files <- c(
    file.path(repo_root, "app.R"),
    list.files(file.path(repo_root, "R"), pattern = "\\.R$", full.names = TRUE)
  )

  referenced_keys <- unique(unlist(lapply(scan_files, extract_i18n_keys), use.names = FALSE))

  # Keep only dot-qualified locale keys like app.title or plot.ui.combine.add.
  referenced_keys <- sort(referenced_keys[grepl("^[a-z0-9_]+\\.[a-z0-9_.]+$", referenced_keys)])

  sv_keys <- names(app_env$i18n_locales$sv)
  en_keys <- names(app_env$i18n_locales$en)

  missing_in_sv <- setdiff(referenced_keys, sv_keys)
  missing_in_en <- setdiff(referenced_keys, en_keys)

  expect(
    length(missing_in_sv) == 0,
    paste("Referenced keys missing in Swedish locale:", paste(missing_in_sv, collapse = ", "))
  )

  expect(
    length(missing_in_en) == 0,
    paste("Referenced keys missing in English locale:", paste(missing_in_en, collapse = ", "))
  )
})
