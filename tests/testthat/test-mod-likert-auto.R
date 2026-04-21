library(shiny)

make_mod_likert_env <- function(notification_log) {
  mod_env <- new.env(parent = globalenv())

  file_list <- list.files(
    path = file.path(repo_root, "R"),
    pattern = "\\.R$",
    recursive = TRUE,
    full.names = TRUE
  )

  for (file in file_list) {
    source(file, local = mod_env)
  }

  mod_env$showNotification <- function(ui, ...) {
    notification_log$messages <- c(notification_log$messages, as.character(ui))
    invisible(NULL)
  }

  mod_env
}

test_that("likert module auto-select computes scaled scores and warns when selected columns are not 10", {
  notification_log <- new.env(parent = emptyenv())
  notification_log$messages <- character(0)

  mod_env <- make_mod_likert_env(notification_log)

  user_df <- data.frame(
    Q1 = c("1", "2", "3"),
    Q2 = c("2", "3", "4"),
    Group = c("A", "B", "A"),
    stringsAsFactors = FALSE
  )

  testServer(
    mod_env$mod_likert_server,
    args = list(
      data = reactive(user_df),
      active_tab = reactive("likert")
    ),
    {
      session$flushReact()

      preview_html <- paste(as.character(output$likert_preview_ui), collapse = "")
      expect_true(grepl("Skalad poäng", preview_html, fixed = TRUE))
    }
  )

  expect_true(any(grepl("Exakt 10 kolumner rekommenderas.", notification_log$messages, fixed = TRUE)))
})
