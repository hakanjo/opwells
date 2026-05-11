library(shiny)

make_mod_statistics_env <- function(notification_log) {
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

test_that("statistics module renders category summary and pairwise comparisons", {
  notification_log <- new.env(parent = emptyenv())
  notification_log$messages <- character(0)

  mod_env <- make_mod_statistics_env(notification_log)

  user_df <- data.frame(
    grp = c("A", "B", "A", "B", "A", "B"),
    Q1 = c("1", "2", "2", "3", "3", "4"),
    Q2 = c("1", "2", "2", "3", "3", "4"),
    stringsAsFactors = FALSE
  )

  likert_state <- list(
    scaled_scores = c(10, 20, 30, 40, 50, 60),
    selected_columns = c("Q1", "Q2"),
    numeric_items = data.frame(Q1 = c(1, 2, 2, 3, 3, 4), Q2 = c(1, 2, 2, 3, 3, 4), stringsAsFactors = FALSE)
  )

  testServer(
    mod_env$mod_statistics_server,
    args = list(
      data = reactive(user_df),
      likert_state = reactive(likert_state),
      active_tab = reactive("statistics")
    ),
    {
      session$flushReact()

      summary_html <- paste(as.character(output$summary_statistics_ui), collapse = "")
      expect_true(grepl("Välj en kategorikolumn", summary_html, fixed = TRUE))

      session$setInputs(group_col = "grp")
      session$flushReact()

      session$setInputs(selected_groups = c("A", "B"))
      session$flushReact()

      summary_html <- paste(as.character(output$summary_statistics_ui), collapse = "")
      expect_true(grepl("Kategori", summary_html, fixed = TRUE))
      expect_true(grepl(">A<", summary_html))
      expect_true(grepl(">B<", summary_html))
      expect_true(grepl("2/3 nedre", summary_html, fixed = TRUE))

      pairwise_html <- paste(as.character(output$pairwise_statistics_ui), collapse = "")
      expect_true(grepl("Grupp 1", pairwise_html, fixed = TRUE))
      expect_true(grepl("Skillnad i medel", pairwise_html, fixed = TRUE))
      expect_true(grepl("Skillnad i median", pairwise_html, fixed = TRUE))
      expect_false(grepl("p-värde", pairwise_html, fixed = TRUE))
    }
  )
})

test_that("statistics module shows guidance before scaled scores exist", {
  notification_log <- new.env(parent = emptyenv())
  notification_log$messages <- character(0)

  mod_env <- make_mod_statistics_env(notification_log)

  user_df <- data.frame(
    grp = c("A", "B"),
    value = c(1, 2),
    stringsAsFactors = FALSE
  )

  testServer(
    mod_env$mod_statistics_server,
    args = list(
      data = reactive(user_df),
      likert_state = reactive(list(scaled_scores = NULL, selected_columns = character(0), numeric_items = data.frame())),
      active_tab = reactive("statistics")
    ),
    {
      session$flushReact()

      summary_html <- paste(as.character(output$summary_statistics_ui), collapse = "")
      expect_true(grepl("Skalad poäng saknas", summary_html, fixed = TRUE))

      pairwise_html <- paste(as.character(output$pairwise_statistics_ui), collapse = "")
      expect_true(grepl("Skalad poäng saknas", pairwise_html, fixed = TRUE))
    }
  )
})