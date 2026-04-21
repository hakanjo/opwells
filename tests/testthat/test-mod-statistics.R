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

count_statistics_group_selectors <- function(group_controls_ui) {
  html <- paste(as.character(group_controls_ui), collapse = "")
  matches <- regmatches(html, gregexpr("group_[0-9]+", html, perl = TRUE))[[1]]
  matches <- matches[matches != ""]
  length(unique(matches))
}

test_that("statistics module renders total and grouped statistics", {
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

      total_html <- paste(as.character(output$total_statistics_ui), collapse = "")
      expect_true(grepl("Totalt", total_html, fixed = TRUE))
      expect_true(grepl("Cronbach's alpha", total_html, fixed = TRUE))

      session$setInputs(group_col = "grp")
      session$flushReact()

      expect_equal(count_statistics_group_selectors(output$group_controls_ui), 2L)

      grouped_html <- paste(as.character(output$grouped_statistics_ui), collapse = "")
      expect_true(grepl("Per grupp", grouped_html, fixed = TRUE))
      expect_true(grepl(">A<", grouped_html))
      expect_true(grepl(">B<", grouped_html))
    }
  )
})

test_that("statistics module shows guidance before likert selection", {
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

      total_html <- paste(as.character(output$total_statistics_ui), collapse = "")
      expect_true(grepl("Välj Likert-kolumner", total_html, fixed = TRUE))
    }
  )
})