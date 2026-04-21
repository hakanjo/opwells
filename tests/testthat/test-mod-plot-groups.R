library(shiny)

make_mod_plot_env <- function(notification_log) {
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

  # Keep reference tab observers stable in tests.
  mod_env$load_ref_data <- function() {
    data.frame(
      group = c("Ref A", "Ref B"),
      mean = c(50, 60),
      sd = c(10, 12),
      q_1_6 = c(40, 50),
      q_5_6 = c(60, 70),
      stringsAsFactors = FALSE
    )
  }

  mod_env$showNotification <- function(ui, ...) {
    notification_log$messages <- c(notification_log$messages, as.character(ui))
    invisible(NULL)
  }

  mod_env
}

count_user_group_selectors <- function(user_groups_ui) {
  html <- paste(as.character(user_groups_ui), collapse = "")
  matches <- regmatches(html, gregexpr("user_group_[0-9]+", html, perl = TRUE))[[1]]
  if (!length(matches)) {
    return(0L)
  }

  length(unique(matches))
}

test_that("adding beyond available user groups keeps selection count and warns", {
  notification_log <- new.env(parent = emptyenv())
  notification_log$messages <- character(0)

  mod_env <- make_mod_plot_env(notification_log)

  user_df <- data.frame(
    grp = c("A", "B", "A"),
    value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  testServer(
    mod_env$mod_plot_server,
    args = list(data = reactive(user_df), scores = reactive(c(10, 20, 30))),
    {
      session$setInputs(group_col = "grp")
      session$flushReact()

      expect_equal(count_user_group_selectors(output$user_groups_ui), 2L)

      session$setInputs(add_user_group = 1)
      session$flushReact()

      expect_equal(count_user_group_selectors(output$user_groups_ui), 2L)
    }
  )

  expect_true(any(grepl("Alla tillgängliga grupper är redan valda.", notification_log$messages, fixed = TRUE)))
})

test_that("removing with one user group left keeps selection and warns", {
  notification_log <- new.env(parent = emptyenv())
  notification_log$messages <- character(0)

  mod_env <- make_mod_plot_env(notification_log)

  user_df <- data.frame(
    grp = c("A", "A", "A"),
    value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  testServer(
    mod_env$mod_plot_server,
    args = list(data = reactive(user_df), scores = reactive(c(10, 20, 30))),
    {
      session$setInputs(group_col = "grp")
      session$flushReact()

      expect_equal(count_user_group_selectors(output$user_groups_ui), 1L)

      session$setInputs(remove_user_group_1 = 1)
      session$flushReact()

      expect_equal(count_user_group_selectors(output$user_groups_ui), 1L)
    }
  )

  expect_true(any(grepl("Minst en grupp måste vara vald.", notification_log$messages, fixed = TRUE)))
})
