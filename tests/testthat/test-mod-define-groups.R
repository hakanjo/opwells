library(shiny)

make_mod_define_groups_env <- function(notification_log) {
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

test_that("define groups module can add and remove combined groups in shared state", {
  notification_log <- new.env(parent = emptyenv())
  notification_log$messages <- character(0)

  mod_env <- make_mod_define_groups_env(notification_log)

  user_df <- data.frame(
    grp = c("A", "B", "A"),
    Q1 = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )

  shared_group_state <- shiny::reactiveValues(
    selections = list(),
    combined_groups = list(),
    include_total = FALSE,
    reference_groups = character(0)
  )

  testServer(
    mod_env$mod_define_groups_server,
    args = list(
      data = reactive(user_df),
      likert_state = reactive(list(selected_columns = "Q1")),
      group_state = shared_group_state,
      lang = reactive("sv")
    ),
    {
      session$flushReact()

      session$setInputs(grp_grp = "A")
      session$setInputs(combined_group_label = "Grupp A")
      session$setInputs(add_combined_group = 1)
      session$flushReact()

      expect_length(shared_group_state$combined_groups, 1L)
      expect_equal(shared_group_state$combined_groups[[1]]$label, "Grupp A")
      expect_true(isTRUE(shared_group_state$combined_groups[[1]]$is_combined))
      expect_equal(shared_group_state$combined_groups[[1]]$source_defs[[1]]$col, "grp")
      expect_equal(shared_group_state$combined_groups[[1]]$source_defs[[1]]$value, "A")

      session$setInputs(remove_combined_group_Grupp_A = 1)
      session$flushReact()

      expect_length(shared_group_state$combined_groups, 0L)
      expect_true(any(grepl("skapad", notification_log$messages, fixed = TRUE)))
      expect_true(any(grepl("borttagen", notification_log$messages, fixed = TRUE)))
    }
  )
})
