library(shiny)

make_mod_data_input_env <- function() {
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

  mod_env
}

test_that("data_input module auto-selects matching columns and computes scaled scores", {
  mod_env <- make_mod_data_input_env()

  user_df <- data.frame(
    Q1 = c("1", "2", "3"),
    Q2 = c("2", "3", "4"),
    Group = c("A", "B", "A"),
    stringsAsFactors = FALSE
  )

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(user_df, tmp, row.names = FALSE)

  testServer(
    mod_env$mod_data_input_server,
    {
      session$setInputs(upload_file = list(
        name = "test.csv",
        size = file.size(tmp),
        type = "text/csv",
        datapath = tmp
      ))
      session$flushReact()

      result <- session$returned
      state <- result$likert_state()

      expect_false(is.null(state$scaled_scores))
      expect_equal(sort(state$selected_columns), c("Q1", "Q2"))
    }
  )
})

test_that("data_input module excludes OPWELLS/F/Q columns without numeric suffix", {
  mod_env <- make_mod_data_input_env()

  user_df <- data.frame(
    OPWELLS_1 = c("1", "2", "3"),
    OPWELLS_score = c("10", "11", "12"),
    F_10 = c("2", "2", "3"),
    Q_3 = c("3", "4", "5"),
    Q_note = c("bad", "bad", "bad"),
    stringsAsFactors = FALSE
  )

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(user_df, tmp, row.names = FALSE)

  testServer(
    mod_env$mod_data_input_server,
    {
      session$setInputs(upload_file = list(
        name = "test.csv",
        size = file.size(tmp),
        type = "text/csv",
        datapath = tmp
      ))
      session$flushReact()

      selected <- session$returned$likert_state()$selected_columns

      expect_equal(sort(selected), c("F_10", "OPWELLS_1", "Q_3"))
      expect_false("OPWELLS_score" %in% selected)
      expect_false("Q_note" %in% selected)
    }
  )
})
