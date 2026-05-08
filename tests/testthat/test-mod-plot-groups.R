library(shiny)

make_mod_plot_env <- function() {
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

test_that("plot_normalize_group_selection keeps valid defaults", {
  mod_env <- make_mod_plot_env()

  expect_equal(
    mod_env$plot_normalize_group_selection(character(0), c("B", "A", "C"), target_n = 2L),
    c("B", "A")
  )
  expect_equal(
    mod_env$plot_normalize_group_selection(c("A", "A", "Z"), c("A", "B", "C"), target_n = 2L),
    c("A", "B")
  )
})

test_that("plot_parse_group_definitions converts selections to labelled defs", {
  mod_env <- make_mod_plot_env()

  defs <- mod_env$plot_parse_group_definitions(list(sex = c("Kvinna", "Man"), age = "65-70"))
  expect_length(defs, 3L)
  expect_equal(defs[[1]]$col, "sex")
  expect_equal(defs[[1]]$value, "Kvinna")
  expect_equal(defs[[1]]$label, "Kvinna")
  expect_equal(defs[[3]]$label, "65-70")

  # values shared across columns are disambiguated
  defs2 <- mod_env$plot_parse_group_definitions(list(col1 = "X", col2 = "X"))
  expect_equal(defs2[[1]]$label, "col1: X")
  expect_equal(defs2[[2]]$label, "col2: X")
})

test_that("plot_build_combined_payload assembles user, raw, and reference layers", {
  mod_env <- make_mod_plot_env()

  user_df <- data.frame(
    grp = c("A", "A", "B", "B"),
    age_group = c("old", "old", "older", "older"),
    stringsAsFactors = FALSE
  )
  ref_df <- data.frame(
    group = c("A", "B"),
    mean = c(55, 62),
    sd = c(8, 9),
    q_1_6 = c(47, 54),
    q_5_6 = c(63, 70),
    stringsAsFactors = FALSE
  )

  payload <- mod_env$plot_build_combined_payload(
    user_df = user_df,
    scores = c(10, 20, 30, 40),
    group_definitions = mod_env$plot_parse_group_definitions(list(grp = c("A", "B"))),
    ref_df = ref_df,
    layers = c("user", "reference", "raw")
  )

  expect_equal(payload$ordered_groups, c("A", "B"))
  expect_equal(nrow(payload$user_summary), 2L)
  expect_equal(nrow(payload$ref_summary), 2L)
  expect_equal(nrow(payload$user_raw), 4L)
  expect_true(all(c("hover_text", "row_id", "score_value") %in% names(payload$user_raw)))
  expect_match(payload$user_raw$hover_text[1], "Rad: 1")
})

test_that("plot_build_combined_payload reports missing reference groups gracefully", {
  mod_env <- make_mod_plot_env()

  user_df <- data.frame(
    grp = c("A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )
  ref_df <- data.frame(
    group = c("A"),
    mean = c(55),
    sd = c(8),
    q_1_6 = c(47),
    q_5_6 = c(63),
    stringsAsFactors = FALSE
  )

  payload <- mod_env$plot_build_combined_payload(
    user_df = user_df,
    scores = c(10, 20, 30, 40),
    group_definitions = mod_env$plot_parse_group_definitions(list(grp = c("A", "B"))),
    ref_df = ref_df,
    layers = c("user", "reference")
  )

  expect_true(any(grepl("Referensdata saknas för: B", payload$status_messages, fixed = TRUE)))
})

test_that("plot_build_plotly_figure returns a plotly widget with traces", {
  skip_if_not_installed("plotly")
  mod_env <- make_mod_plot_env()

  payload <- mod_env$plot_build_combined_payload(
    user_df = data.frame(grp = c("A", "A", "B"), stringsAsFactors = FALSE),
    scores = c(20, 35, 60),
    group_definitions = mod_env$plot_parse_group_definitions(list(grp = c("A", "B"))),
    ref_df = data.frame(
      group = c("A", "B"),
      mean = c(55, 62),
      sd = c(8, 9),
      q_1_6 = c(47, 54),
      q_5_6 = c(63, 70),
      stringsAsFactors = FALSE
    ),
    layers = c("user", "reference", "raw")
  )

  fig <- mod_env$plot_build_plotly_figure(payload)
  built_fig <- plotly::plotly_build(fig)

  expect_s3_class(fig, "plotly")
  expect_gte(length(built_fig$x$data), 5L)
})
