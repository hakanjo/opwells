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

test_that("plot_layers_from_selection toggles between reference and user layers", {
  mod_env <- make_mod_plot_env()

  expect_equal(mod_env$plot_layers_from_selection(list(), show_raw = FALSE), "reference")

  defs <- mod_env$plot_parse_group_definitions(list(grp = "A"))
  expect_equal(mod_env$plot_layers_from_selection(defs, show_raw = FALSE), "user")
  expect_equal(mod_env$plot_layers_from_selection(defs, show_raw = TRUE), c("user", "raw"))
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

test_that("plot_build_user_data can include a total user row", {
  mod_env <- make_mod_plot_env()

  user_df <- data.frame(
    grp = c("A", "A", "B", "B"),
    stringsAsFactors = FALSE
  )

  defs <- c(
    list(mod_env$plot_total_group_definition()),
    mod_env$plot_parse_group_definitions(list(grp = c("A", "B")))
  )

  user_data <- mod_env$plot_build_user_data(
    user_df = user_df,
    scores = c(10, 20, 30, 40),
    group_definitions = defs
  )

  expect_equal(user_data$summary$group_label, c("Totalt", "A", "B"))
  expect_equal(user_data$summary$n, c(4L, 2L, 2L))
  expect_true(isTRUE(user_data$summary$is_total[1]))
  expect_equal(sum(user_data$raw$group_label == "Totalt"), 4L)
})

test_that("plot_build_combined_payload keeps total on the top row", {
  mod_env <- make_mod_plot_env()

  user_df <- data.frame(
    grp = c("A", "A", "B", "B"),
    stringsAsFactors = FALSE
  )

  payload <- mod_env$plot_build_combined_payload(
    user_df = user_df,
    scores = c(10, 20, 30, 40),
    group_definitions = c(
      list(mod_env$plot_total_group_definition()),
      mod_env$plot_parse_group_definitions(list(grp = c("B", "A")))
    ),
    ref_df = data.frame(),
    layers = c("user", "raw")
  )

  expect_equal(payload$ordered_groups, c("Totalt", "B", "A"))
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

test_that("plot_build_combined_payload returns reference data when no groups are selected", {
  mod_env <- make_mod_plot_env()

  payload <- mod_env$plot_build_combined_payload(
    user_df = data.frame(grp = c("A", "B"), stringsAsFactors = FALSE),
    scores = c(10, 20),
    group_definitions = list(),
    ref_df = data.frame(
      group = c("A", "B"),
      mean = c(55, 62),
      sd = c(8, 9),
      q_1_6 = c(47, 54),
      q_5_6 = c(63, 70),
      stringsAsFactors = FALSE
    ),
    layers = "reference"
  )

  expect_equal(nrow(payload$ref_summary), 2L)
  expect_equal(nrow(payload$user_summary), 0L)
  expect_equal(nrow(payload$user_raw), 0L)
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

test_that("plot_build_plotly_figure centers summary traces on tick rows", {
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

  built_fig <- plotly::plotly_build(mod_env$plot_build_plotly_figure(payload))
  tick_vals <- sort(unique(as.numeric(built_fig$x$layout$yaxis$tickvals)))

  marker_traces <- Filter(function(trace) identical(trace$type, "scatter") && identical(trace$mode, "markers"), built_fig$x$data)
  summary_marker_traces <- Filter(function(trace) !isTRUE(grepl("Individuella", trace$name %||% "")), marker_traces)
  summary_y <- sort(unique(unlist(lapply(summary_marker_traces, function(trace) as.numeric(trace$y)))))

  expect_equal(summary_y, tick_vals)
})

# Tests for combined groups functionality
test_that("plot_create_combined_group creates a valid combined group", {
  mod_env <- make_mod_plot_env()

  source_defs <- list(
    list(col = "sex", value = "Kvinna", label = "Kvinna", is_total = FALSE),
    list(col = "year", value = "2024", label = "2024", is_total = FALSE)
  )

  combined <- mod_env$plot_create_combined_group(source_defs, "Test Combined")

  expect_true(isTRUE(combined$is_combined))
  expect_equal(combined$label, "Test Combined")
  expect_equal(length(combined$source_defs), 2L)
  expect_false(isTRUE(combined$is_total))
})

test_that("plot_get_group_indices retrieves indices for simple groups correctly", {
  mod_env <- make_mod_plot_env()

  user_df <- data.frame(
    sex = c("Kvinna", "Kvinna", "Man", "Man"),
    stringsAsFactors = FALSE
  )

  simple_group <- list(col = "sex", value = "Kvinna", label = "Kvinna", is_total = FALSE)
  idx <- mod_env$plot_get_group_indices(user_df, simple_group)

  expect_equal(idx, c(1L, 2L))
})

test_that("plot_get_group_indices retrieves indices for total group", {
  mod_env <- make_mod_plot_env()

  user_df <- data.frame(
    sex = c("Kvinna", "Man", "Kvinna"),
    stringsAsFactors = FALSE
  )

  total_group <- mod_env$plot_total_group_definition()
  idx <- mod_env$plot_get_group_indices(user_df, total_group)

  expect_equal(idx, c(1L, 2L, 3L))
})

test_that("plot_get_group_indices retrieves indices for combined groups using OR logic", {
  mod_env <- make_mod_plot_env()

  user_df <- data.frame(
    sex = c("Kvinna", "Kvinna", "Man", "Man"),
    year = c("2024", "2025", "2024", "2025"),
    stringsAsFactors = FALSE
  )

  source_defs <- list(
    list(col = "sex", value = "Kvinna", label = "Kvinna", is_total = FALSE),
    list(col = "year", value = "2024", label = "2024", is_total = FALSE)
  )

  combined <- mod_env$plot_create_combined_group(source_defs, "Women or 2024")
  idx <- mod_env$plot_get_group_indices(user_df, combined)

  # Should match: (sex == Kvinna) OR (year == 2024)
  # Rows: 1(Kvinna,2024), 2(Kvinna,2025), 3(Man,2024) = indices 1, 2, 3
  expect_equal(sort(idx), c(1L, 2L, 3L))
})

test_that("plot_build_user_data works with combined groups", {
  mod_env <- make_mod_plot_env()

  user_df <- data.frame(
    sex = c("Kvinna", "Kvinna", "Man", "Man", "Kvinna"),
    year = c("2024", "2025", "2024", "2025", "2024"),
    stringsAsFactors = FALSE
  )

  scores <- c(50, 60, 70, 75, 55)

  source_defs <- list(
    list(col = "sex", value = "Kvinna", label = "Kvinna", is_total = FALSE),
    list(col = "year", value = "2024", label = "2024", is_total = FALSE)
  )

  combined <- mod_env$plot_create_combined_group(source_defs, "Women or 2024")

  user_data <- mod_env$plot_build_user_data(
    user_df = user_df,
    scores = scores,
    group_definitions = list(combined)
  )

  expect_equal(nrow(user_data$summary), 1L)
  expect_equal(user_data$summary$group_label[1], "Women or 2024")
  expect_equal(user_data$summary$n[1], 4L) # Rows 1, 2, 3, 5
  expect_equal(nrow(user_data$raw), 4L)
})

test_that("plot_build_user_data combines simple and combined groups", {
  mod_env <- make_mod_plot_env()

  user_df <- data.frame(
    sex = c("Kvinna", "Kvinna", "Man", "Man"),
    year = c("2024", "2025", "2024", "2025"),
    stringsAsFactors = FALSE
  )

  scores <- c(50, 60, 70, 75)

  # Create one simple and one combined group
  simple_defs <- mod_env$plot_parse_group_definitions(list(sex = "Kvinna"))

  source_defs <- list(
    list(col = "year", value = "2024", label = "2024", is_total = FALSE)
  )
  combined <- mod_env$plot_create_combined_group(source_defs, "Year 2024")

  all_defs <- c(simple_defs, list(combined))

  user_data <- mod_env$plot_build_user_data(
    user_df = user_df,
    scores = scores,
    group_definitions = all_defs
  )

  expect_equal(nrow(user_data$summary), 2L)
  expect_equal(user_data$summary$group_label, c("Kvinna", "Year 2024"))
  expect_equal(user_data$summary$n, c(2L, 2L))
})

test_that("plot_build_combined_payload includes combined groups", {
  mod_env <- make_mod_plot_env()

  user_df <- data.frame(
    sex = c("Kvinna", "Kvinna", "Man", "Man"),
    year = c("2024", "2025", "2024", "2025"),
    stringsAsFactors = FALSE
  )

  scores <- c(50, 60, 70, 75)

  source_defs <- list(
    list(col = "sex", value = "Kvinna", label = "Kvinna", is_total = FALSE),
    list(col = "year", value = "2024", label = "2024", is_total = FALSE)
  )

  combined <- mod_env$plot_create_combined_group(source_defs, "Women or 2024")

  payload <- mod_env$plot_build_combined_payload(
    user_df = user_df,
    scores = scores,
    group_definitions = list(combined),
    ref_df = data.frame(),
    layers = "user"
  )

  expect_equal(payload$ordered_groups, c("Women or 2024"))
  expect_equal(nrow(payload$user_summary), 1L)
})

test_that("plot_build_combined_payload orders combined groups with basic groups", {
  mod_env <- make_mod_plot_env()

  user_df <- data.frame(
    sex = c("Kvinna", "Kvinna", "Man", "Man"),
    year = c("2024", "2025", "2024", "2025"),
    stringsAsFactors = FALSE
  )

  scores <- c(50, 60, 70, 75)

  basic_defs <- mod_env$plot_parse_group_definitions(list(sex = "Kvinna"))
  combined <- mod_env$plot_create_combined_group(
    list(list(col = "year", value = "2024", label = "2024", is_total = FALSE)),
    "Year 2024"
  )

  payload <- mod_env$plot_build_combined_payload(
    user_df = user_df,
    scores = scores,
    group_definitions = c(basic_defs, list(combined)),
    ref_df = data.frame(),
    layers = "user"
  )

  # Should have both groups in output
  expect_equal(length(payload$ordered_groups), 2L)
  expect_true("Kvinna" %in% payload$ordered_groups)
  expect_true("Year 2024" %in% payload$ordered_groups)
})

test_that("plot_build_combined_payload includes total with combined groups", {
  mod_env <- make_mod_plot_env()

  user_df <- data.frame(
    sex = c("Kvinna", "Kvinna", "Man", "Man"),
    stringsAsFactors = FALSE
  )

  scores <- c(50, 60, 70, 75)

  combined <- mod_env$plot_create_combined_group(
    list(list(col = "sex", value = "Kvinna", label = "Kvinna", is_total = FALSE)),
    "Women Only"
  )

  total_def <- mod_env$plot_total_group_definition()

  payload <- mod_env$plot_build_combined_payload(
    user_df = user_df,
    scores = scores,
    group_definitions = list(total_def, combined),
    ref_df = data.frame(),
    layers = "user"
  )

  # Total should be first
  expect_equal(payload$ordered_groups[1], "Totalt")
  expect_true("Women Only" %in% payload$ordered_groups)
})

test_that("plot_build_combined_payload with multiple combined groups", {
  mod_env <- make_mod_plot_env()

  user_df <- data.frame(
    sex = c("Kvinna", "Kvinna", "Man", "Man", "Kvinna"),
    year = c("2024", "2025", "2024", "2025", "2024"),
    stringsAsFactors = FALSE
  )

  scores <- c(50, 60, 70, 75, 55)

  combined_1 <- mod_env$plot_create_combined_group(
    list(list(col = "sex", value = "Kvinna", label = "Kvinna", is_total = FALSE)),
    "Only Women"
  )

  combined_2 <- mod_env$plot_create_combined_group(
    list(list(col = "year", value = "2024", label = "2024", is_total = FALSE)),
    "Only 2024"
  )

  payload <- mod_env$plot_build_combined_payload(
    user_df = user_df,
    scores = scores,
    group_definitions = list(combined_1, combined_2),
    ref_df = data.frame(),
    layers = "user"
  )

  expect_equal(length(payload$ordered_groups), 2L)
  expect_true("Only Women" %in% payload$ordered_groups)
  expect_true("Only 2024" %in% payload$ordered_groups)
  expect_equal(nrow(payload$user_summary), 2L)
})

test_that("plot_build_plotly_figure renders combined groups", {
  skip_if_not_installed("plotly")
  mod_env <- make_mod_plot_env()

  user_df <- data.frame(
    sex = c("Kvinna", "Kvinna", "Man", "Man"),
    stringsAsFactors = FALSE
  )

  scores <- c(50, 60, 70, 75)

  combined <- mod_env$plot_create_combined_group(
    list(list(col = "sex", value = "Kvinna", label = "Kvinna", is_total = FALSE)),
    "Women Only"
  )

  payload <- mod_env$plot_build_combined_payload(
    user_df = user_df,
    scores = scores,
    group_definitions = list(combined),
    ref_df = data.frame(),
    layers = c("user", "raw")
  )

  fig <- mod_env$plot_build_plotly_figure(payload)
  built_fig <- plotly::plotly_build(fig)

  expect_s3_class(fig, "plotly")
  expect_gte(length(built_fig$x$data), 3L) # summary segments, markers, and raw points
})
