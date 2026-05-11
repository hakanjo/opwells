plot_trim_non_empty <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[!is.na(x_chr) & nzchar(x_chr)]
}

plot_total_group_label <- function(lang = i18n_default_language) {
  i18n_t(lang, "plot.total_label")
}

plot_total_group_definition <- function(lang = i18n_default_language) {
  list(col = NA_character_, value = NA_character_, label = plot_total_group_label(lang), is_total = TRUE)
}

plot_candidate_group_columns <- function(user_df, exclude_cols = character(0)) {
  if (!is.data.frame(user_df) || nrow(user_df) == 0 || ncol(user_df) == 0) {
    return(character(0))
  }

  candidate_names <- setdiff(names(user_df), exclude_cols)
  is_group_like <- vapply(candidate_names, function(col_nm) {
    x <- user_df[[col_nm]]
    out <- tryCatch({
      x_chr <- plot_trim_non_empty(x)
      if (!length(x_chr)) {
        return(FALSE)
      }

      x_num <- suppressWarnings(as.numeric(x_chr))
      all_numeric <- !any(is.na(x_num))
      if (!all_numeric) {
        return(TRUE)
      }

      length(unique(x_num)) <= 10
    }, error = function(e) FALSE)

    isTRUE(out)
  }, logical(1))

  candidate_names[is_group_like]
}

plot_normalize_group_selection <- function(selected, groups, target_n = 2L, allow_empty = FALSE) {
  groups <- plot_trim_non_empty(groups)
  groups <- unique(groups)
  if (!length(groups)) {
    return(character(0))
  }

  selected <- plot_trim_non_empty(selected)
  selected <- unique(selected[selected %in% groups])

  if (!length(selected) && !allow_empty) {
    selected <- utils::head(groups, max(target_n, 1L))
  }

  if (target_n > 0 && length(selected) < target_n && !allow_empty) {
    missing <- setdiff(groups, selected)
    if (length(missing)) {
      selected <- c(selected, utils::head(missing, target_n - length(selected)))
    }
  }

  unique(selected)
}

plot_column_groups <- function(user_df, exclude_cols = character(0)) {
  cols <- plot_candidate_group_columns(user_df, exclude_cols)
  if (!length(cols)) return(list())
  result <- lapply(cols, function(col) sort(unique(plot_trim_non_empty(user_df[[col]]))))
  names(result) <- cols
  result
}

plot_reference_group_choices <- function(ref_df) {
  if (!is.data.frame(ref_df) || nrow(ref_df) == 0 || !("group" %in% names(ref_df))) {
    return(character(0))
  }

  sort(unique(plot_trim_non_empty(ref_df$group)))
}

plot_sanitize_group_selections <- function(selections, col_groups) {
  if (!is.list(selections) || !length(col_groups)) {
    return(list())
  }

  out <- lapply(names(col_groups), function(col_nm) {
    selected <- selections[[col_nm]]
    selected <- plot_trim_non_empty(selected)
    selected[selected %in% col_groups[[col_nm]]]
  })
  names(out) <- names(col_groups)

  Filter(function(v) length(v) > 0, out)
}

plot_read_group_selections_from_input <- function(input, col_groups) {
  if (!length(col_groups)) {
    return(list())
  }

  selections <- lapply(names(col_groups), function(col_nm) input[[paste0("grp_", col_nm)]])
  names(selections) <- names(col_groups)
  plot_sanitize_group_selections(selections, col_groups)
}

plot_parse_group_definitions <- function(selections) {
  defs <- list()
  for (col in names(selections)) {
    values <- plot_trim_non_empty(selections[[col]])
    for (value in values) {
      defs <- c(defs, list(list(col = col, value = value)))
    }
  }
  if (!length(defs)) return(defs)

  all_values <- vapply(defs, `[[`, character(1), "value")
  all_cols <- vapply(defs, `[[`, character(1), "col")
  value_col_count <- tapply(all_cols, all_values, function(x) length(unique(x)))
  ambiguous <- names(value_col_count)[value_col_count > 1]

  lapply(defs, function(d) {
    d$label <- if (d$value %in% ambiguous) paste0(d$col, ": ", d$value) else d$value
    d$is_total <- FALSE
    d
  })
}

plot_build_selected_group_definitions <- function(selections, combined_groups = list(), include_total = FALSE, lang = i18n_default_language) {
  defs <- plot_parse_group_definitions(selections)

  if (length(combined_groups)) {
    defs <- c(defs, combined_groups)
  }

  if (isTRUE(include_total)) {
    defs <- c(list(plot_total_group_definition(lang)), defs)
  }

  defs
}

plot_create_combined_group <- function(source_defs, label) {
  list(
    is_combined = TRUE,
    source_defs = source_defs,
    label = label,
    is_total = FALSE
  )
}

plot_layers_from_selection <- function(group_definitions, show_raw = FALSE) {
  if (!length(group_definitions)) {
    return("reference")
  }

  if (isTRUE(show_raw)) {
    return(c("user", "raw"))
  }

  "user"
}

plot_get_group_indices <- function(user_df, group_def) {
  if (isTRUE(group_def$is_total)) {
    return(seq_len(nrow(user_df)))
  }

  if (isTRUE(group_def$is_combined)) {
    idx_list <- lapply(group_def$source_defs, function(src_def) {
      plot_get_group_indices(user_df, src_def)
    })
    return(sort(unique(unlist(idx_list))))
  }

  col <- group_def$col
  value <- group_def$value

  if (!(col %in% names(user_df))) {
    return(integer(0))
  }

  row_vals <- trimws(as.character(user_df[[col]]))
  which(row_vals == value)
}

plot_build_user_data <- function(user_df, scores, group_definitions, lang = i18n_default_language) {
  if (!is.data.frame(user_df) || nrow(user_df) == 0 || is.null(scores)) {
    return(list(summary = data.frame(), raw = data.frame()))
  }
  if (!length(group_definitions)) {
    return(list(summary = data.frame(), raw = data.frame()))
  }

  score_values <- suppressWarnings(as.numeric(scores))

  summary_list <- lapply(group_definitions, function(gd) {
    label <- gd$label
    is_total <- isTRUE(gd$is_total)

    idx <- plot_get_group_indices(user_df, gd)

    if (!length(idx)) return(NULL)

    x_clean <- score_values[idx]
    x_clean <- x_clean[!is.na(x_clean)]

    if (!length(x_clean)) {
      return(data.frame(
        group_label = label,
        mean = NA_real_, sd = NA_real_, q_1_6 = NA_real_, q_5_6 = NA_real_,
        n = 0L, source = "user", is_total = is_total,
        hover_text = i18n_t(lang, "plot.hover.no_valid", label),
        stringsAsFactors = FALSE
      ))
    }

    mean_value <- mean(x_clean)
    q_low <- as.numeric(stats::quantile(x_clean, probs = 1 / 6, na.rm = TRUE))
    q_high <- as.numeric(stats::quantile(x_clean, probs = 5 / 6, na.rm = TRUE))

    data.frame(
      group_label = label,
      mean = mean_value, sd = stats::sd(x_clean),
      q_1_6 = q_low, q_5_6 = q_high, n = length(x_clean),
      source = "user", is_total = is_total,
      hover_text = i18n_t(lang, "plot.hover.summary", label, mean_value, q_low, q_high, length(x_clean)),
      stringsAsFactors = FALSE
    )
  })
  summary_list <- Filter(Negate(is.null), summary_list)
  summary_data <- if (length(summary_list)) do.call(rbind, summary_list) else data.frame()

  raw_list <- lapply(group_definitions, function(gd) {
    label <- gd$label
    is_total <- isTRUE(gd$is_total)

    idx <- plot_get_group_indices(user_df, gd)

    if (!length(idx)) return(NULL)

    df <- data.frame(
      row_id = idx, group_label = label,
      score_value = score_values[idx], source = "raw", is_total = is_total,
      stringsAsFactors = FALSE
    )
    df <- df[!is.na(df$score_value), , drop = FALSE]
    if (!nrow(df)) return(NULL)
    df$hover_text <- i18n_t(lang, "plot.hover.raw", df$group_label, df$row_id, df$score_value)
    df
  })
  raw_list <- Filter(Negate(is.null), raw_list)
  raw_data <- if (length(raw_list)) do.call(rbind, raw_list) else data.frame()

  list(summary = summary_data, raw = raw_data)
}

plot_build_reference_data <- function(ref_df, selected_groups, lang = i18n_default_language) {
  if (!is.data.frame(ref_df) || nrow(ref_df) == 0 || !all(c("group", "mean", "q_1_6", "q_5_6") %in% names(ref_df))) {
    return(data.frame())
  }

  ref_df$group_label <- trimws(as.character(ref_df$group))
  ref_df <- ref_df[!is.na(ref_df$group_label) & nzchar(ref_df$group_label), , drop = FALSE]
  if (!length(selected_groups)) {
    selected_groups <- unique(ref_df$group_label)
  }

  ref_df <- ref_df[ref_df$group_label %in% selected_groups, , drop = FALSE]
  if (!nrow(ref_df)) {
    return(data.frame())
  }

  ref_df$source <- "reference"
  ref_df$hover_text <- i18n_t(
    lang,
    "plot.hover.reference",
    ref_df$group_label,
    suppressWarnings(as.numeric(ref_df$mean)),
    suppressWarnings(as.numeric(ref_df$q_1_6)),
    suppressWarnings(as.numeric(ref_df$q_5_6))
  )

  ref_df
}

plot_build_status_messages <- function(layers, user_df, scores, group_definitions, ref_summary, selected_reference_groups = character(0), lang = i18n_default_language) {
  messages <- character(0)

  show_user <- "user" %in% layers || "raw" %in% layers
  show_ref <- "reference" %in% layers

  if (show_user) {
    if (!is.data.frame(user_df) || nrow(user_df) == 0) {
      messages <- c(messages, i18n_t(lang, "plot.status.no_user_data"))
    } else if (is.null(scores)) {
      messages <- c(messages, i18n_t(lang, "plot.status.no_scores"))
    }
  }

  selected_labels <- if (length(group_definitions)) {
    vapply(group_definitions, `[[`, character(1), "label")
  } else {
    character(0)
  }

  if (show_ref) {
    selected_reference_groups <- unique(plot_trim_non_empty(selected_reference_groups))
    expected_ref_labels <- if (length(selected_reference_groups)) selected_reference_groups else selected_labels
    missing_groups <- setdiff(expected_ref_labels, unique(ref_summary$group_label))
    if (length(missing_groups)) {
      messages <- c(messages, i18n_t(lang, "plot.status.missing_reference", paste(missing_groups, collapse = ", ")))
    }
  }

  unique(messages)
}

plot_build_combined_payload <- function(user_df, scores, group_definitions, ref_df, layers, selected_reference_groups = character(0), lang = i18n_default_language) {
  layers <- unique(plot_trim_non_empty(layers))
  user_data <- list(summary = data.frame(), raw = data.frame())
  ref_summary <- data.frame()

  selected_labels <- if (length(group_definitions)) {
    vapply(group_definitions, `[[`, character(1), "label")
  } else {
    character(0)
  }

  if ("user" %in% layers || "raw" %in% layers) {
    user_data <- plot_build_user_data(user_df, scores, group_definitions, lang = lang)
  }

  if ("reference" %in% layers) {
    selected_ref_labels <- unique(plot_trim_non_empty(selected_reference_groups))
    if (!length(selected_ref_labels)) {
      selected_ref_labels <- selected_labels
    }

    ref_summary <- plot_build_reference_data(ref_df, selected_ref_labels, lang = lang)
  }

  ordered_groups <- unique(c(selected_labels, user_data$summary$group_label, ref_summary$group_label, user_data$raw$group_label))
  ordered_groups <- ordered_groups[!is.na(ordered_groups) & nzchar(ordered_groups)]

  total_group_labels <- unique(c(
    user_data$summary$group_label[!is.null(user_data$summary$is_total) & user_data$summary$is_total %in% TRUE],
    user_data$raw$group_label[!is.null(user_data$raw$is_total) & user_data$raw$is_total %in% TRUE]
  ))
  total_group_labels <- total_group_labels[!is.na(total_group_labels) & nzchar(total_group_labels)]
  if (length(total_group_labels)) {
    ordered_groups <- c(total_group_labels, setdiff(ordered_groups, total_group_labels))
  }

  list(
    user_summary = user_data$summary,
    user_raw = if ("raw" %in% layers) user_data$raw else data.frame(),
    ref_summary = ref_summary,
    ordered_groups = ordered_groups,
    status_messages = plot_build_status_messages(
      layers = layers,
      user_df = user_df,
      scores = scores,
      group_definitions = group_definitions,
      ref_summary = ref_summary,
      selected_reference_groups = selected_reference_groups,
      lang = lang
    )
  )
}

plot_build_empty_figure <- function(message) {
  plotly::plot_ly() |>
    plotly::layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(list(
        text = message,
        x = 0.5,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 15, color = "#5a5a5a")
      )),
      margin = list(l = 30, r = 30, t = 20, b = 20)
    )
}

plot_group_jitter <- function(n) {
  if (n <= 1) {
    return(0)
  }

  seq(-0.18, 0.18, length.out = n)
}

plot_build_plotly_figure <- function(payload, lang = i18n_default_language) {
  present_groups <- payload$ordered_groups[payload$ordered_groups %in% unique(c(
    payload$user_summary$group_label,
    payload$ref_summary$group_label,
    payload$user_raw$group_label
  ))]

  if (!length(present_groups)) {
    return(plot_build_empty_figure(i18n_t(lang, "plot.empty.no_data")))
  }

  y_positions <- stats::setNames(seq_along(present_groups), present_groups)

  user_plot <- data.frame()
  ref_plot <- data.frame()
  raw_plot <- data.frame()

  for (group_label in present_groups) {
    base_y <- unname(y_positions[[group_label]])

    ur <- payload$user_summary[payload$user_summary$group_label == group_label, , drop = FALSE]
    if (nrow(ur) > 0) {
      ur$y_val <- base_y
      user_plot <- rbind(user_plot, ur)
    }

    rr <- payload$ref_summary[payload$ref_summary$group_label == group_label, , drop = FALSE]
    if (nrow(rr) > 0) {
      rr$y_val <- base_y
      ref_plot <- rbind(ref_plot, rr)
    }

    raw <- payload$user_raw[payload$user_raw$group_label == group_label, , drop = FALSE]
    if (nrow(raw) > 0) {
      raw$y_value <- base_y + plot_group_jitter(nrow(raw))
      raw_plot <- rbind(raw_plot, raw)
    }
  }

  has_user <- nrow(user_plot) > 0
  has_ref <- nrow(ref_plot) > 0
  has_raw <- nrow(raw_plot) > 0

  fig <- plotly::plot_ly(
    type = "scatter",
    mode = "none",
    showlegend = FALSE,
    hoverinfo = "none"
  )

  if (has_user) {
    fig <- fig |>
      plotly::add_segments(
        data = user_plot,
        x = ~q_1_6,
        xend = ~q_5_6,
        y = ~y_val,
        yend = ~y_val,
        line = list(color = "#61a43e", width = 2),
        text = ~hover_text,
        hoverinfo = "text",
        showlegend = FALSE,
        inherit = FALSE
      ) |>
      plotly::add_markers(
        data = user_plot,
        x = ~mean,
        y = ~y_val,
        marker = list(color = "#61a43e", size = 11, symbol = "circle"),
        text = ~hover_text,
        hoverinfo = "text",
        name = i18n_t(lang, "plot.legend.user"),
        legendgroup = "user",
        showlegend = TRUE,
        inherit = FALSE
      )
  }

  if (has_ref) {
    fig <- fig |>
      plotly::add_segments(
        data = ref_plot,
        x = ~q_1_6,
        xend = ~q_5_6,
        y = ~y_val,
        yend = ~y_val,
        line = list(color = "#3f78b0", width = 2),
        text = ~hover_text,
        hoverinfo = "text",
        showlegend = FALSE,
        inherit = FALSE
      ) |>
      plotly::add_markers(
        data = ref_plot,
        x = ~mean,
        y = ~y_val,
        marker = list(color = "#3f78b0", size = 11, symbol = "diamond"),
        text = ~hover_text,
        hoverinfo = "text",
        name = i18n_t(lang, "plot.legend.reference"),
        legendgroup = "reference",
        showlegend = TRUE,
        inherit = FALSE
      )
  }

  if (has_raw) {
    fig <- fig |>
      plotly::add_markers(
        data = raw_plot,
        x = ~score_value,
        y = ~y_value,
        marker = list(color = "#3f78b0", size = 8, symbol = "circle-open"),
        text = ~hover_text,
        hoverinfo = "text",
        name = i18n_t(lang, "plot.legend.raw"),
        legendgroup = "raw",
        showlegend = TRUE,
        inherit = FALSE
      )
  }

  fig |>
    plotly::layout(
      showlegend = FALSE,
      xaxis = list(
        title = "",
        range = c(-5, 105),
        tickmode = "linear",
        dtick = 10,
        showgrid = FALSE,
        zeroline = FALSE
      ),
      yaxis = list(
        title = "",
        tickmode = "array",
        tickvals = unname(y_positions),
        ticktext = names(y_positions),
        autorange = "reversed",
        showgrid = FALSE,
        zeroline = FALSE
      ),
      hovermode = "closest",
      margin = list(l = 150, r = 40, t = 20, b = 80),
      annotations = list(
        list(
          text = i18n_t(lang, "plot.axis.low"),
          x = 0,
          y = 0,
          xref = "x",
          yref = "paper",
          showarrow = FALSE,
          xanchor = "left",
          font = list(color = "#6b6b6b")
        ),
        list(
          text = i18n_t(lang, "plot.axis.high"),
          x = 100,
          y = 0,
          xref = "x",
          yref = "paper",
          showarrow = FALSE,
          xanchor = "right",
          font = list(color = "#6b6b6b")
        )
      )
    ) |>
    plotly::config(displayModeBar = TRUE, responsive = TRUE)
}

mod_plot_ui <- function(id, lang = i18n_default_language) {
  ns <- NS(id)
  tr <- function(key, ...) i18n_t(lang, key, ...)

  tagList(
    br(),
    fluidRow(
      column(
        width = 3,
        p(tr("plot.ui.left_intro")),
        uiOutput(ns("group_selectors")),
        uiOutput(ns("reference_group_selector")),
        hr(),
        p(strong(tr("plot.ui.combine.title")), style = "margin-top: 20px;"),
        p(tr("plot.ui.combine.help"), style = "font-size: 0.9em; color: #6b6b6b;"),
        textInput(ns("combined_group_label"), label = tr("plot.ui.combine.label"), placeholder = tr("plot.ui.combine.placeholder")),
        actionButton(ns("add_combined_group"), tr("plot.ui.combine.add"), class = "btn-primary"),
        uiOutput(ns("combined_groups_list")),
        hr(),
        uiOutput(ns("include_total_toggle")),
        uiOutput(ns("raw_points_toggle")),
        uiOutput(ns("plot_status"))
      ),
      column(
        width = 9,
        p(tr("plot.ui.right_intro")),
        plotly::plotlyOutput(ns("comparison_plot"), height = "calc(95vh - 280px)")
      )
    )
  )
}

mod_plot_server <- function(id, data = NULL, scores = NULL, item_cols = NULL, group_state = NULL, active_tab = NULL, lang = NULL) {
  moduleServer(id, function(input, output, session) {
    resolved_lang <- if (is.null(lang)) reactive(i18n_default_language) else lang
    tr <- function(key, ...) i18n_t(resolved_lang(), key, ...)

    current_scores <- reactive({
      if (is.null(scores)) {
        return(NULL)
      }

      value <- scores()
      if (is.list(value) && "scaled_scores" %in% names(value)) {
        return(value$scaled_scores)
      }

      value
    })

    ref_data <- reactive({
      load_ref_data()
    })

    user_data <- reactive({
      if (is.null(data)) {
        return(NULL)
      }

      value <- data()
      if (!is.data.frame(value) || nrow(value) == 0 || ncol(value) == 0) {
        return(NULL)
      }

      value
    })

    current_item_cols <- reactive({
      if (is.null(item_cols)) return(character(0))
      v <- item_cols()
      if (is.list(v) && "selected_columns" %in% names(v)) v$selected_columns else if (is.character(v)) v else character(0)
    })

    col_groups <- reactive(plot_column_groups(user_data(), current_item_cols()))
    ref_group_choices <- reactive(plot_reference_group_choices(ref_data()))

    group_controller <- group_selection_controller(
      input = input,
      session = session,
      col_groups = col_groups,
      ref_group_choices = ref_group_choices,
      group_state = group_state,
      active_tab = active_tab,
      tab_value = "plot",
      include_total_input_id = "include_total"
    )

    use_shared_group_state <- isTRUE(group_controller$use_shared_group_state)
    selected_group_selections <- group_controller$selected_group_selections
    get_combined_groups <- group_controller$get_combined_groups
    set_combined_groups <- group_controller$set_combined_groups
    current_include_total <- group_controller$current_include_total
    current_reference_groups <- group_controller$current_reference_groups

    observe({
      groups <- col_groups()
      for (col in names(groups)) {
        input_id <- paste0("grp_", col)
        choices <- groups[[col]]
        current <- if (use_shared_group_state) {
          selected_group_selections()[[col]]
        } else {
          isolate(input[[input_id]])
        }
        valid <- current[current %in% choices]

        current_input <- isolate(input[[input_id]])
        current_norm <- sort(unique(plot_trim_non_empty(current_input)))
        valid_norm <- sort(unique(plot_trim_non_empty(valid)))

        if (identical(current_norm, valid_norm)) {
          next
        }

        freezeReactiveValue(input, input_id)
        updateSelectizeInput(session, input_id, choices = choices, selected = valid, server = TRUE)
      }
    })

    output$group_selectors <- renderUI({
      groups <- col_groups()
      ns <- session$ns
      if (!length(groups)) {
        return(p(tr("plot.ui.load_data_for_groups"), style = "color: #6b6b6b;"))
      }
      lapply(names(groups), function(col) {
        selectizeInput(
          ns(paste0("grp_", col)),
          label = col,
          choices = groups[[col]],
          selected = NULL,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        )
      })
    })

    output$reference_group_selector <- renderUI({
      if (is.null(user_data())) {
        return(NULL)
      }

      choices <- ref_group_choices()
      if (!length(choices)) {
        return(NULL)
      }

      selectizeInput(
        session$ns("reference_groups"),
        label = tr("plot.ui.reference_groups"),
        choices = choices,
        selected = current_reference_groups(),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })

    group_definitions <- reactive({
      plot_build_selected_group_definitions(
        selections = selected_group_selections(),
        combined_groups = get_combined_groups(),
        include_total = current_include_total(),
        lang = resolved_lang()
      )
    })

    observeEvent(input$add_combined_group, {
      label <- trimws(input$combined_group_label)
      if (!nzchar(label)) {
        showNotification(tr("plot.notif.enter_name"), type = "warning")
        return()
      }

      current_selections <- selected_group_selections()

      if (!length(current_selections)) {
        showNotification(tr("plot.notif.select_at_least_one"), type = "warning")
        return()
      }

      source_defs <- plot_parse_group_definitions(current_selections)

      if (!length(source_defs)) {
        showNotification(tr("plot.notif.create_failed"), type = "error")
        return()
      }

      combined_group <- plot_create_combined_group(source_defs, label)

      current_combined <- get_combined_groups()
      set_combined_groups(c(current_combined, list(combined_group)))

      updateTextInput(session, "combined_group_label", value = "")

      showNotification(tr("plot.notif.created", label), type = "message")
    })

    created_observers <- reactiveVal(character(0))

    output$combined_groups_list <- renderUI({
      combined <- get_combined_groups()
      if (!length(combined)) {
        return(NULL)
      }

      ns <- session$ns
      tagList(
        p(strong(tr("plot.ui.combined_count", length(combined))), style = "margin-top: 15px; margin-bottom: 5px;"),
        lapply(seq_along(combined), function(i) {
          group <- combined[[i]]
          label <- group$label
          safe_id <- gsub("[^a-zA-Z0-9_]", "_", label)
          tags$div(
            style = "background-color: #f5f5f5; padding: 8px; margin: 5px 0; border-radius: 4px; display: flex; justify-content: space-between; align-items: center;",
            tags$span(label),
            actionButton(
              ns(paste0("remove_combined_group_", safe_id)),
              tr("plot.ui.remove"),
              class = "btn-sm btn-default",
              style = "margin: 0;"
            )
          )
        })
      )
    })

    observe({
      combined <- get_combined_groups()
      if (!length(combined)) {
        return()
      }

      current_buttons <- vapply(combined, function(g) {
        paste0("remove_combined_group_", gsub("[^a-zA-Z0-9_]", "_", g$label))
      }, character(1))

      existing <- created_observers()

      for (i in seq_along(combined)) {
        label <- combined[[i]]$label
        safe_id <- gsub("[^a-zA-Z0-9_]", "_", label)
        button_id <- paste0("remove_combined_group_", safe_id)

        if (!(button_id %in% existing)) {
          local({
            btn_id <- button_id
            group_label <- label

            observeEvent(input[[btn_id]], {
              current_combined <- get_combined_groups()
              idx <- which(vapply(current_combined, function(g) g$label == group_label, logical(1)))

              if (length(idx) > 0) {
                removed_label <- current_combined[[idx[1]]]$label
                set_combined_groups(current_combined[-idx[1]])
                showNotification(tr("plot.notif.removed", removed_label), type = "message")
              }
            }, ignoreInit = TRUE)
          })

          created_observers(c(created_observers(), button_id))
        }
      }
    })

    output$include_total_toggle <- renderUI({
      current_value <- current_include_total()

      checkboxInput(
        session$ns("include_total"),
        label = tr("plot.ui.include_total"),
        value = isTRUE(current_value)
      )
    })

    output$raw_points_toggle <- renderUI({
      if (!length(group_definitions())) {
        return(NULL)
      }

      current_value <- isolate(input$show_raw_points)

      checkboxInput(
        session$ns("show_raw_points"),
        label = tr("plot.ui.show_raw"),
        value = isTRUE(current_value)
      )
    })

    plot_layers <- reactive({
      has_user_groups <- length(group_definitions()) > 0
      has_reference_groups <- length(current_reference_groups()) > 0

      layers <- character(0)
      if (has_user_groups) {
        layers <- c(layers, "user")
        if (isTRUE(input$show_raw_points)) {
          layers <- c(layers, "raw")
        }

        if (has_reference_groups) {
          layers <- c(layers, "reference")
        }
      } else {
        layers <- c(layers, "reference")
      }

      unique(layers)
    })

    plot_payload <- reactive({
      plot_build_combined_payload(
        user_df = user_data(),
        scores = current_scores(),
        group_definitions = group_definitions(),
        ref_df = ref_data(),
        layers = plot_layers(),
        selected_reference_groups = current_reference_groups(),
        lang = resolved_lang()
      )
    })

    output$plot_status <- renderUI({
      payload <- plot_payload()
      if (!length(payload$status_messages)) {
        return(NULL)
      }

      tagList(lapply(payload$status_messages, function(message) {
        p(style = "color: #6b6b6b;", message)
      }))
    })

    output$comparison_plot <- plotly::renderPlotly({
      plot_build_plotly_figure(plot_payload(), lang = resolved_lang())
    })

    list(
      column_groups = reactive(plot_column_groups(user_data(), current_item_cols())),
      plot_payload = plot_payload
    )
  })
}