plot_trim_non_empty <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[!is.na(x_chr) & nzchar(x_chr)]
}

plot_total_group_label <- function() {
  "Totalt"
}

plot_total_group_definition <- function() {
  list(col = NA_character_, value = NA_character_, label = plot_total_group_label(), is_total = TRUE)
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

# Returns a named list: column name -> sorted unique values from that column.
plot_column_groups <- function(user_df, exclude_cols = character(0)) {
  cols <- plot_candidate_group_columns(user_df, exclude_cols)
  if (!length(cols)) return(list())
  result <- lapply(cols, function(col) sort(unique(plot_trim_non_empty(user_df[[col]]))))
  names(result) <- cols
  result
}

# Convert named selections (list(col = c(val,...))) into a list of group
# definition records: list(col=, value=, label=).  Labels are the value string
# unless the same value appears in more than one column, in which case they are
# disambiguated as "col: value".
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
  all_cols   <- vapply(defs, `[[`, character(1), "col")
  value_col_count <- tapply(all_cols, all_values, function(x) length(unique(x)))
  ambiguous <- names(value_col_count)[value_col_count > 1]

  lapply(defs, function(d) {
    d$label <- if (d$value %in% ambiguous) paste0(d$col, ": ", d$value) else d$value
    d$is_total <- FALSE
    d
  })
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

# group_definitions: list of list(col=, value=, label=) produced by
# plot_parse_group_definitions().  Each entry selects one group for the plot.
plot_build_user_data <- function(user_df, scores, group_definitions) {
  if (!is.data.frame(user_df) || nrow(user_df) == 0 || is.null(scores)) {
    return(list(summary = data.frame(), raw = data.frame()))
  }
  if (!length(group_definitions)) {
    return(list(summary = data.frame(), raw = data.frame()))
  }

  score_values <- suppressWarnings(as.numeric(scores))

  summary_list <- lapply(group_definitions, function(gd) {
    col      <- gd$col
    value    <- gd$value
    label    <- gd$label
    is_total <- isTRUE(gd$is_total)

    if (is_total) {
      idx <- seq_len(nrow(user_df))
    } else {
      if (!(col %in% names(user_df))) return(NULL)
      row_vals <- trimws(as.character(user_df[[col]]))
      idx <- which(row_vals == value)
    }

    if (!length(idx)) return(NULL)

    x_clean <- score_values[idx]
    x_clean <- x_clean[!is.na(x_clean)]

    if (!length(x_clean)) {
      return(data.frame(
        group_label = label,
        mean = NA_real_, sd = NA_real_, q_1_6 = NA_real_, q_5_6 = NA_real_,
        n = 0L, source = "user", is_total = is_total,
        hover_text = sprintf("<b>%s</b><br>Inga giltiga poäng", label),
        stringsAsFactors = FALSE
      ))
    }

    mean_value <- mean(x_clean)
    q_low  <- as.numeric(stats::quantile(x_clean, probs = 1 / 6, na.rm = TRUE))
    q_high <- as.numeric(stats::quantile(x_clean, probs = 5 / 6, na.rm = TRUE))

    data.frame(
      group_label = label,
      mean = mean_value, sd = stats::sd(x_clean),
      q_1_6 = q_low, q_5_6 = q_high, n = length(x_clean),
      source = "user", is_total = is_total,
      hover_text = sprintf(
        "<b>%s</b><br>Medel: %.1f<br>Två tredjedelar: %.1f-%.1f<br>Antal svar: %d",
        label, mean_value, q_low, q_high, length(x_clean)
      ),
      stringsAsFactors = FALSE
    )
  })
  summary_list <- Filter(Negate(is.null), summary_list)
  summary_data <- if (length(summary_list)) do.call(rbind, summary_list) else data.frame()

  raw_list <- lapply(group_definitions, function(gd) {
    col      <- gd$col
    value    <- gd$value
    label    <- gd$label
    is_total <- isTRUE(gd$is_total)

    if (is_total) {
      idx <- seq_len(nrow(user_df))
    } else {
      if (!(col %in% names(user_df))) return(NULL)
      row_vals <- trimws(as.character(user_df[[col]]))
      idx <- which(row_vals == value)
    }

    if (!length(idx)) return(NULL)

    df <- data.frame(
      row_id = idx + 1, group_label = label,
      score_value = score_values[idx], source = "raw", is_total = is_total,
      stringsAsFactors = FALSE
    )
    df <- df[!is.na(df$score_value), , drop = FALSE]
    if (!nrow(df)) return(NULL)
    df$hover_text <- sprintf(
      "<b>%s</b><br>Rad: %d<br>Skalad poäng: %.1f",
      df$group_label, df$row_id, df$score_value
    )
    df
  })
  raw_list <- Filter(Negate(is.null), raw_list)
  raw_data <- if (length(raw_list)) do.call(rbind, raw_list) else data.frame()

  list(summary = summary_data, raw = raw_data)
}

plot_build_reference_data <- function(ref_df, selected_groups) {
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
  ref_df$hover_text <- sprintf(
    "<b>%s</b><br>Medel: %.1f<br>Två tredjedelar: %.1f-%.1f",
    ref_df$group_label,
    suppressWarnings(as.numeric(ref_df$mean)),
    suppressWarnings(as.numeric(ref_df$q_1_6)),
    suppressWarnings(as.numeric(ref_df$q_5_6))
  )

  ref_df
}

plot_build_status_messages <- function(layers, user_df, scores, group_definitions, ref_summary) {
  messages <- character(0)

  show_user <- "user" %in% layers || "raw" %in% layers
  show_ref  <- "reference" %in% layers

  if (show_user) {
    if (!is.data.frame(user_df) || nrow(user_df) == 0) {
      messages <- c(messages, "Ingen användardata laddad. Ladda data i fliken Ladda upp data för att visa användardata.")
    } else if (is.null(scores)) {
      messages <- c(messages, "Ingen skalad poäng beräknad för användardata.")
    }
  }

  selected_labels <- if (length(group_definitions)) {
    vapply(group_definitions, `[[`, character(1), "label")
  } else {
    character(0)
  }

  if (show_ref && length(selected_labels) && is.data.frame(ref_summary) && nrow(ref_summary) > 0) {
    missing_groups <- setdiff(selected_labels, unique(ref_summary$group_label))
    if (length(missing_groups)) {
      messages <- c(messages, sprintf("Referensdata saknas för: %s", paste(missing_groups, collapse = ", ")))
    }
  }

  unique(messages)
}

plot_build_combined_payload <- function(user_df, scores, group_definitions, ref_df, layers) {
  layers <- unique(plot_trim_non_empty(layers))
  user_data   <- list(summary = data.frame(), raw = data.frame())
  ref_summary <- data.frame()

  selected_labels <- if (length(group_definitions)) {
    vapply(group_definitions, `[[`, character(1), "label")
  } else {
    character(0)
  }

  if ("user" %in% layers || "raw" %in% layers) {
    user_data <- plot_build_user_data(user_df, scores, group_definitions)
  }

  if ("reference" %in% layers) {
    ref_summary <- plot_build_reference_data(ref_df, selected_labels)
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
      layers           = layers,
      user_df          = user_df,
      scores           = scores,
      group_definitions = group_definitions,
      ref_summary      = ref_summary
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

plot_build_plotly_figure <- function(payload) {
  present_groups <- payload$ordered_groups[payload$ordered_groups %in% unique(c(
    payload$user_summary$group_label,
    payload$ref_summary$group_label,
    payload$user_raw$group_label
  ))]

  if (!length(present_groups)) {
    return(plot_build_empty_figure("Ingen data att visa för det aktuella urvalet."))
  }

  y_positions <- stats::setNames(seq_along(present_groups), present_groups)

  # Pre-build batched data frames with resolved y positions so we can add each
  # trace type once rather than per-group, avoiding the untyped orphan trace
  # that results from loop-accumulating onto an empty plot_ly() base.
  user_plot <- data.frame()
  ref_plot  <- data.frame()
  raw_plot  <- data.frame()

  for (group_label in present_groups) {
    base_y <- unname(y_positions[[group_label]])

    ur <- payload$user_summary[payload$user_summary$group_label == group_label, , drop = FALSE]
    if (nrow(ur) > 0) {
        ur$y_val  <- base_y
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
      raw_plot    <- rbind(raw_plot, raw)
    }
  }

  has_user <- nrow(user_plot) > 0
  has_ref  <- nrow(ref_plot)  > 0
  has_raw  <- nrow(raw_plot)  > 0

  # Initialise with a typed, invisible base trace so Plotly never encounters an
  # untyped trace and therefore never emits "no trace type specified" warnings.
  fig <- plotly::plot_ly(
    type        = "scatter",
    mode        = "none",
    showlegend  = FALSE,
    hoverinfo   = "none"
  )

  if (has_user) {
    fig <- fig |>
      plotly::add_segments(
        data      = user_plot,
        x         = ~q_1_6,
        xend      = ~q_5_6,
        y         = ~y_val,
        yend      = ~y_val,
        line      = list(color = "#1f78b4", width = 4),
        text      = ~hover_text,
        hoverinfo = "text",
        showlegend = FALSE,
        inherit   = FALSE
      ) |>
      plotly::add_markers(
        data        = user_plot,
        x           = ~mean,
        y           = ~y_val,
        marker      = list(color = "#1f78b4", size = 11, symbol = "circle"),
        text        = ~hover_text,
        hoverinfo   = "text",
        name        = "Användardata",
        legendgroup = "user",
        showlegend  = TRUE,
        inherit     = FALSE
      )
  }

  if (has_ref) {
    fig <- fig |>
      plotly::add_segments(
        data      = ref_plot,
        x         = ~q_1_6,
        xend      = ~q_5_6,
        y         = ~y_val,
        yend      = ~y_val,
        line      = list(color = "#e07a2f", width = 4, dash = "dot"),
        text      = ~hover_text,
        hoverinfo = "text",
        showlegend = FALSE,
        inherit   = FALSE
      ) |>
      plotly::add_markers(
        data        = ref_plot,
        x           = ~mean,
        y           = ~y_val,
        marker      = list(color = "#e07a2f", size = 11, symbol = "diamond-open"),
        text        = ~hover_text,
        hoverinfo   = "text",
        name        = "Referensdata",
        legendgroup = "reference",
        showlegend  = TRUE,
        inherit     = FALSE
      )
  }

  if (has_raw) {
    fig <- fig |>
      plotly::add_markers(
        data        = raw_plot,
        x           = ~score_value,
        y           = ~y_value,
        marker      = list(color = "rgba(31,120,180,0.35)", size = 8, symbol = "circle-open"),
        text        = ~hover_text,
        hoverinfo   = "text",
        name        = "Individuella datapunkter",
        legendgroup = "raw",
        showlegend  = TRUE,
        inherit     = FALSE
      )
  }

  fig |>
    plotly::layout(
      showlegend = FALSE,
      xaxis = list(
        title = "",
        range = c(0, 100),
        tickmode = "linear",
        dtick = 10,
        zeroline = FALSE
      ),
      yaxis = list(
        title = "",
        tickmode = "array",
        tickvals = unname(y_positions),
        ticktext = names(y_positions),
        autorange = "reversed",
        zeroline = FALSE
      ),
      hovermode = "closest",
      margin = list(l = 150, r = 40, t = 20, b = 80),
      annotations = list(
        list(
          text = "Lägst välbefinnande",
          x = 0,
          y = 0,
          xref = "x",
          yref = "paper",
          showarrow = FALSE,
          xanchor = "left",
          font = list(color = "#6b6b6b")
        ),
        list(
          text = "Högst välbefinnande",
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

mod_plot_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    fluidRow(
      column(
        width = 3,
        p("När inga grupper är valda visas referensdata. När en eller flera grupper väljs visas användardata. Håll pekaren över punkter eller sammanfattningar för detaljer."),
        uiOutput(ns("group_selectors")),
        uiOutput(ns("include_total_toggle")),
        uiOutput(ns("raw_points_toggle")),
        uiOutput(ns("plot_status"))
      ),
      column(
        width = 9,
        p("Sammanfattningsmarkören visar gruppens medelvärde. Linjen visar spannet där ungefär två tredjedelar av svaren ligger. Referensdata visas med separat symbol och linjestil."),
        plotly::plotlyOutput(ns("comparison_plot"), height = "calc(95vh - 280px)")
      )
    )
  )
}

mod_plot_server <- function(id, data = NULL, scores = NULL, item_cols = NULL) {
  moduleServer(id, function(input, output, session) {
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

    observe({
      col_groups <- plot_column_groups(user_data(), current_item_cols())
      for (col in names(col_groups)) {
        input_id <- paste0("grp_", col)
        choices  <- col_groups[[col]]
        current  <- isolate(input[[input_id]])
        valid    <- current[current %in% choices]
        updateSelectizeInput(session, input_id, choices = choices, selected = valid, server = TRUE)
      }
    })

    output$group_selectors <- renderUI({
      col_groups <- plot_column_groups(user_data(), current_item_cols())
      ns <- session$ns
      if (!length(col_groups)) {
        return(p("Ladda data för att välja grupper.", style = "color: #6b6b6b;"))
      }
      lapply(names(col_groups), function(col) {
        selectizeInput(
          ns(paste0("grp_", col)),
          label = col,
          choices = col_groups[[col]],
          selected = NULL,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        )
      })
    })

    group_definitions <- reactive({
      col_groups <- plot_column_groups(user_data(), current_item_cols())
      selections <- lapply(names(col_groups), function(col) input[[paste0("grp_", col)]])
      names(selections) <- names(col_groups)
      selections <- Filter(function(v) length(plot_trim_non_empty(v)) > 0, selections)
      defs <- plot_parse_group_definitions(selections)
      if (length(defs) && isTRUE(input$include_total)) {
        defs <- c(list(plot_total_group_definition()), defs)
      }
      defs
    })

    output$include_total_toggle <- renderUI({
      col_groups <- plot_column_groups(user_data(), current_item_cols())
      has_selection <- any(vapply(names(col_groups), function(col) {
        length(plot_trim_non_empty(input[[paste0("grp_", col)]])) > 0
      }, logical(1)))
      if (!has_selection) {
        return(NULL)
      }

      current_value <- isolate(input$include_total)

      checkboxInput(
        session$ns("include_total"),
        label = "Inkludera totalt",
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
        label = "Visa individuella datapunkter",
        value = isTRUE(current_value)
      )
    })

    plot_layers <- reactive({
      plot_layers_from_selection(
        group_definitions = group_definitions(),
        show_raw = isTRUE(input$show_raw_points)
      )
    })

    plot_payload <- reactive({
      plot_build_combined_payload(
        user_df           = user_data(),
        scores            = current_scores(),
        group_definitions = group_definitions(),
        ref_df            = ref_data(),
        layers            = plot_layers()
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
      plot_build_plotly_figure(plot_payload())
    })

    list(
      column_groups = reactive(plot_column_groups(user_data(), current_item_cols())),
      plot_payload = plot_payload
    )
  })
}
