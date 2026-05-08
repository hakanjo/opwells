plot_trim_non_empty <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[!is.na(x_chr) & nzchar(x_chr)]
}

plot_candidate_group_columns <- function(user_df) {
  if (!is.data.frame(user_df) || nrow(user_df) == 0 || ncol(user_df) == 0) {
    return(character(0))
  }

  candidate_names <- names(user_df)
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

plot_available_groups <- function(user_df = NULL, group_col_name = NULL, ref_df = NULL) {
  if (is.data.frame(user_df) && nrow(user_df) > 0 && !is.null(group_col_name) && nzchar(group_col_name) && group_col_name %in% names(user_df)) {
    user_groups <- sort(unique(plot_trim_non_empty(user_df[[group_col_name]])))
    if (length(user_groups)) {
      return(user_groups)
    }
  }

  if (is.data.frame(ref_df) && nrow(ref_df) > 0 && "group" %in% names(ref_df)) {
    return(sort(unique(plot_trim_non_empty(ref_df$group))))
  }

  character(0)
}

plot_build_user_data <- function(user_df, scores, group_col_name, selected_groups) {
  if (!is.data.frame(user_df) || nrow(user_df) == 0 || is.null(scores)) {
    return(list(summary = data.frame(), raw = data.frame()))
  }

  if (is.null(group_col_name) || !nzchar(group_col_name) || !(group_col_name %in% names(user_df))) {
    return(list(summary = data.frame(), raw = data.frame()))
  }

  groups <- plot_trim_non_empty(user_df[[group_col_name]])
  if (!length(groups)) {
    return(list(summary = data.frame(), raw = data.frame()))
  }

  selected_groups <- plot_normalize_group_selection(selected_groups, sort(unique(groups)), target_n = 2L, allow_empty = TRUE)
  if (!length(selected_groups)) {
    return(list(summary = data.frame(), raw = data.frame()))
  }

  row_group_values <- trimws(as.character(user_df[[group_col_name]]))
  selected_idx <- row_group_values %in% selected_groups
  if (!any(selected_idx)) {
    return(list(summary = data.frame(), raw = data.frame()))
  }

  score_values <- suppressWarnings(as.numeric(scores))
  plot_rows <- which(selected_idx)
  plot_scores <- score_values[selected_idx]
  plot_groups <- row_group_values[selected_idx]

  split_rows <- split(seq_along(plot_scores), plot_groups)
  summary_list <- lapply(names(split_rows), function(group_label) {
    idx <- split_rows[[group_label]]
    x_clean <- plot_scores[idx]
    x_clean <- x_clean[!is.na(x_clean)]
    if (!length(x_clean)) {
      return(data.frame(
        group_label = group_label,
        mean = NA_real_,
        sd = NA_real_,
        q_1_6 = NA_real_,
        q_5_6 = NA_real_,
        n = 0L,
        source = "user",
        hover_text = sprintf("<b>%s</b><br>Källa: Användardata<br>Inga giltiga poäng", group_label),
        stringsAsFactors = FALSE
      ))
    }

    mean_value <- mean(x_clean)
    q_low <- as.numeric(stats::quantile(x_clean, probs = 1 / 6, na.rm = TRUE))
    q_high <- as.numeric(stats::quantile(x_clean, probs = 5 / 6, na.rm = TRUE))

    data.frame(
      group_label = group_label,
      mean = mean_value,
      sd = stats::sd(x_clean),
      q_1_6 = q_low,
      q_5_6 = q_high,
      n = length(x_clean),
      source = "user",
      hover_text = sprintf(
        "<b>%s</b><br>Källa: Användardata<br>Medel: %.1f<br>Två tredjedelar: %.1f-%.1f<br>Antal svar: %d",
        group_label,
        mean_value,
        q_low,
        q_high,
        length(x_clean)
      ),
      stringsAsFactors = FALSE
    )
  })

  summary_data <- do.call(rbind, summary_list)
  summary_data <- summary_data[summary_data$group_label %in% selected_groups, , drop = FALSE]

  raw_data <- data.frame(
    row_id = plot_rows,
    group_label = plot_groups,
    score_value = plot_scores,
    source = "raw",
    stringsAsFactors = FALSE
  )
  raw_data <- raw_data[!is.na(raw_data$score_value), , drop = FALSE]
  raw_data$hover_text <- sprintf(
    "<b>%s</b><br>Rad: %d<br>Skalad poäng: %.1f",
    raw_data$group_label,
    raw_data$row_id,
    raw_data$score_value
  )

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
    "<b>%s</b><br>Källa: Referensdata<br>Medel: %.1f<br>Två tredjedelar: %.1f-%.1f",
    ref_df$group_label,
    suppressWarnings(as.numeric(ref_df$mean)),
    suppressWarnings(as.numeric(ref_df$q_1_6)),
    suppressWarnings(as.numeric(ref_df$q_5_6))
  )

  ref_df
}

plot_build_status_messages <- function(layers, user_df, scores, group_col_name, selected_groups, ref_summary) {
  messages <- character(0)

  show_user <- "user" %in% layers || "raw" %in% layers
  show_ref <- "reference" %in% layers

  if (show_user) {
    if (!is.data.frame(user_df) || nrow(user_df) == 0) {
      messages <- c(messages, "Ingen användardata laddad. Ladda data i fliken Ladda upp data för att visa användardata.")
    } else if (is.null(scores)) {
      messages <- c(messages, "Ingen skalad poäng beräknad för användardata.")
    } else if (is.null(group_col_name) || !nzchar(group_col_name) || !(group_col_name %in% names(user_df))) {
      messages <- c(messages, "Välj en gruppdefinition för att jämföra användardata mellan grupper.")
    } else if (!length(selected_groups)) {
      messages <- c(messages, "Välj minst en grupp för att visa användardata.")
    }
  }

  if (show_ref && length(selected_groups) && nrow(ref_summary) > 0) {
    missing_groups <- setdiff(selected_groups, unique(ref_summary$group_label))
    if (length(missing_groups)) {
      messages <- c(messages, sprintf("Referensdata saknas för: %s", paste(missing_groups, collapse = ", ")))
    }
  }

  unique(messages)
}

plot_build_combined_payload <- function(user_df, scores, group_col_name, selected_groups, ref_df, layers) {
  layers <- unique(plot_trim_non_empty(layers))
  user_data <- list(summary = data.frame(), raw = data.frame())
  ref_summary <- data.frame()

  if ("user" %in% layers || "raw" %in% layers) {
    user_data <- plot_build_user_data(user_df, scores, group_col_name, selected_groups)
  }

  if ("reference" %in% layers) {
    ref_summary <- plot_build_reference_data(ref_df, selected_groups)
  }

  ordered_groups <- unique(c(selected_groups, user_data$summary$group_label, ref_summary$group_label, user_data$raw$group_label))
  ordered_groups <- ordered_groups[!is.na(ordered_groups) & nzchar(ordered_groups)]

  list(
    user_summary = user_data$summary,
    user_raw = if ("raw" %in% layers) user_data$raw else data.frame(),
    ref_summary = ref_summary,
    ordered_groups = ordered_groups,
    status_messages = plot_build_status_messages(
      layers = layers,
      user_df = user_df,
      scores = scores,
      group_col_name = group_col_name,
      selected_groups = selected_groups,
      ref_summary = ref_summary
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
      ur$y_val  <- base_y + 0.12
      user_plot <- rbind(user_plot, ur)
    }

    rr <- payload$ref_summary[payload$ref_summary$group_label == group_label, , drop = FALSE]
    if (nrow(rr) > 0) {
      rr$y_val <- base_y - 0.12
      ref_plot <- rbind(ref_plot, rr)
    }

    raw <- payload$user_raw[payload$user_raw$group_label == group_label, , drop = FALSE]
    if (nrow(raw) > 0) {
      raw$y_value <- base_y + 0.12 + plot_group_jitter(nrow(raw))
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
      legend = list(orientation = "h", x = 0, y = -0.15),
      annotations = list(
        list(
          text = "Lägst välbefinnande",
          x = 0,
          y = -0.14,
          xref = "x",
          yref = "paper",
          showarrow = FALSE,
          xanchor = "left",
          font = list(color = "#6b6b6b")
        ),
        list(
          text = "Högst välbefinnande",
          x = 100,
          y = -0.14,
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
        width = 4,
        p("Jämför användardata och referensdata i samma figur. Välj grupper till vänster och håll pekaren över punkter eller sammanfattningar för detaljer."),
        selectInput(ns("group_col"), "Gruppdefinition i användardata", choices = character(0)),
        selectizeInput(
          ns("selected_groups"),
          "Grupper att visa",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        ),
        checkboxGroupInput(
          ns("data_layers"),
          "Visa i figuren",
          choices = c(
            "Användardata" = "user",
            "Referensdata" = "reference",
            "Individuella datapunkter" = "raw"
          ),
          selected = c("user", "reference")
        ),
        uiOutput(ns("plot_status"))
      ),
      column(
        width = 8,
        p("Sammanfattningsmarkören visar gruppens medelvärde. Linjen visar spannet där ungefär två tredjedelar av svaren ligger. Referensdata visas med separat symbol och linjestil."),
        plotly::plotlyOutput(ns("comparison_plot"), height = "560px")
      )
    )
  )
}

mod_plot_server <- function(id, data = NULL, scores = NULL) {
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

    observe({
      user_df <- user_data()
      group_choices <- plot_candidate_group_columns(user_df)
      selected_group_col <- NULL
      if (length(group_choices)) {
        selected_group_col <- if (!is.null(input$group_col) && input$group_col %in% group_choices) input$group_col else group_choices[1]
      }

      updateSelectInput(session, "group_col", choices = group_choices, selected = selected_group_col)
    })

    available_groups <- reactive({
      plot_available_groups(
        user_df = user_data(),
        group_col_name = input$group_col,
        ref_df = ref_data()
      )
    })

    # Use observeEvent so input$selected_groups is read via isolate() and does
    # not become a reactive dependency of this observer, which would create a
    # feedback loop: user picks group → observer fires → updateSelectizeInput
    # → input changes → observer fires again → flicker.
    observeEvent(available_groups(), {
      groups   <- available_groups()
      current  <- isolate(input$selected_groups)
      selected <- plot_normalize_group_selection(current, groups, target_n = 2L, allow_empty = FALSE)
      updateSelectizeInput(session, "selected_groups", choices = groups, selected = selected, server = TRUE)
    }, ignoreNULL = FALSE)

    plot_layers <- reactive({
      layers <- unique(plot_trim_non_empty(input$data_layers))
      if (!length(layers)) {
        return(character(0))
      }
      layers
    })

    plot_payload <- reactive({
      plot_build_combined_payload(
        user_df = user_data(),
        scores = current_scores(),
        group_col_name = input$group_col,
        selected_groups = plot_normalize_group_selection(input$selected_groups, available_groups(), target_n = 2L, allow_empty = TRUE),
        ref_df = ref_data(),
        layers = plot_layers()
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
      available_groups = available_groups,
      plot_payload = plot_payload
    )
  })
}
