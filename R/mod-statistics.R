mod_statistics_ui <- function(id, lang = i18n_default_language) {
  ns <- NS(id)
  tr <- function(key, ...) i18n_t(lang, key, ...)

  tagList(
    br(),
    h5(tr("stats.ui.title")),
    helpText(tr("stats.ui.help")),
    fluidRow(
      column(
        width = 4,
        p(tr("stats.ui.left_intro")),
        uiOutput(ns("group_selectors")),
        uiOutput(ns("reference_group_selector")),
        hr(),
        p(strong(tr("stats.ui.combine.title")), style = "margin-top: 20px;"),
        p(tr("stats.ui.combine.help"), style = "font-size: 0.9em; color: #6b6b6b;"),
        textInput(ns("combined_group_label"), label = tr("stats.ui.combine.label"), placeholder = tr("stats.ui.combine.placeholder")),
        actionButton(ns("add_combined_group"), tr("stats.ui.combine.add"), class = "btn-primary"),
        uiOutput(ns("combined_groups_list")),
        uiOutput(ns("include_total_toggle"))
      ),
      column(
        width = 8,
        h4(tr("stats.ui.summary_title")),
        uiOutput(ns("summary_statistics_ui")),
        h4(tr("stats.ui.pairwise_title")),
        uiOutput(ns("pairwise_statistics_ui"))
      )
    )
  )
}

mod_statistics_server <- function(id, data = NULL, likert_state = NULL, active_tab = NULL, group_state = NULL, lang = NULL) {
  moduleServer(id, function(input, output, session) {
    resolved_lang <- if (is.null(lang)) reactive(i18n_default_language) else lang
    tr <- function(key, ...) i18n_t(resolved_lang(), key, ...)

    ref_data <- reactive({
      load_ref_data()
    })

    has_user_data <- reactive({
      if (is.null(data)) {
        return(FALSE)
      }

      user_df <- data()
      is.data.frame(user_df) && nrow(user_df) > 0 && ncol(user_df) > 0
    })

    column_groups <- reactive({
      user_df <- if (is.null(data)) NULL else data()
      state <- current_likert_state()
      excluded <- if (is.null(state) || is.null(state$selected_columns)) character(0) else state$selected_columns

      plot_column_groups(user_df, exclude_cols = excluded)
    })

    ref_group_choices <- reactive(plot_reference_group_choices(ref_data()))

    group_controller <- group_selection_controller(
      input = input,
      session = session,
      col_groups = column_groups,
      ref_group_choices = ref_group_choices,
      group_state = group_state,
      active_tab = active_tab,
      tab_value = "statistics",
      include_total_input_id = "include_total"
    )

    use_shared_group_state <- isTRUE(group_controller$use_shared_group_state)
    selected_group_selections <- group_controller$selected_group_selections
    get_combined_groups <- group_controller$get_combined_groups
    set_combined_groups <- group_controller$set_combined_groups
    current_include_total <- group_controller$current_include_total
    current_reference_groups <- group_controller$current_reference_groups

    selected_group_definitions <- reactive({
      plot_build_selected_group_definitions(
        selections = selected_group_selections(),
        combined_groups = get_combined_groups(),
        include_total = current_include_total(),
        lang = resolved_lang()
      )
    })

    format_num <- function(x, digits = 2) {
      ifelse(is.na(x), "", i18n_format_number(x, resolved_lang(), digits = digits))
    }

    build_table <- function(df) {
      if (!is.data.frame(df) || nrow(df) == 0 || ncol(df) == 0) {
        return(tags$p(tr("stats.table.no_data")))
      }

      header_cells <- lapply(names(df), tags$th)
      body_rows <- lapply(seq_len(nrow(df)), function(row_i) {
        cells <- lapply(seq_along(df), function(col_i) {
          value <- df[[col_i]][row_i]
          tags$td(if (is.na(value) || identical(value, "")) "" else as.character(value))
        })
        tags$tr(cells)
      })

      tags$table(
        class = "table table-striped table-bordered",
        tags$thead(tags$tr(header_cells)),
        tags$tbody(body_rows)
      )
    }

    current_likert_state <- reactive({
      if (is.null(likert_state)) {
        return(NULL)
      }

      state <- likert_state()
      if (!is.list(state)) {
        return(NULL)
      }

      state
    })

    observe({
      col_groups <- column_groups()
      for (col_nm in names(col_groups)) {
        input_id <- paste0("grp_", col_nm)
        choices <- col_groups[[col_nm]]
        current <- if (use_shared_group_state) {
          selected_group_selections()[[col_nm]]
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

        updateSelectizeInput(
          session,
          input_id,
          choices = choices,
          selected = valid,
          server = TRUE
        )
      }
    })

    output$group_selectors <- renderUI({
      col_groups <- column_groups()
      if (!length(col_groups)) {
        return(p(tr("stats.ui.load_data_for_groups"), style = "color: #6b6b6b;"))
      }

      lapply(names(col_groups), function(col_nm) {
        selectizeInput(
          session$ns(paste0("grp_", col_nm)),
          label = col_nm,
          choices = col_groups[[col_nm]],
          selected = NULL,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        )
      })
    })

    output$reference_group_selector <- renderUI({
      if (!has_user_data()) {
        return(NULL)
      }

      choices <- ref_group_choices()
      if (!length(choices)) {
        return(NULL)
      }

      selectizeInput(
        session$ns("reference_groups"),
        label = tr("stats.ui.reference_groups"),
        choices = choices,
        selected = current_reference_groups(),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
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
        p(strong(tr("stats.ui.combined_count", length(combined))), style = "margin-top: 15px; margin-bottom: 5px;"),
        lapply(seq_along(combined), function(i) {
          group <- combined[[i]]
          label <- group$label
          safe_id <- gsub("[^a-zA-Z0-9_]", "_", label)

          tags$div(
            style = "background-color: #f5f5f5; padding: 8px; margin: 5px 0; border-radius: 4px; display: flex; justify-content: space-between; align-items: center;",
            tags$span(label),
            actionButton(
              ns(paste0("remove_combined_group_", safe_id)),
              tr("stats.ui.remove"),
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
      checkboxInput(
        session$ns("include_total"),
        label = tr("stats.ui.include_total"),
        value = current_include_total()
      )
    })

    comparison_data <- reactive({
      req(!is.null(data))
      user_df <- data()
      state <- current_likert_state()
      group_defs <- selected_group_definitions()

      req(
        is.data.frame(user_df),
        nrow(user_df) > 0,
        !is.null(state),
        !is.null(state$scaled_scores),
        length(state$scaled_scores) == nrow(user_df)
      )

      req(length(group_defs) > 0)

      scores <- suppressWarnings(as.numeric(state$scaled_scores))

      rows <- lapply(group_defs, function(gd) {
        if (!is.list(gd) || is.null(gd$label) || !nzchar(gd$label)) {
          return(NULL)
        }

        idx <- plot_get_group_indices(user_df, gd)
        if (!length(idx)) {
          return(NULL)
        }

        valid_idx <- idx[!is.na(scores[idx])]
        if (!length(valid_idx)) {
          return(NULL)
        }

        data.frame(
          group = gd$label,
          score = scores[valid_idx],
          stringsAsFactors = FALSE
        )
      })

      rows <- Filter(Negate(is.null), rows)
      req(length(rows) > 0)

      do.call(rbind, rows)
    })

    reference_statistics_raw <- reactive({
      ref_df <- ref_data()

      if (!is.data.frame(ref_df) || nrow(ref_df) == 0) {
        return(data.frame())
      }

      required_cols <- c("group", "mean", "median", "sd", "q_1_6", "q_5_6")
      if (!all(required_cols %in% names(ref_df))) {
        return(data.frame())
      }

      out <- data.frame(
        category = trimws(as.character(ref_df$group)),
        n = NA_integer_,
        mean = suppressWarnings(as.numeric(ref_df$mean)),
        median = suppressWarnings(as.numeric(ref_df$median)),
        sd = suppressWarnings(as.numeric(ref_df$sd)),
        range_low = suppressWarnings(as.numeric(ref_df$q_1_6)),
        range_high = suppressWarnings(as.numeric(ref_df$q_5_6)),
        stringsAsFactors = FALSE
      )

      out <- out[!is.na(out$category) & nzchar(out$category), , drop = FALSE]
      out <- out[order(out$category), , drop = FALSE]
      out
    })

    reference_summary_statistics <- reactive({
      out <- reference_statistics_raw()
      if (!is.data.frame(out) || nrow(out) == 0) {
        return(data.frame())
      }

      out$mean <- format_num(out$mean)
      out$median <- format_num(out$median)
      out$sd <- format_num(out$sd)
      out$range_low <- format_num(out$range_low)
      out$range_high <- format_num(out$range_high)

      names(out) <- c(
        tr("stats.summary.col.category"),
        tr("stats.summary.col.n"),
        tr("stats.summary.col.mean"),
        tr("stats.summary.col.median"),
        tr("stats.summary.col.sd"),
        tr("stats.summary.col.range_low"),
        tr("stats.summary.col.range_high")
      )

      out
    })

    reference_pairwise_statistics <- reactive({
      ref_df <- reference_statistics_raw()
      if (!is.data.frame(ref_df) || nrow(ref_df) < 2) {
        return(data.frame())
      }

      groups <- unique(ref_df$category)
      pairs <- utils::combn(groups, 2, simplify = FALSE)

      rows <- lapply(pairs, function(grp_pair) {
        g1 <- grp_pair[[1]]
        g2 <- grp_pair[[2]]

        x1 <- ref_df[ref_df$category == g1, , drop = FALSE][1, ]
        x2 <- ref_df[ref_df$category == g2, , drop = FALSE][1, ]

        data.frame(
          group_1 = g1,
          group_2 = g2,
          n1 = NA_integer_,
          n2 = NA_integer_,
          mean_diff = x1$mean - x2$mean,
          median_diff = x1$median - x2$median,
          stringsAsFactors = FALSE
        )
      })

      out <- do.call(rbind, rows)
      out$mean_diff <- format_num(out$mean_diff)
      out$median_diff <- format_num(out$median_diff)

      names(out) <- c(
        tr("stats.pairwise.col.group1"),
        tr("stats.pairwise.col.group2"),
        tr("stats.pairwise.col.n1"),
        tr("stats.pairwise.col.n2"),
        tr("stats.pairwise.col.mean_diff"),
        tr("stats.pairwise.col.median_diff")
      )

      out
    })

    summary_statistics <- reactive({
      df <- comparison_data()
      groups <- sort(unique(df$group))

      rows <- lapply(groups, function(g) {
        x <- df$score[df$group == g]
        data.frame(
          category = g,
          n = length(x),
          mean = mean(x),
          median = stats::median(x),
          sd = stats::sd(x),
          range_low = as.numeric(stats::quantile(x, probs = 1 / 6, na.rm = TRUE, names = FALSE)),
          range_high = as.numeric(stats::quantile(x, probs = 5 / 6, na.rm = TRUE, names = FALSE)),
          stringsAsFactors = FALSE
        )
      })

      out <- do.call(rbind, rows)
      out$mean <- format_num(out$mean)
      out$median <- format_num(out$median)
      out$sd <- format_num(out$sd)
      out$range_low <- format_num(out$range_low)
      out$range_high <- format_num(out$range_high)

      names(out) <- c(
        tr("stats.summary.col.category"),
        tr("stats.summary.col.n"),
        tr("stats.summary.col.mean"),
        tr("stats.summary.col.median"),
        tr("stats.summary.col.sd"),
        tr("stats.summary.col.range_low"),
        tr("stats.summary.col.range_high")
      )

      out
    })

    pairwise_statistics <- reactive({
      df <- comparison_data()
      user_groups <- sort(unique(df$group))
      ref_groups <- current_reference_groups()
      ref_stats <- reference_statistics_raw()

      all_groups <- c(user_groups, ref_groups)
      if (length(all_groups) < 2) {
        return(data.frame())
      }

      pairs <- utils::combn(all_groups, 2, simplify = FALSE)
      rows <- lapply(pairs, function(grp_pair) {
        g1 <- grp_pair[[1]]
        g2 <- grp_pair[[2]]

        is_g1_user <- g1 %in% user_groups
        is_g2_user <- g2 %in% user_groups
        is_g1_ref <- g1 %in% ref_groups
        is_g2_ref <- g2 %in% ref_groups

        if (is_g1_user && is_g2_user) {
          x1 <- df$score[df$group == g1]
          x2 <- df$score[df$group == g2]
          data.frame(
            group_1 = g1,
            group_2 = g2,
            n1 = length(x1),
            n2 = length(x2),
            mean_diff = mean(x1) - mean(x2),
            median_diff = stats::median(x1) - stats::median(x2),
            stringsAsFactors = FALSE
          )
        } else if ((is_g1_user && is_g2_ref) || (is_g1_ref && is_g2_user)) {
          if (is_g1_ref) {
            temp <- g1
            g1 <- g2
            g2 <- temp
            is_g1_user <- TRUE
            is_g2_ref <- TRUE
          }

          x1 <- df$score[df$group == g1]
          ref_row <- ref_stats[ref_stats$category == g2, , drop = FALSE]

          if (nrow(ref_row) == 0) {
            return(NULL)
          }

          data.frame(
            group_1 = g1,
            group_2 = g2,
            n1 = length(x1),
            n2 = NA_integer_,
            mean_diff = mean(x1) - ref_row$mean[1],
            median_diff = stats::median(x1) - ref_row$median[1],
            stringsAsFactors = FALSE
          )
        } else {
          NULL
        }
      })

      rows <- Filter(Negate(is.null), rows)
      if (length(rows) == 0) {
        return(data.frame())
      }

      out <- do.call(rbind, rows)
      out$mean_diff <- format_num(out$mean_diff)
      out$median_diff <- format_num(out$median_diff)

      names(out) <- c(
        tr("stats.pairwise.col.group1"),
        tr("stats.pairwise.col.group2"),
        tr("stats.pairwise.col.n1"),
        tr("stats.pairwise.col.n2"),
        tr("stats.pairwise.col.mean_diff"),
        tr("stats.pairwise.col.median_diff")
      )

      out
    })

    show_statistics_status_notification <- function() {
      if (!identical(active_tab(), "statistics")) {
        return()
      }

      notification_id <- session$ns("statistics_status_notification")
      user_df <- if (is.null(data)) NULL else data()
      state <- current_likert_state()

      if (!is.data.frame(user_df) || nrow(user_df) == 0 || ncol(user_df) == 0) {
        showNotification(
          tr("stats.notif.showing_reference"),
          type = "message",
          duration = 3,
          id = notification_id
        )
        return()
      }

      if (is.null(state) || is.null(state$scaled_scores)) {
        showNotification(
          tr("stats.notif.no_scores"),
          type = "warning",
          duration = 3,
          id = notification_id
        )
        return()
      }

      showNotification(
        tr("stats.notif.ready"),
        type = "message",
        duration = 3,
        id = notification_id
      )
    }

    observeEvent(active_tab(), {
      if (!identical(active_tab(), "statistics")) {
        return()
      }

      show_statistics_status_notification()
    }, ignoreInit = TRUE)

    observeEvent(list(data(), current_likert_state()), {
      if (!identical(active_tab(), "statistics")) {
        return()
      }

      show_statistics_status_notification()
    }, ignoreInit = TRUE)

    output$summary_statistics_ui <- renderUI({
      if (!has_user_data()) {
        return(build_table(reference_summary_statistics()))
      }

      state <- current_likert_state()
      if (is.null(state) || is.null(state$scaled_scores)) {
        return(div(class = "text-muted", tr("stats.ui.summary.no_scores")))
      }

      if (!length(selected_group_definitions())) {
        return(build_table(reference_summary_statistics()))
      }

      stats_df <- summary_statistics()
      build_table(stats_df)
    })

    output$pairwise_statistics_ui <- renderUI({
      if (!has_user_data()) {
        return(NULL)
      }

      state <- current_likert_state()
      if (is.null(state) || is.null(state$scaled_scores)) {
        return(div(class = "text-muted", tr("stats.ui.pairwise.no_scores")))
      }

      if (!length(selected_group_definitions())) {
        return(NULL)
      }

      pairwise_df <- pairwise_statistics()
      if (!is.data.frame(pairwise_df) || nrow(pairwise_df) == 0) {
        return(div(class = "text-muted", tr("stats.ui.pairwise.need_two")))
      }

      build_table(pairwise_df)
    })
  })
}