mod_statistics_ui <- function(id, lang = i18n_default_language) {
  ns <- NS(id)
  tr <- function(key, ...) i18n_t(lang, key, ...)
  tr_md <- function(key, ...) i18n_t_markdown(lang, key, ...)

  fluidRow(
    column(
      width = 3,
      helpText(tr_md("stats.help")),
      hr(),
      uiOutput(ns("group_selectors")),
      uiOutput(ns("reference_group_selector")),
      uiOutput(ns("include_total_toggle"))
    ),
    column(
      width = 9,
      h4(tr("stats.summary_title")),
      div(
        style = "margin-bottom: 10px;",
        downloadButton(ns("download_summary_csv"), tr("export.download_csv")),
        tags$span(style = "display: inline-block; width: 8px;"),
        downloadButton(ns("download_summary_xlsx"), tr("export.download_xlsx"))
      ),
      uiOutput(ns("summary_statistics_ui")),
      uiOutput(ns("pairwise_section_ui")),
      uiOutput(ns("explanation_ui")),
    )
  )
}

mod_statistics_server <- function(id, data = NULL, likert_state = NULL, active_tab = NULL, group_state = NULL, lang = NULL) {
  moduleServer(id, function(input, output, session) {
    resolved_lang <- if (is.null(lang)) reactive(i18n_default_language) else lang
    tr <- function(key, ...) i18n_t(resolved_lang(), key, ...)
    tr_md <- function(key, ...) i18n_t_markdown(resolved_lang(), key, ...)

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

    base_column_groups <- reactive({
      user_df <- if (is.null(data)) NULL else data()
      state <- current_likert_state()
      excluded <- if (is.null(state) || is.null(state$selected_columns)) character(0) else state$selected_columns

      plot_column_groups(user_df, exclude_cols = excluded)
    })

    combined_groups_for_choices <- reactive({
      if (!inherits(group_state, "reactivevalues")) {
        return(list())
      }

      value <- group_state$combined_groups
      if (is.list(value)) value else list()
    })

    column_groups <- reactive({
      plot_with_combined_group_choices(
        base_column_groups(),
        combined_groups_for_choices()
      )
    })

    ref_group_choices <- reactive(plot_reference_group_choices(ref_data(), lang = resolved_lang()))

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
    current_include_total <- group_controller$current_include_total
    current_reference_groups <- group_controller$current_reference_groups

    selected_group_definitions <- reactive({
      plot_build_selected_group_definitions(
        selections = selected_group_selections(),
        combined_groups = get_combined_groups(),
        include_total = current_include_total(),
        lang = resolved_lang(),
        include_all_combined = FALSE
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
        return(p(tr("stats.load_data_for_groups"), style = "color: #6b6b6b;"))
      }

      lapply(names(col_groups), function(col_nm) {
        label <- if (identical(col_nm, plot_combined_groups_column_key())) {
          tr("define_groups.defined_groups")
        } else {
          col_nm
        }

        selectizeInput(
          session$ns(paste0("grp_", col_nm)),
          label = label,
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
        label = tr("stats.reference_groups"),
        choices = choices,
        selected = current_reference_groups(),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })

    output$include_total_toggle <- renderUI({
      if (!has_user_data()) {
        return(NULL)
      }

      checkboxInput(
        session$ns("include_total"),
        label = tr("stats.include_total"),
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

      required_cols <- c("group", "mean", "sd", "q_1_6", "q_5_6", "n")
      if (!all(required_cols %in% names(ref_df))) {
        return(data.frame())
      }

      out <- data.frame(
        category = i18n_reference_group_labels(ref_df$group, lang = resolved_lang()),
        n = suppressWarnings(as.integer(round(as.numeric(ref_df$n)))),
        mean = suppressWarnings(as.numeric(ref_df$mean)),
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
      out$sd <- format_num(out$sd)
      out$range_low <- format_num(out$range_low)
      out$range_high <- format_num(out$range_high)

      names(out) <- c(
        tr("stats.summary.col.category"),
        tr("stats.summary.col.n"),
        tr("stats.summary.col.mean"),
        tr("stats.summary.col.sd"),
        tr("stats.summary.col.range_low"),
        tr("stats.summary.col.range_high")
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
          sd = stats::sd(x),
          range_low = as.numeric(stats::quantile(x, probs = 1 / 6, na.rm = TRUE, names = FALSE)),
          range_high = as.numeric(stats::quantile(x, probs = 5 / 6, na.rm = TRUE, names = FALSE)),
          stringsAsFactors = FALSE
        )
      })

      out <- do.call(rbind, rows)
      out$mean <- format_num(out$mean)
      out$sd <- format_num(out$sd)
      out$range_low <- format_num(out$range_low)
      out$range_high <- format_num(out$range_high)

      names(out) <- c(
        tr("stats.summary.col.category"),
        tr("stats.summary.col.n"),
        tr("stats.summary.col.mean"),
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

      build_pairwise_row <- function(group_1, group_2, n1, n2, mean1, mean2, p_value) {
        if (is.finite(mean1) && is.finite(mean2) && mean2 > mean1) {
          temp_group <- group_1
          group_1 <- group_2
          group_2 <- temp_group

          temp_n <- n1
          n1 <- n2
          n2 <- temp_n

          temp_mean <- mean1
          mean1 <- mean2
          mean2 <- temp_mean
        }

        data.frame(
          group_1 = group_1,
          group_2 = group_2,
          n1 = n1,
          n2 = n2,
          mean_diff = mean1 - mean2,
          p_value = p_value,
          stringsAsFactors = FALSE
        )
      }

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
          mean1 <- mean(x1)
          mean2 <- mean(x2)
          p_val <- calculate_ttest_pvalue(x1, x2)
          build_pairwise_row(
            group_1 = g1,
            group_2 = g2,
            n1 = length(x1),
            n2 = length(x2),
            mean1 = mean1,
            mean2 = mean2,
            p_value = p_val
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

          ref_mean <- ref_row$mean[1]
          ref_sd <- ref_row$sd[1]
          ref_n <- ref_row$n[1]
          p_val <- calculate_ttest_pvalue_from_summary(
            x = x1,
            ref_mean = ref_mean,
            ref_sd = ref_sd,
            ref_n = ref_n
          )

          build_pairwise_row(
            group_1 = g1,
            group_2 = g2,
            n1 = length(x1),
            n2 = suppressWarnings(as.integer(round(as.numeric(ref_n)))),
            mean1 = mean(x1),
            mean2 = ref_mean,
            p_value = p_val
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
      out$p_value <- format_p_value(out$p_value)

      names(out) <- c(
        tr("stats.pairwise.col.group1"),
        tr("stats.pairwise.col.group2"),
        tr("stats.pairwise.col.n1"),
        tr("stats.pairwise.col.n2"),
        tr("stats.pairwise.col.mean_diff"),
        tr("stats.pairwise.col.p_value")
      )

      out
    })

    summary_table_for_download <- reactive({
      if (!has_user_data()) {
        return(reference_summary_statistics())
      }

      state <- current_likert_state()
      if (is.null(state) || is.null(state$scaled_scores)) {
        return(data.frame())
      }

      if (!length(selected_group_definitions())) {
        return(reference_summary_statistics())
      }

      summary_statistics()
    })

    pairwise_table_for_download <- reactive({
      if (!has_user_data()) {
        return(data.frame())
      }

      state <- current_likert_state()
      if (is.null(state) || is.null(state$scaled_scores)) {
        return(data.frame())
      }

      if (!length(selected_group_definitions())) {
        return(data.frame())
      }

      pairwise_statistics()
    })

    output$pairwise_section_ui <- renderUI({
      pairwise_df <- pairwise_table_for_download()
      if (!is.data.frame(pairwise_df) || nrow(pairwise_df) == 0) {
        return(NULL)
      }

      tagList(
        h4(tr("stats.pairwise_title")),
        div(
          style = "margin-bottom: 10px;",
          downloadButton(session$ns("download_pairwise_csv"), tr("export.download_csv")),
          tags$span(style = "display: inline-block; width: 8px;"),
          downloadButton(session$ns("download_pairwise_xlsx"), tr("export.download_xlsx"))
        ),
        build_table(pairwise_df)
      )
    })

    summary_filename_suffix <- reactive({
      if (identical(resolved_lang(), "sv")) "statistik-kategorier" else "statistics-categories"
    })

    pairwise_filename_suffix <- reactive({
      if (identical(resolved_lang(), "sv")) "parvisa-jamforelser-mellan-grupper" else "pairwise-comparisons-between-groups"
    })

    output$download_summary_csv <- downloadHandler(
      filename = function() {
        paste0("OPWELLS-", format(Sys.Date(), "%Y-%m-%d"), "-", summary_filename_suffix(), ".csv")
      },
      content = function(file) {
        result_df <- summary_table_for_download()
        validate(need(is.data.frame(result_df) && nrow(result_df) > 0, tr("stats.download.no_summary_data")))
        # Write UTF-8 BOM for Excel compatibility
        con_bin <- file(file, open = "wb")
        writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con_bin)
        close(con_bin)
        con_txt <- file(file, open = "a", encoding = "UTF-8")
        on.exit(close(con_txt), add = TRUE)
        utils::write.csv(result_df, file = con_txt, row.names = FALSE, na = "")
      }
    )

    output$download_summary_xlsx <- downloadHandler(
      filename = function() {
        paste0("OPWELLS-", format(Sys.Date(), "%Y-%m-%d"), "-", summary_filename_suffix(), ".xlsx")
      },
      content = function(file) {
        result_df <- summary_table_for_download()
        validate(need(is.data.frame(result_df) && nrow(result_df) > 0, tr("stats.download.no_summary_data")))
        validate(need(requireNamespace("writexl", quietly = TRUE), tr("export.validation.writexl")))

        writexl::write_xlsx(result_df, path = file)
      }
    )

    output$download_pairwise_csv <- downloadHandler(
      filename = function() {
        paste0("OPWELLS-", format(Sys.Date(), "%Y-%m-%d"), "-", pairwise_filename_suffix(), ".csv")
      },
      content = function(file) {
        result_df <- pairwise_table_for_download()
        validate(need(is.data.frame(result_df) && nrow(result_df) > 0, tr("stats.download.no_pairwise_data")))
        # Write UTF-8 BOM for Excel compatibility
        con_bin <- file(file, open = "wb")
        writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con_bin)
        close(con_bin)
        con_txt <- file(file, open = "a", encoding = "UTF-8")
        on.exit(close(con_txt), add = TRUE)
        utils::write.csv(result_df, file = con_txt, row.names = FALSE, na = "")
      }
    )

    output$download_pairwise_xlsx <- downloadHandler(
      filename = function() {
        paste0("OPWELLS-", format(Sys.Date(), "%Y-%m-%d"), "-", pairwise_filename_suffix(), ".xlsx")
      },
      content = function(file) {
        result_df <- pairwise_table_for_download()
        validate(need(is.data.frame(result_df) && nrow(result_df) > 0, tr("stats.download.no_pairwise_data")))
        validate(need(requireNamespace("writexl", quietly = TRUE), tr("export.validation.writexl")))

        writexl::write_xlsx(result_df, path = file)
      }
    )

    output$summary_statistics_ui <- renderUI({
      if (!has_user_data()) {
        return(build_table(reference_summary_statistics()))
      }

      state <- current_likert_state()
      if (is.null(state) || is.null(state$scaled_scores)) {
        return(div(class = "text-muted", tr("stats.summary.no_scores")))
      }

      if (!length(selected_group_definitions())) {
        return(build_table(reference_summary_statistics()))
      }

      stats_df <- summary_statistics()
      build_table(stats_df)
    })

    output$explanation_ui <- renderUI({
      show_reference_explanation <- !has_user_data() ||
        !length(selected_group_definitions()) ||
        length(current_reference_groups()) > 0

      tagList(
        div(
          style = "margin-bottom: 10px;",
          tr_md("stats.explanation")
        ),
        if (show_reference_explanation) {
          div(
            style = "margin-bottom: 10px;",
            tr_md("stats.explanation.ref_group")
          )
        }
      )
    })

  })
}
