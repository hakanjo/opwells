mod_statistics_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    h5("Statistiska jämförelser för användardata"),
    helpText("För varje kategori visas medel, median, standardavvikelse och tvåtredjedelsintervall (16,7:e till 83,3:e percentilen). Parvisa gruppjämförelser visas under tabellen."),
    fluidRow(
      column(
        width = 4,
        p("Välj en eller flera grupper i en eller flera kolumner. Valda grupper jämförs mot varandra i tabellerna till höger."),
        uiOutput(ns("group_selectors")),
        hr(),
        p(strong("Kombinera grupper"), style = "margin-top: 20px;"),
        p("Välj ett namn och klicka på 'Lägg till kombinerad grupp' för att kombinera de valda grupperna.", style = "font-size: 0.9em; color: #6b6b6b;"),
        textInput(ns("combined_group_label"), label = "Namn på kombinerad grupp", placeholder = "T.ex. 'Kvinnor 2024-2025'"),
        actionButton(ns("add_combined_group"), "Lägg till kombinerad grupp", class = "btn-primary"),
        uiOutput(ns("combined_groups_list")),
        uiOutput(ns("include_total_toggle"))
      ),
      column(
        width = 8,
        h4("Deskriptiv statistik per kategori"),
        uiOutput(ns("summary_statistics_ui")),
        h4("Parvisa jämförelser mellan grupper"),
        uiOutput(ns("pairwise_statistics_ui"))
      )
    )
  )
}

mod_statistics_server <- function(id, data = NULL, likert_state = NULL, active_tab = NULL, group_state = NULL) {
  moduleServer(id, function(input, output, session) {
    use_shared_group_state <- inherits(group_state, "reactivevalues")
    is_module_active <- reactive({
      if (is.null(active_tab)) {
        return(TRUE)
      }

      identical(active_tab(), "statistics")
    })

    column_groups <- reactive({
      user_df <- if (is.null(data)) NULL else data()
      state <- current_likert_state()
      excluded <- if (is.null(state) || is.null(state$selected_columns)) character(0) else state$selected_columns

      plot_column_groups(user_df, exclude_cols = excluded)
    })

    local_combined_groups <- reactiveVal(list())

    get_combined_groups <- reactive({
      if (!use_shared_group_state) {
        return(local_combined_groups())
      }

      value <- group_state$combined_groups
      if (is.list(value)) value else list()
    })

    set_combined_groups <- function(value) {
      if (use_shared_group_state) {
        group_state$combined_groups <- value
      } else {
        local_combined_groups(value)
      }
    }

    current_include_total <- reactive({
      if (!use_shared_group_state) {
        return(isTRUE(input$include_total))
      }

      isTRUE(group_state$include_total)
    })

    observeEvent(input$include_total, {
      if (!use_shared_group_state) {
        return()
      }

      if (!isTRUE(is_module_active())) {
        return()
      }

      value <- isTRUE(input$include_total)
      if (!identical(isTRUE(group_state$include_total), value)) {
        group_state$include_total <- value
      }
    }, ignoreInit = TRUE)

    selected_group_selections <- reactive({
      col_groups <- column_groups()
      if (!length(col_groups)) {
        return(list())
      }

      if (use_shared_group_state) {
        return(plot_sanitize_group_selections(group_state$selections, col_groups))
      }

      plot_read_group_selections_from_input(input, col_groups)
    })

    input_group_selections <- reactive({
      col_groups <- column_groups()
      plot_read_group_selections_from_input(input, col_groups)
    })

    observeEvent(input_group_selections(), {
      if (!use_shared_group_state) {
        return()
      }

      if (!isTRUE(is_module_active())) {
        return()
      }

      col_groups <- column_groups()
      if (!length(col_groups)) {
        if (!identical(group_state$selections, list())) {
          group_state$selections <- list()
        }
        return()
      }

      input_selections <- input_group_selections()
      if (!identical(input_selections, plot_sanitize_group_selections(group_state$selections, col_groups))) {
        group_state$selections <- input_selections
      }
    }, ignoreInit = TRUE)

    selected_group_definitions <- reactive({
      plot_build_selected_group_definitions(
        selections = selected_group_selections(),
        combined_groups = get_combined_groups(),
        include_total = current_include_total()
      )
    })

    format_num <- function(x, digits = 2) {
      ifelse(is.na(x), "", format(round(x, digits), nsmall = digits, trim = TRUE))
    }

    build_table <- function(df) {
      if (!is.data.frame(df) || nrow(df) == 0 || ncol(df) == 0) {
        return(tags$p("Ingen statistik tillgänglig."))
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
        return(p("Ladda data för att välja grupper.", style = "color: #6b6b6b;"))
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

    observe({
      if (!use_shared_group_state) {
        return()
      }

      updateCheckboxInput(session, "include_total", value = current_include_total())
    })

    observeEvent(input$add_combined_group, {
      label <- trimws(input$combined_group_label)
      if (!nzchar(label)) {
        showNotification("Ange ett namn för den kombinerade gruppen", type = "warning")
        return()
      }

      current_selections <- selected_group_selections()
      if (!length(current_selections)) {
        showNotification("Välj minst en grupp för att kombinera", type = "warning")
        return()
      }

      source_defs <- plot_parse_group_definitions(current_selections)
      if (!length(source_defs)) {
        showNotification("Kunde inte skapa kombinerad grupp", type = "error")
        return()
      }

      combined_group <- plot_create_combined_group(source_defs, label)
      current_combined <- get_combined_groups()
      set_combined_groups(c(current_combined, list(combined_group)))

      updateTextInput(session, "combined_group_label", value = "")
      showNotification(sprintf("Kombinerad grupp '%s' skapad!", label), type = "message")
    })

    created_observers <- reactiveVal(character(0))

    output$combined_groups_list <- renderUI({
      combined <- get_combined_groups()
      if (!length(combined)) {
        return(NULL)
      }

      ns <- session$ns
      tagList(
        p(strong(sprintf("Kombinerade grupper (%d)", length(combined))), style = "margin-top: 15px; margin-bottom: 5px;"),
        lapply(seq_along(combined), function(i) {
          group <- combined[[i]]
          label <- group$label
          safe_id <- gsub("[^a-zA-Z0-9_]", "_", label)

          tags$div(
            style = "background-color: #f5f5f5; padding: 8px; margin: 5px 0; border-radius: 4px; display: flex; justify-content: space-between; align-items: center;",
            tags$span(label),
            actionButton(
              ns(paste0("remove_combined_group_", safe_id)),
              "Ta bort",
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
                showNotification(sprintf("Kombinerad grupp '%s' borttagen", removed_label), type = "message")
              }
            }, ignoreInit = TRUE)
          })

          created_observers(c(created_observers(), button_id))
        }
      }
    })

    output$include_total_toggle <- renderUI({
      col_groups <- column_groups()
      selections <- selected_group_selections()

      has_selection <- any(vapply(names(col_groups), function(col) {
        length(plot_trim_non_empty(selections[[col]])) > 0
      }, logical(1)))

      has_combined <- length(get_combined_groups()) > 0
      if (!has_selection && !has_combined) {
        return(NULL)
      }

      checkboxInput(
        session$ns("include_total"),
        label = "Inkludera totalt",
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

    summary_statistics <- reactive({
      df <- comparison_data()
      groups <- sort(unique(df$group))

      rows <- lapply(groups, function(g) {
        x <- df$score[df$group == g]
        data.frame(
          Kategori = g,
          N = length(x),
          Medel = mean(x),
          Median = stats::median(x),
          SD = stats::sd(x),
          `2/3 nedre` = as.numeric(stats::quantile(x, probs = 1 / 6, na.rm = TRUE, names = FALSE)),
          `2/3 övre` = as.numeric(stats::quantile(x, probs = 5 / 6, na.rm = TRUE, names = FALSE)),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      })

      out <- do.call(rbind, rows)
      out$Medel <- format_num(out$Medel)
      out$Median <- format_num(out$Median)
      out$SD <- format_num(out$SD)
      out$`2/3 nedre` <- format_num(out$`2/3 nedre`)
      out$`2/3 övre` <- format_num(out$`2/3 övre`)
      out
    })

    pairwise_statistics <- reactive({
      df <- comparison_data()
      groups <- sort(unique(df$group))

      if (length(groups) < 2) {
        return(data.frame())
      }

      pairs <- utils::combn(groups, 2, simplify = FALSE)
      rows <- lapply(pairs, function(grp_pair) {
        g1 <- grp_pair[[1]]
        g2 <- grp_pair[[2]]
        x1 <- df$score[df$group == g1]
        x2 <- df$score[df$group == g2]

        data.frame(
          `Grupp 1` = g1,
          `Grupp 2` = g2,
          N1 = length(x1),
          N2 = length(x2),
          `Skillnad i medel` = mean(x1) - mean(x2),
          `Skillnad i median` = stats::median(x1) - stats::median(x2),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      })

      out <- do.call(rbind, rows)
      out$`Skillnad i medel` <- format_num(out$`Skillnad i medel`)
      out$`Skillnad i median` <- format_num(out$`Skillnad i median`)
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
          "Ingen användardata laddad. Ladda data i fliken Ladda upp data för att visa jämförelser.",
          type = "warning",
          duration = 3,
          id = notification_id
        )
        return()
      }

      if (is.null(state) || is.null(state$scaled_scores)) {
        showNotification(
          "Skalad poäng saknas. Beräkna poäng i dataflödet för att visa statistiska jämförelser.",
          type = "warning",
          duration = 3,
          id = notification_id
        )
        return()
      }

      showNotification(
        "Visar statistiska jämförelser för användardata.",
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
      state <- current_likert_state()
      if (is.null(state) || is.null(state$scaled_scores)) {
        return(div(class = "text-muted", "Skalad poäng saknas. Beräkna poäng för att visa statistik per kategori."))
      }

      if (!length(selected_group_definitions())) {
        return(div(class = "text-muted", "Välj minst en grupp för att visa statistik per kategori."))
      }

      stats_df <- summary_statistics()
      build_table(stats_df)
    })

    output$pairwise_statistics_ui <- renderUI({
      state <- current_likert_state()
      if (is.null(state) || is.null(state$scaled_scores)) {
        return(div(class = "text-muted", "Skalad poäng saknas. Beräkna poäng för att visa parvisa jämförelser."))
      }

      if (!length(selected_group_definitions())) {
        return(div(class = "text-muted", "Välj minst två grupper för att visa parvisa jämförelser."))
      }

      pairwise_df <- pairwise_statistics()
      if (!is.data.frame(pairwise_df) || nrow(pairwise_df) == 0) {
        return(div(class = "text-muted", "Minst två grupper med data krävs för parvisa jämförelser."))
      }

      build_table(pairwise_df)
    })
  })
}