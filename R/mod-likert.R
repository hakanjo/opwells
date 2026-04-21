mod_likert_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h5("Beräkning av poäng"),
    helpText("Klicka på kolumnnamn i förhandsgranskningen för att inkludera/exkludera dem från poängberäkningen."),
    tags$style(HTML(
      paste(
        ".likert-preview-table { width: 100%; border-collapse: collapse; }",
        ".likert-preview-table th, .likert-preview-table td { border: 1px solid #ddd; padding: 6px; font-size: 0.9em; }",
        ".likert-preview-table th { position: sticky; top: 0; background-color: #fff; z-index: 1; }",
        ".likert-col-link { color: #1f5ea8; text-decoration: none; font-weight: 600; }",
        ".likert-col-link:hover { text-decoration: underline; }",
        ".likert-col-link.selected { color: #0b5e2b; }",
        ".likert-score-col { background-color: #f5f9ff; }"
      )
    )),
    uiOutput(ns("likert_preview_ui")),
    actionButton(ns("compute_scores"), "Beräkna skalad poäng"),
    downloadButton(ns("download_scores"), "Ladda ner resultattabell (.csv)"),
    downloadButton(ns("download_scores_xlsx"), "Ladda ner resultattabell (.xlsx)"),
    uiOutput(ns("score_status"))
  )
}

mod_likert_server <- function(id, data, active_tab = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    scores <- reactiveVal(NULL)
    likert_cols_selected <- reactiveVal(character(0))
    previous_col_names <- reactiveVal(NULL)
    col_click_observers <- list()

    as_display_cell <- function(x, is_score_col = FALSE) {
      val <- if (is.na(x) || identical(x, "")) "" else as.character(x)
      td_class <- if (is_score_col) "likert-score-col" else NULL
      tags$td(class = td_class, val)
    }

    has_likert_data <- reactive({
      current_data <- data()
      is.data.frame(current_data) && ncol(current_data) > 0 && nrow(current_data) > 0
    })

    observeEvent(active_tab(), {
      if (!identical(active_tab(), "likert")) {
        return()
      }

      notification_id <- ns("likert_selection_prompt")

      if (!has_likert_data()) {
        showNotification(
          "Ingen användardata laddad. Ladda data i fliken Ladda data för att beräkna poäng.",
          type = "warning",
          duration = 3,
          id = notification_id
        )
        return()
      }

      if (is.null(scores())) {
        showNotification(
          "Klicka på en eller flera förhandsgranskningskolumner för att välja dem för beräkning.",
          type = "message",
          duration = 3,
          id = notification_id
        )
        return()
      }

      removeNotification(notification_id)
    }, ignoreInit = TRUE)

    output$likert_preview_ui <- renderUI({
      current_data <- data()
      if (!has_likert_data()) {
        return(NULL)
      }

      preview <- current_data
      data_cols <- names(preview)
      selected_cols <- likert_cols_selected()

      if (!is.null(scores())) {
        preview[["Skalad poäng"]] <- scores()
      }

      header_cells <- lapply(seq_along(data_cols), function(i) {
        col_nm <- data_cols[[i]]
        link_classes <- paste(
          "likert-col-link",
          if (col_nm %in% selected_cols) "selected" else "",
          collapse = " "
        )

        tags$th(
          actionLink(
            inputId = ns(paste0("select_col_", i)),
            label = col_nm,
            class = link_classes
          )
        )
      })

      if ("Skalad poäng" %in% names(preview)) {
        header_cells <- c(header_cells, list(tags$th(class = "likert-score-col", "Skalad poäng")))
      }

      body_rows <- lapply(seq_len(nrow(preview)), function(row_i) {
        row_cells <- lapply(seq_along(names(preview)), function(col_i) {
          col_nm <- names(preview)[[col_i]]
          as_display_cell(preview[[col_i]][[row_i]], is_score_col = identical(col_nm, "Skalad poäng"))
        })
        tags$tr(row_cells)
      })

      tagList(
        div(
          tags$strong("Valda kolumner: "),
          if (length(selected_cols)) paste(selected_cols, collapse = ", ") else "Ingen"
        ),
        br(),
        div(
          style = "overflow-x: auto; max-height: 600px; overflow-y: auto;",
          tags$table(
            class = "likert-preview-table",
            tags$thead(tags$tr(header_cells)),
            tags$tbody(body_rows)
          )
        )
      )
    })

    observeEvent(data(), {
      current_data <- data()
      had_scores <- !is.null(scores())

      if (length(col_click_observers)) {
        lapply(col_click_observers, function(obs) obs$destroy())
      }

      cols <- names(current_data)
      if (!length(cols)) {
        likert_cols_selected(character(0))
        previous_col_names(NULL)
        scores(NULL)
        col_click_observers <<- list()
        return()
      }

      old_cols <- previous_col_names()
      headers_changed <- is.null(old_cols) || !identical(old_cols, cols)

      # Keep only still-existing columns selected after upload/edit changes.
      selected_after_prune <- intersect(likert_cols_selected(), cols)

      if (headers_changed) {
        auto_cols <- cols[
          grepl("^OPWELLS", cols, ignore.case = TRUE) |
          grepl("F(?:10|[1-9])(?![0-9])", cols, ignore.case = TRUE, perl = TRUE) | 
          grepl("Q(?:10|[1-9])(?![0-9])", cols, ignore.case = TRUE, perl = TRUE)
        ]
        selected_after_prune <- unique(c(selected_after_prune, auto_cols))
      }

      likert_cols_selected(selected_after_prune)
      previous_col_names(cols)
      selected_cols <- likert_cols_selected()

      col_click_observers <<- lapply(seq_along(cols), function(i) {
        col_nm <- cols[[i]]
        observeEvent(input[[paste0("select_col_", i)]], {
          selected <- likert_cols_selected()

          if (col_nm %in% selected) {
            likert_cols_selected(setdiff(selected, col_nm))
            return()
          }

          likert_cols_selected(c(selected, col_nm))
        }, ignoreInit = TRUE)
      })

      # If scores were already computed, keep them in sync with edited/uploaded data.
      if (had_scores && length(selected_cols) > 0) {
        scores(compute_scaled_score(current_data, selected_cols))
      } else {
        scores(NULL)
      }
    }, ignoreNULL = FALSE)

    # Reset scores when selection changes to avoid stale values in preview.
    observeEvent(likert_cols_selected(), {
      scores(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$compute_scores, {
      req(length(likert_cols_selected()) > 0)
      current_data <- data()
      if (!has_likert_data()) {
        showNotification(
          "Ingen data laddad. Klistra in eller ladda upp data först.",
          type = "warning",
          duration = 3,
          id = ns("likert_no_data")
        )
        return()
      }
      scores(compute_scaled_score(current_data, likert_cols_selected()))

      n_selected <- length(likert_cols_selected())
      warning_notification_id <- ns("score_status_warning")
      if (n_selected != 10) {
        showNotification(
          paste0(
            "Skalad poäng beräknades med ",
            n_selected,
            " valda kolumner. Exakt 10 kolumner rekommenderas."
          ),
          type = "warning",
          duration = 5,
          id = warning_notification_id
        )
      } else {
        removeNotification(warning_notification_id)
      }
    })

    result_table_for_download <- function() {
      current_data <- data()
      req(nrow(current_data) > 0)

      selected <- likert_cols_selected()
      if (!length(selected)) {
        return(current_data)
      }

      current_scores <- scores()
      if (is.null(current_scores)) {
        current_scores <- compute_scaled_score(current_data, selected)
        scores(current_scores)
      }

      result_df <- current_data
      result_df[["Skalad poäng"]] <- current_scores
      result_df
    }

    output$download_scores <- downloadHandler(
      filename = function() {
        paste0("scores-", format(Sys.Date(), "%Y-%m-%d"), ".csv")
      },
      content = function(file) {
        result_df <- result_table_for_download()

        utils::write.csv(result_df, file = file, row.names = FALSE, na = "")
      }
    )

    output$download_scores_xlsx <- downloadHandler(
      filename = function() {
        paste0("scores-", format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
      },
      content = function(file) {
        result_df <- result_table_for_download()
        validate(
          need(requireNamespace("writexl", quietly = TRUE), "Paketet 'writexl' krävs för xlsx-export.")
        )

        writexl::write_xlsx(result_df, path = file)
      }
    )

    observeEvent(list(scores(), length(likert_cols_selected())), {
      notification_id <- ns("score_status_success")

      if (is.null(scores()) || length(likert_cols_selected()) != 10) {
        removeNotification(notification_id)
        return()
      }

      showNotification(
        "\u2713 Skalad poäng beräknad och visad i förhandsgranskningstabellen.",
        type = "message",
        duration = 3,
        id = notification_id
      )
    }, ignoreInit = TRUE)

    output$score_status <- renderUI({
      if (has_likert_data() && !length(likert_cols_selected())) {
        div(
          class = "alert alert-info",
          "Klicka på en eller flera förhandsgranskningskolumner för att välja dem för beräkning."
        )
      }
    })

    scores
  })
}
