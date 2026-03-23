mod_likert_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h5("Likert Score Computation"),
    helpText("Click column names in the preview to include/exclude them from the score calculation (max 10 columns)."),
    tags$style(HTML(
      paste(
        ".likert-preview-table { width: 100%; border-collapse: collapse; }",
        ".likert-preview-table th, .likert-preview-table td { border: 1px solid #ddd; padding: 6px; font-size: 0.9em; }",
        ".likert-col-link { color: #1f5ea8; text-decoration: none; font-weight: 600; }",
        ".likert-col-link:hover { text-decoration: underline; }",
        ".likert-col-link.selected { color: #0b5e2b; }",
        ".likert-score-col { background-color: #f5f9ff; }"
      )
    )),
    uiOutput(ns("likert_preview_ui")),
    actionButton(ns("compute_scores"), "Compute Scaled Scores"),
    downloadButton(ns("download_scores"), "Download Result Table (.csv)"),
    downloadButton(ns("download_scores_xlsx"), "Download Result Table (.xlsx)"),
    uiOutput(ns("score_status"))
  )
}

mod_likert_server <- function(id, data) {
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

    output$likert_preview_ui <- renderUI({
      current_data <- data()
      validate(
        need(ncol(current_data) > 0 && nrow(current_data) > 0, "No data loaded. Please paste or upload data first.")
      )

      preview <- head(current_data, 20)
      data_cols <- names(preview)
      selected_cols <- likert_cols_selected()

      if (!is.null(scores())) {
        preview[["Scaled score"]] <- head(scores(), nrow(preview))
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

      if ("Scaled score" %in% names(preview)) {
        header_cells <- c(header_cells, list(tags$th(class = "likert-score-col", "Scaled score")))
      }

      body_rows <- lapply(seq_len(nrow(preview)), function(row_i) {
        row_cells <- lapply(seq_along(names(preview)), function(col_i) {
          col_nm <- names(preview)[[col_i]]
          as_display_cell(preview[[col_i]][[row_i]], is_score_col = identical(col_nm, "Scaled score"))
        })
        tags$tr(row_cells)
      })

      tagList(
        div(
          tags$strong("Selected columns: "),
          if (length(selected_cols)) paste(selected_cols, collapse = ", ") else "None"
        ),
        br(),
        tags$table(
          class = "likert-preview-table",
          tags$thead(tags$tr(header_cells)),
          tags$tbody(body_rows)
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
        auto_cols <- grep("^OPWELLS", cols, value = TRUE, ignore.case = TRUE)
        selected_after_prune <- unique(c(selected_after_prune, auto_cols))

        if (length(selected_after_prune) > 10) {
          selected_after_prune <- selected_after_prune[seq_len(10)]
          showNotification(
            "More than 10 columns matched current selection rules. Keeping the first 10.",
            type = "warning"
          )
        }
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

          if (length(selected) >= 10) {
            showNotification("You can select at most 10 Likert columns.", type = "warning")
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
      validate(
        need(nrow(current_data) > 0, "No data loaded. Please paste or upload data first."),
        need(length(likert_cols_selected()) <= 10, "Please select at most 10 Likert columns.")
      )
      scores(compute_scaled_score(current_data, likert_cols_selected()))
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
        if (length(selected) > 10) {
          stop("Please select at most 10 Likert columns.", call. = FALSE)
        }

        current_scores <- compute_scaled_score(current_data, selected)
        scores(current_scores)
      }

      result_df <- current_data
      result_df[["Scaled score"]] <- current_scores
      result_df
    }

    output$download_scores <- downloadHandler(
      filename = function() {
        paste0("likert_scores_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
      },
      content = function(file) {
        result_df <- result_table_for_download()

        utils::write.csv(result_df, file = file, row.names = FALSE, na = "")
      }
    )

    output$download_scores_xlsx <- downloadHandler(
      filename = function() {
        paste0("likert_scores_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
      },
      content = function(file) {
        result_df <- result_table_for_download()
        validate(
          need(requireNamespace("writexl", quietly = TRUE), "Package 'writexl' is required for xlsx export.")
        )

        writexl::write_xlsx(result_df, path = file)
      }
    )

    output$score_status <- renderUI({
      if (!is.null(scores())) {
        n_selected <- length(likert_cols_selected())

        if (n_selected < 10) {
          div(
            class = "alert alert-warning",
            paste0(
              "Scaled scores were computed using ",
              n_selected,
              " selected columns. Up to 10 columns are recommended."
            )
          )
        } else {
          div(
            class = "alert alert-success",
            "\u2713 Scaled scores computed and shown in the preview table."
          )
        }
      } else if (!length(likert_cols_selected())) {
        div(
          class = "alert alert-info",
          "Click one or more preview columns to select them for scoring."
        )
      }
    })

    scores
  })
}
