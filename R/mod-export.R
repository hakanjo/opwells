mod_export_ui <- function(id, lang = i18n_default_language) {
  ns <- NS(id)
  tr <- function(key, ...) i18n_t(lang, key, ...)

  tagList(
    h5(tr("export.ui.title")),
    helpText(tr("export.ui.help")),
    downloadButton(ns("download_scores"), tr("export.ui.download_csv")),
    downloadButton(ns("download_scores_xlsx"), tr("export.ui.download_xlsx")),
    uiOutput(ns("export_status"))
  )
}

mod_export_server <- function(id, data, likert_state, active_tab = NULL, lang = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    resolved_lang <- if (is.null(lang)) reactive(i18n_default_language) else lang
    tr <- function(key, ...) i18n_t(resolved_lang(), key, ...)

    current_selected_columns <- reactive({
      state <- likert_state()
      if (is.null(state) || is.null(state$selected_columns)) {
        return(character(0))
      }

      state$selected_columns
    })

    current_scaled_scores <- reactive({
      state <- likert_state()
      if (is.null(state)) {
        return(NULL)
      }

      state$scaled_scores
    })

    has_export_data <- reactive({
      current_data <- data()
      is.data.frame(current_data) && nrow(current_data) > 0
    })

    result_table_for_download <- function() {
      current_data <- data()
      req(is.data.frame(current_data), nrow(current_data) > 0)

      selected <- current_selected_columns()
      if (!length(selected)) {
        return(current_data)
      }

      current_scores <- current_scaled_scores()
      if (is.null(current_scores)) {
        current_scores <- compute_scaled_score(current_data, selected)
      }

      result_df <- current_data
      result_df[[tr("export.column.scaled_score")]] <- current_scores
      result_df
    }

    output$download_scores <- downloadHandler(
      filename = function() {
        paste0("OPWELLS-", format(Sys.Date(), "%Y-%m-%d"), ".csv")
      },
      content = function(file) {
        result_df <- result_table_for_download()
        # Write UTF-8 BOM first, then append CSV through a UTF-8 text connection.
        con_bin <- file(file, open = "wb")
        writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con_bin)
        close(con_bin)

        con_txt <- file(file, open = "a", encoding = "UTF-8")
        on.exit(close(con_txt), add = TRUE)
        utils::write.csv(result_df, file = con_txt, row.names = FALSE, na = "")
      }
    )

    output$download_scores_xlsx <- downloadHandler(
      filename = function() {
        paste0("OPWELLS-", format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
      },
      content = function(file) {
        result_df <- result_table_for_download()
        validate(
          need(requireNamespace("writexl", quietly = TRUE), tr("export.validation.writexl"))
        )

        writexl::write_xlsx(result_df, path = file)
      }
    )

    if (!is.null(active_tab)) {
      observeEvent(active_tab(), {
        if (!identical(active_tab(), "export")) {
          return()
        }

        notification_id <- ns("export_prompt")

        if (!has_export_data()) {
          showNotification(
            tr("export.notif.no_data"),
            type = "warning",
            duration = 3,
            id = notification_id
          )
          return()
        }

        if (!length(current_selected_columns())) {
          showNotification(
            tr("export.notif.no_cols"),
            type = "message",
            duration = 4,
            id = notification_id
          )
          return()
        }

        removeNotification(notification_id)
      }, ignoreInit = TRUE)
    }

    output$export_status <- renderUI({
      if (!has_export_data()) {
        return(div(class = "alert alert-info", tr("export.status.no_data")))
      }

      if (!length(current_selected_columns())) {
        return(div(class = "alert alert-warning", tr("export.status.no_cols")))
      }

      if (is.null(current_scaled_scores())) {
        return(div(class = "alert alert-info", tr("export.status.pending")))
      }

      div(class = "alert alert-success", tr("export.status.ready"))
    })
  })
}