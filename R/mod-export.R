mod_export_ui <- function(id, lang = i18n_default_language) {
  ns <- NS(id)
  tr <- function(key, ...) i18n_t(lang, key, ...)
  tr_md <- function(key, ...) i18n_t_markdown(lang, key, ...)

  tagList(
    h4(tr("export.title")),
    helpText(tr("export.help")),
    downloadButton(ns("download_scores"), tr("export.download_csv")),
    downloadButton(ns("download_scores_xlsx"), tr("export.download_xlsx")),
    h5(tr("export.share.title")),
    helpText(tr_md("export.share.invitation")),
    helpText(tr_md("export.share.attach_instruction"))
  )
}

mod_export_server <- function(id, data, likert_state, lang = NULL) {
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

        writexl::write_xlsx(result_df, path = file)
      }
    )

    output$export_status <- renderUI({
      if (!has_export_data()) {
        return(div(class = "alert alert-info", tr("export.status.no_data")))
      }

      if (!length(current_selected_columns())) {
        return(div(class = "alert alert-warning", tr("export.status.no_cols")))
      }

      if (is.null(current_scaled_scores())) {
        return(NULL)
      }

      NULL
    })

    last_export_notification_state <- reactiveVal(NULL)

    observe({
      if (!has_export_data()) {
        last_export_notification_state(NULL)
        return()
      }

      if (!length(current_selected_columns())) {
        last_export_notification_state(NULL)
        return()
      }

      current_state <- !is.null(current_scaled_scores())
      last_state <- last_export_notification_state()

      if (current_state && !isTRUE(last_state)) {
        showNotification(tr("export.status.ready"), type = "message", duration = 15)
      }

      last_export_notification_state(current_state)
    })
  })
}