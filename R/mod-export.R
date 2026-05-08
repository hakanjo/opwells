mod_export_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h5("Export av resultat"),
    helpText("Ladda ner användardata med kolumnen 'Skalad po\u00e4ng', automatiskt ber\u00e4knad fr\u00e5n uppladdad fil."),
    downloadButton(ns("download_scores"), "Ladda ner resultattabell (.csv)"),
    downloadButton(ns("download_scores_xlsx"), "Ladda ner resultattabell (.xlsx)"),
    uiOutput(ns("export_status"))
  )
}

mod_export_server <- function(id, data, likert_state, active_tab = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
      result_df[["Skalad poäng"]] <- current_scores
      result_df
    }

    output$download_scores <- downloadHandler(
      filename = function() {
        paste0("OPWELLS-", format(Sys.Date(), "%Y-%m-%d"), ".csv")
      },
      content = function(file) {
        result_df <- result_table_for_download()
        utils::write.csv(result_df, file = file, row.names = FALSE, na = "")
      }
    )

    output$download_scores_xlsx <- downloadHandler(
      filename = function() {
        paste0("OPWELLS-", format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
      },
      content = function(file) {
        result_df <- result_table_for_download()
        validate(
          need(requireNamespace("writexl", quietly = TRUE), "Paketet 'writexl' krävs för xlsx-export.")
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
            "Ingen användardata laddad. Ladda data i fliken Ladda upp data för att kunna exportera.",
            type = "warning",
            duration = 3,
            id = notification_id
          )
          return()
        }

        if (!length(current_selected_columns())) {
          showNotification(
            "Inga OPWELLS/fr\u00e5gefr\u00e5gor hittades i den uppladdade filen. Export kommer att inneh\u00e5lla originaldata utan skalad po\u00e4ng.",
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
        return(div(class = "alert alert-info", "Ladda upp data först för att aktivera export."))
      }

      if (!length(current_selected_columns())) {
        return(div(class = "alert alert-warning", "Inga OPWELLS/fr\u00e5gefr\u00e5gor hittades i filen. Exporten inneh\u00e5ller originaldata utan skalad po\u00e4ng."))
      }

      if (is.null(current_scaled_scores())) {
        return(div(class = "alert alert-info", "Skalad po\u00e4ng ber\u00e4knas automatiskt vid uppladdning av fil med matchande kolumnnamn."))
      }

      div(class = "alert alert-success", "Exporten inkluderar aktuell skalad poäng.")
    })
  })
}
