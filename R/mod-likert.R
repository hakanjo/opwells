mod_likert_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h5("Likert Score Computation"),
    helpText("Select up to 10 columns containing Likert responses (0-4) to sum and scale."),
    uiOutput(ns("likert_selector_ui")),
    actionButton(ns("compute_scores"), "Compute Scaled Scores"),
    uiOutput(ns("score_status"))
  )
}

mod_likert_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    scores <- reactiveVal(NULL)
    likert_cols_selected <- reactiveVal(character(0))

    output$likert_selector_ui <- renderUI({
      cols <- names(data())
      if (length(cols) == 0) return(NULL)
      checkboxGroupInput(
        ns("likert_cols"),
        "Select Likert columns:",
        choices = cols,
        selected = likert_cols_selected()
      )
    })

    observeEvent(input$likert_cols, {
      likert_cols_selected(input$likert_cols)
    }, ignoreNULL = FALSE)

    # Reset scores whenever data changes (e.g. new upload, edit, or clear)
    observeEvent(data(), {
      scores(NULL)
    }, ignoreNULL = FALSE)

    observeEvent(input$compute_scores, {
      req(length(likert_cols_selected()) > 0)
      current_data <- data()
      validate(
        need(nrow(current_data) > 0, "No data loaded. Please paste or upload data first."),
        need(length(likert_cols_selected()) <= 10, "Please select at most 10 Likert columns.")
      )
      scores(compute_scaled_score(current_data, likert_cols_selected()))
    })

    output$score_status <- renderUI({
      if (!is.null(scores())) {
        div(
          class = "alert alert-success",
          "\u2713 Scaled scores computed."
        )
      }
    })

    scores
  })
}
