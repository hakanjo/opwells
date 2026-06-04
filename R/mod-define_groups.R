mod_define_groups_ui <- function(id, lang = i18n_default_language) {
  ns <- NS(id)
  tr <- function(key, ...) i18n_t(lang, key, ...)
  tr_md <- function(key, ...) i18n_t_markdown(lang, key, ...)
  
  fluidRow(
    column(
      width = 3,
      helpText(tr_md("define_groups.help")),
      hr(),
      uiOutput(ns("group_selectors")),
      uiOutput(ns("combined_group_controls"))
    ),
    column(
      width = 9,
      uiOutput(ns("combined_groups_list"))
    )
  )
}

mod_define_groups_server <- function(id, data = NULL, likert_state = NULL, group_state = NULL, lang = NULL) {
  moduleServer(id, function(input, output, session) {
    resolved_lang <- if (is.null(lang)) reactive(i18n_default_language) else lang
    tr <- function(key, ...) i18n_t(resolved_lang(), key, ...)

    use_shared_group_state <- inherits(group_state, "reactivevalues")
    local_combined_groups <- reactiveVal(list())

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

    column_groups <- reactive({
      user_df <- user_data()
      state <- current_likert_state()
      excluded <- if (is.null(state) || is.null(state$selected_columns)) character(0) else state$selected_columns

      plot_column_groups(user_df, exclude_cols = excluded)
    })

    get_combined_groups <- reactive({
      if (use_shared_group_state) {
        value <- group_state$combined_groups
        return(if (is.list(value)) value else list())
      }

      local_combined_groups()
    })

    set_combined_groups <- function(value) {
      if (use_shared_group_state) {
        group_state$combined_groups <- value
      } else {
        local_combined_groups(value)
      }
    }

    selected_group_selections <- reactive({
      plot_read_group_selections_from_input(input, column_groups())
    })

    observe({
      col_groups <- column_groups()
      for (col_nm in names(col_groups)) {
        input_id <- paste0("grp_", col_nm)
        choices <- col_groups[[col_nm]]
        current <- isolate(input[[input_id]])
        valid <- current[current %in% choices]

        current_norm <- sort(unique(plot_trim_non_empty(current)))
        valid_norm <- sort(unique(plot_trim_non_empty(valid)))

        if (identical(current_norm, valid_norm)) {
          next
        }

        freezeReactiveValue(input, input_id)
        updateSelectizeInput(session, input_id, choices = choices, selected = valid, server = TRUE)
      }
    })

    output$group_selectors <- renderUI({
      col_groups <- column_groups()
      if (!length(col_groups)) {
        return(p(tr("define_groups.load_data_for_groups"), style = "color: #6b6b6b;"))
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

    output$combined_group_controls <- renderUI({
      if (is.null(user_data())) {
        return(NULL)
      }

      tagList(
        textInput(
          session$ns("combined_group_label"),
          label = tr("define_groups.combine.label"),
          placeholder = tr("define_groups.combine.placeholder")
        ),
        actionButton(
          session$ns("add_combined_group"),
          tr("define_groups.combine.add"),
          class = "btn-primary"
        )
      )
    })

    observeEvent(input$add_combined_group, {
      if (is.null(user_data())) {
        return()
      }

      label <- trimws(input$combined_group_label)
      if (!nzchar(label)) {
        showNotification(tr("define_groups.notif.enter_name"), type = "warning", duration = 15)
        return()
      }

      current_selections <- selected_group_selections()
      if (!length(current_selections)) {
        showNotification(tr("define_groups.notif.select_at_least_one"), type = "warning", duration = 15)
        return()
      }

      source_defs <- plot_parse_group_definitions(current_selections)
      if (!length(source_defs)) {
        showNotification(tr("define_groups.notif.create_failed"), type = "error", duration = 15)
        return()
      }

      combined_group <- plot_create_combined_group(source_defs, label)
      current_combined <- get_combined_groups()
      set_combined_groups(c(current_combined, list(combined_group)))

      updateTextInput(session, "combined_group_label", value = "")
      showNotification(tr("define_groups.notif.created", label), type = "message", duration = 15)
    })

    created_observers <- reactiveVal(character(0))

    output$combined_groups_list <- renderUI({
      combined <- get_combined_groups()
      if (!length(combined)) {
        return(NULL)
      }

      ns <- session$ns
      tagList(
        h4(tr("define_groups.title")),
        lapply(seq_along(combined), function(i) {
          group <- combined[[i]]
          label <- group$label
          safe_id <- gsub("[^a-zA-Z0-9_]", "_", label)

          tags$div(
            style = "background-color: #f5f5f5; padding: 8px; margin: 5px 0; border-radius: 4px; display: flex; justify-content: space-between; align-items: center;",
            tags$span(label),
            actionButton(
              ns(paste0("remove_combined_group_", safe_id)),
              tr("define_groups.remove"),
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
                showNotification(tr("define_groups.notif.removed", removed_label), type = "message", duration = 15)
              }
            }, ignoreInit = TRUE)
          })

          created_observers(c(created_observers(), button_id))
        }
      }
    })
  })
}
