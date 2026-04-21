mod_statistics_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    h5("Statistik för valda Likert-frågor"),
    helpText("Fliken använder samma valda frågor som i Poäng. Skalad poäng sammanfattas när den har beräknats."),
    tabsetPanel(
      id = ns("statistics_subtab"),
      tabPanel(
        "Totalt",
        br(),
        uiOutput(ns("total_statistics_ui"))
      ),
      tabPanel(
        "Grupperat",
        br(),
        fluidRow(
          column(
            width = 4,
            selectInput(ns("group_col"), "Gruppdefinition (valfritt)", choices = c("Ingen gruppering" = ""), selected = ""),
            uiOutput(ns("group_controls_ui")),
            actionButton(ns("add_group"), "Lägg till grupp")
          ),
          column(
            width = 8,
            uiOutput(ns("grouped_statistics_ui"))
          )
        )
      )
    )
  )
}

mod_statistics_server <- function(id, data = NULL, likert_state = NULL, active_tab = NULL) {
  moduleServer(id, function(input, output, session) {

    normalize_group_selection <- function(selected, groups, target_n = 2L) {
      groups <- as.character(groups)
      groups <- groups[!is.na(groups) & nzchar(groups)]
      if (!length(groups)) {
        return(character(0))
      }

      selected <- as.character(selected)
      selected <- selected[!is.na(selected) & nzchar(selected)]
      selected <- unique(selected[selected %in% groups])

      if (!length(selected)) {
        selected <- groups[1]
      }

      if (target_n > 0 && length(selected) < target_n) {
        missing <- setdiff(groups, selected)
        if (length(missing)) {
          selected <- c(selected, utils::head(missing, target_n - length(selected)))
        }
      }

      selected
    }

    group_choice_for_index <- function(groups, selected_groups, idx) {
      groups <- as.character(groups)
      if (!length(groups)) {
        return(character(0))
      }

      current <- selected_groups[idx]
      others <- selected_groups[-idx]
      available <- unique(c(current, setdiff(groups, others)))
      c(intersect(groups, available), setdiff(available, groups))
    }

    detect_group_columns <- function(user_df, excluded_cols = character(0)) {
      if (!is.data.frame(user_df) || nrow(user_df) == 0 || ncol(user_df) == 0) {
        return(character(0))
      }

      candidate_names <- setdiff(names(user_df), excluded_cols)
      if (!length(candidate_names)) {
        return(character(0))
      }

      is_group_like <- vapply(candidate_names, function(col_nm) {
        x <- user_df[[col_nm]]
        out <- tryCatch({
          x_chr <- trimws(as.character(x))
          x_chr <- x_chr[!is.na(x_chr) & nzchar(x_chr)]
          if (!length(x_chr)) {
            return(FALSE)
          }

          x_num <- suppressWarnings(as.numeric(x_chr))
          all_numeric <- !any(is.na(x_num))
          if (!all_numeric) {
            return(TRUE)
          }

          length(unique(x_num)) <= 10
        }, error = function(e) FALSE)

        isTRUE(out)
      }, logical(1))

      candidate_names[is_group_like]
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

    build_statistics_section <- function(title, stats_bundle, has_scaled_scores = FALSE) {
      display_bundle <- format_statistics_for_display(stats_bundle)

      scaled_content <- if (isTRUE(has_scaled_scores)) {
        build_table(display_bundle$scaled_summary)
      } else {
        div(class = "alert alert-info", "Ingen skalad poäng har beräknats ännu.")
      }

      tagList(
        h4(title),
        h5("Skalad poäng"),
        scaled_content,
        h5("Reliabilitet"),
        build_table(display_bundle$reliability),
        h5("Item-total-korrelationer"),
        build_table(display_bundle$item_total)
      )
    }

    selected_groups <- reactiveVal(character(0))
    remove_observers <- list()
    select_observers <- list()

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
      user_df <- if (is.null(data)) NULL else data()
      excluded <- character(0)
      state <- current_likert_state()
      if (!is.null(state) && length(state$selected_columns)) {
        excluded <- state$selected_columns
      }

      group_cols <- detect_group_columns(user_df, excluded_cols = excluded)
      choices <- c("Ingen gruppering" = "", stats::setNames(group_cols, group_cols))
      selected <- input$group_col
      if (is.null(selected) || !nzchar(selected) || !selected %in% group_cols) {
        selected <- ""
      }

      updateSelectInput(session, "group_col", choices = choices, selected = selected)
    })

    observe({
      if (is.null(data) || is.null(input$group_col) || !nzchar(input$group_col)) {
        selected_groups(character(0))
        return()
      }

      user_df <- data()
      req(is.data.frame(user_df), nrow(user_df) > 0, input$group_col %in% names(user_df))

      groups <- sort(unique(as.character(user_df[[input$group_col]])))
      groups <- groups[!is.na(groups) & nzchar(groups)]
      current <- selected_groups()
      has_valid_current <- length(intersect(current, groups)) > 0
      target_n <- if (has_valid_current) 1L else 2L
      selected_groups(normalize_group_selection(current, groups, target_n = target_n))
    })

    output$group_controls_ui <- renderUI({
      if (is.null(data) || is.null(input$group_col) || !nzchar(input$group_col)) {
        return(NULL)
      }

      user_df <- data()
      req(is.data.frame(user_df), nrow(user_df) > 0, input$group_col %in% names(user_df))

      groups <- sort(unique(as.character(user_df[[input$group_col]])))
      groups <- groups[!is.na(groups) & nzchar(groups)]
      selected <- selected_groups()
      if (!length(groups) || !length(selected)) {
        return(NULL)
      }

      tagList(lapply(seq_along(selected), function(i) {
        fluidRow(
          column(
            width = 9,
            selectInput(
              session$ns(paste0("group_", i)),
              label = paste("Grupp", i),
              choices = group_choice_for_index(groups, selected, i),
              selected = selected[i]
            )
          ),
          column(
            width = 3,
            br(),
            actionButton(
              session$ns(paste0("remove_group_", i)),
              label = "Ta bort",
              class = "btn btn-default btn-sm"
            )
          )
        )
      }))
    })

    observe({
      lapply(remove_observers, function(obs) obs$destroy())
      lapply(select_observers, function(obs) obs$destroy())
      remove_observers <<- list()
      select_observers <<- list()

      current <- selected_groups()
      if (!length(current)) {
        return()
      }

      remove_observers <<- lapply(seq_along(current), function(i) {
        observeEvent(input[[paste0("remove_group_", i)]], {
          values <- selected_groups()
          if (length(values) <= 1) {
            showNotification("Minst en grupp måste vara vald.", type = "message", duration = 3)
            return()
          }

          selected_groups(values[-i])
        }, ignoreInit = TRUE)
      })

      select_observers <<- lapply(seq_along(current), function(i) {
        observeEvent(input[[paste0("group_", i)]], {
          value <- input[[paste0("group_", i)]]
          values <- selected_groups()
          if (i > length(values) || is.null(value) || !nzchar(value) || value %in% values[-i]) {
            return()
          }

          values[i] <- value
          selected_groups(values)
        }, ignoreInit = TRUE)
      })
    })

    observeEvent(input$add_group, {
      if (is.null(data) || is.null(input$group_col) || !nzchar(input$group_col)) {
        return()
      }

      user_df <- data()
      req(is.data.frame(user_df), nrow(user_df) > 0, input$group_col %in% names(user_df))

      groups <- sort(unique(as.character(user_df[[input$group_col]])))
      groups <- groups[!is.na(groups) & nzchar(groups)]
      current <- selected_groups()

      next_group <- setdiff(groups, current)
      if (!length(next_group)) {
        showNotification("Alla tillgängliga grupper är redan valda.", type = "warning", duration = 3)
        return()
      }

      selected_groups(c(current, next_group[1]))
    }, ignoreInit = TRUE)

    total_statistics <- reactive({
      state <- current_likert_state()
      if (is.null(state) || !length(state$selected_columns)) {
        return(NULL)
      }

      build_statistics_bundle(
        scores = state$scaled_scores,
        items_df = state$numeric_items
      )
    })

    grouped_statistics <- reactive({
      if (is.null(data) || is.null(input$group_col) || !nzchar(input$group_col)) {
        return(list())
      }

      user_df <- data()
      state <- current_likert_state()
      selected <- selected_groups()

      req(
        is.data.frame(user_df),
        nrow(user_df) > 0,
        input$group_col %in% names(user_df),
        !is.null(state),
        length(state$selected_columns) > 0,
        length(selected) > 0
      )

      lapply(selected, function(group_name) {
        row_idx <- as.character(user_df[[input$group_col]]) %in% group_name
        scores <- state$scaled_scores
        scores_subset <- if (is.null(scores)) NULL else scores[row_idx]
        items_subset <- state$numeric_items[row_idx, , drop = FALSE]

        list(
          group = group_name,
          stats = build_statistics_bundle(scores = scores_subset, items_df = items_subset),
          has_scaled_scores = !is.null(scores) && any(!is.na(scores_subset))
        )
      })
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
          "Ingen användardata laddad. Ladda data i fliken Ladda data för att visa statistik.",
          type = "warning",
          duration = 3,
          id = notification_id
        )
        return()
      }

      if (is.null(state) || !length(state$selected_columns)) {
        showNotification(
          "Välj Likert-kolumner i fliken Poäng för att visa reliabilitet och item-total-korrelationer.",
          type = "warning",
          duration = 3,
          id = notification_id
        )
        return()
      }

      if (is.null(state$scaled_scores)) {
        showNotification(
          "Skalad poäng är ännu inte beräknad. Reliabilitetsmått visas ändå för de valda frågorna.",
          type = "message",
          duration = 3,
          id = notification_id
        )
        return()
      }

      showNotification(
        "Statistik visas för de valda frågorna och den beräknade skalade poängen.",
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

    output$total_statistics_ui <- renderUI({
      state <- current_likert_state()
      if (is.null(state) || !length(state$selected_columns)) {
        return(div(class = "text-muted", "Välj Likert-kolumner i Poäng för att visa total statistik."))
      }

      stats_bundle <- total_statistics()
      if (is.null(stats_bundle)) {
        return(NULL)
      }

      build_statistics_section(
        title = "Totalt",
        stats_bundle = stats_bundle,
        has_scaled_scores = !is.null(state$scaled_scores) && any(!is.na(state$scaled_scores))
      )
    })

    output$grouped_statistics_ui <- renderUI({
      grouped <- grouped_statistics()
      if (!length(grouped)) {
        return(div(class = "text-muted", "Välj en gruppdefinition för att visa statistik per grupp."))
      }

      tagList(
        h4("Per grupp"),
        tagList(lapply(grouped, function(entry) {
          build_statistics_section(
            title = entry$group,
            stats_bundle = entry$stats,
            has_scaled_scores = entry$has_scaled_scores
          )
        }))
      )
    })
  })
}