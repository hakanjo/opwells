mod_statistics_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    h5("Statistiska jämförelser för användardata"),
    helpText("För varje kategori visas medel, median, standardavvikelse och tvåtredjedelsintervall (16,7:e till 83,3:e percentilen). Parvisa gruppjämförelser visas under tabellen."),
    fluidRow(
      column(
        width = 4,
        selectInput(ns("group_col"), "Kategorikolumn", choices = c("Välj kolumn" = ""), selected = ""),
        uiOutput(ns("group_values_ui"))
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

mod_statistics_server <- function(id, data = NULL, likert_state = NULL, active_tab = NULL) {
  moduleServer(id, function(input, output, session) {

    trim_non_empty <- function(x) {
      x_chr <- trimws(as.character(x))
      x_chr[!is.na(x_chr) & nzchar(x_chr)]
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
        x <- trim_non_empty(user_df[[col_nm]])
        if (!length(x)) {
          return(FALSE)
        }

        x_num <- suppressWarnings(as.numeric(x))
        all_numeric <- !any(is.na(x_num))
        if (!all_numeric) {
          return(TRUE)
        }

        length(unique(x_num)) <= 10
      }, logical(1))

      candidate_names[is_group_like]
    }

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
      user_df <- if (is.null(data)) NULL else data()
      state <- current_likert_state()
      excluded <- if (is.null(state)) character(0) else state$selected_columns

      group_cols <- detect_group_columns(user_df, excluded_cols = excluded)
      selected <- input$group_col
      if (is.null(selected) || !selected %in% group_cols) {
        selected <- ""
      }

      updateSelectInput(
        session,
        "group_col",
        choices = c("Välj kolumn" = "", stats::setNames(group_cols, group_cols)),
        selected = selected
      )
    })

    output$group_values_ui <- renderUI({
      req(!is.null(data), nzchar(input$group_col))
      user_df <- data()
      req(is.data.frame(user_df), input$group_col %in% names(user_df))

      groups <- sort(unique(trim_non_empty(user_df[[input$group_col]])))
      req(length(groups) > 0)

      checkboxGroupInput(
        session$ns("selected_groups"),
        "Kategorier att inkludera",
        choices = groups,
        selected = groups
      )
    })

    comparison_data <- reactive({
      req(!is.null(data))
      user_df <- data()
      state <- current_likert_state()

      req(
        is.data.frame(user_df),
        nrow(user_df) > 0,
        !is.null(state),
        !is.null(state$scaled_scores),
        length(state$scaled_scores) == nrow(user_df),
        nzchar(input$group_col),
        input$group_col %in% names(user_df)
      )

      groups_all <- trim_non_empty(user_df[[input$group_col]])
      groups_available <- sort(unique(groups_all))

      selected_groups <- trim_non_empty(input$selected_groups)
      if (!length(selected_groups)) {
        selected_groups <- groups_available
      }
      selected_groups <- selected_groups[selected_groups %in% groups_available]

      group_vec <- trimws(as.character(user_df[[input$group_col]]))
      scores <- suppressWarnings(as.numeric(state$scaled_scores))

      valid_idx <- !is.na(scores) & !is.na(group_vec) & nzchar(group_vec) & group_vec %in% selected_groups
      df <- data.frame(
        group = group_vec[valid_idx],
        score = scores[valid_idx],
        stringsAsFactors = FALSE
      )

      req(nrow(df) > 0)
      df
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

      if (is.null(input$group_col) || !nzchar(input$group_col)) {
        return(div(class = "text-muted", "Välj en kategorikolumn för att visa statistik per kategori."))
      }

      stats_df <- summary_statistics()
      build_table(stats_df)
    })

    output$pairwise_statistics_ui <- renderUI({
      state <- current_likert_state()
      if (is.null(state) || is.null(state$scaled_scores)) {
        return(div(class = "text-muted", "Skalad poäng saknas. Beräkna poäng för att visa parvisa jämförelser."))
      }

      if (is.null(input$group_col) || !nzchar(input$group_col)) {
        return(div(class = "text-muted", "Välj en kategorikolumn för att visa parvisa jämförelser."))
      }

      pairwise_df <- pairwise_statistics()
      if (!is.data.frame(pairwise_df) || nrow(pairwise_df) == 0) {
        return(div(class = "text-muted", "Minst två grupper med data krävs för parvisa jämförelser."))
      }

      build_table(pairwise_df)
    })
  })
}