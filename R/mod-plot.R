mod_plot_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    tabsetPanel(
      tabPanel(
        "Referensdata",
        br(),
        fluidRow(
          column(
            width = 4,
            uiOutput(ns("ref_groups_ui")),
            actionButton(ns("add_ref_group"), "Lägg till grupp")
          )
        ),
        plotOutput(ns("ref_plot"), height = "500px")
      ),
      tabPanel(
        "Användardata",
        br(),
        fluidRow(
          column(
            width = 4,
            selectInput(ns("group_col"), "Gruppdefinition", choices = NULL),
            uiOutput(ns("user_groups_ui")),
            actionButton(ns("add_user_group"), "Lägg till grupp")
          )
        ),
        plotOutput(ns("user_plot"), height = "500px")
      )
    )
  )
}

mod_plot_server <- function(id, data = NULL, scores = NULL) {
  moduleServer(id, function(input, output, session) {

    normalize_group_selection <- function(selected, groups, target_n = 2L) {
      groups <- as.character(groups)
      groups <- groups[!is.na(groups) & nzchar(groups)]
      if (!length(groups)) {
        return(character(0))
      }

      selected <- as.character(selected)
      selected <- selected[!is.na(selected) & nzchar(selected)]
      selected <- selected[selected %in% groups]
      selected <- unique(selected)

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
      if (!length(groups)) return(character(0))

      current <- selected_groups[idx]
      others <- selected_groups[-idx]
      available <- c(current, setdiff(groups, others))
      available <- unique(available)

      c(intersect(groups, available), setdiff(available, groups))
    }

    build_summary_plot <- function(plot_df, group_col_name, ordered_groups) {
      req(nrow(plot_df) > 0)

      ordered_groups <- as.character(ordered_groups)
      plot_df$group_label <- as.character(plot_df[[group_col_name]])
      plot_df <- plot_df[plot_df$group_label %in% ordered_groups, , drop = FALSE]
      req(nrow(plot_df) > 0)

      present_groups <- ordered_groups[ordered_groups %in% plot_df$group_label]
      req(length(present_groups) >= 1)

      n_groups <- length(present_groups)
      colors <- grDevices::hcl.colors(max(n_groups, 1), palette = "Set 2")
      shapes <- rep(c(21, 22, 23, 24, 25, 3, 4, 8, 15, 16), length.out = n_groups)
      color_map <- setNames(colors, present_groups)
      shape_map <- setNames(shapes, present_groups)

      plot_df$group_position <- factor(
        plot_df$group_label,
        levels = rev(present_groups)
      )
      plot_df$point_color <- unname(color_map[plot_df$group_label])
      plot_df$point_shape <- unname(shape_map[plot_df$group_label])

      ggplot(plot_df, aes(x = mean, y = group_position)) +
      geom_segment(
        aes(
          x = q_1_6,
          xend = q_5_6,
          y = group_position,
          yend = group_position
        ),
        color = "black",
        linewidth = 1,
        arrow = arrow(
          ends = "both",
          type = "closed",
          length = unit(0.25, "cm")
        )
      ) +
      geom_segment(
        aes(
          x = mean,
          xend = mean,
          y = group_position,
          yend = 0
        ),
        color = "grey70",
        linewidth = 0.5
      ) +
      geom_point(
        aes(fill = point_color, shape = point_shape),
        size = 5,
        stroke = 1.5
      ) +
      geom_text(
        aes(label = paste0(group_label, "\n", round(mean, 0), ifelse(!is.na(sd), paste0(" ± ", round(sd, 0)), ""))),
        vjust = -0.5,
        size = 5,
        color = "black",
        fontface = "bold"
      ) +
      scale_fill_identity() +
      scale_shape_identity() +
      scale_x_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, by = 10)
      ) +
      labs(
        x = NULL,
        y = NULL,
        caption = "Medelvärde med det centrala ⅔-intervallet."
      ) +
      annotate(
        "text", label = "Lägst välbefinnande",
        x = 0, y = -Inf, vjust = -1.5, hjust = "left", size = 5,
        color = "grey70"
      ) +
      annotate(
        "text", label = "Högst välbefinnande",
        x = 100, y = -Inf, vjust = -1.5, hjust = "right", size = 5,
        color = "grey70"
      ) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.line.x = element_line(linewidth = 1, color = "black"),
        plot.caption = element_text(size = 12, color = "grey40"),
        axis.text = element_text(size = 12),
        axis.text.y = element_blank()
      )
    }

    # Reference data plot
    ref_data <- reactive({
      load_ref_data()
    })

    ref_selected_groups <- reactiveVal(character(0))
    ref_remove_observers <- list()
    ref_select_observers <- list()

    observe({
      ref_df <- ref_data()
      groups <- sort(unique(as.character(ref_df$group)))
      groups <- groups[!is.na(groups) & nzchar(groups)]

      current <- ref_selected_groups()
      has_valid_current <- length(intersect(current, groups)) > 0
      target_n <- if (has_valid_current) 1L else 2L
      ref_selected_groups(normalize_group_selection(current, groups, target_n = target_n))
    })

    output$ref_groups_ui <- renderUI({
      ref_df <- ref_data()
      groups <- sort(unique(as.character(ref_df$group)))
      groups <- groups[!is.na(groups) & nzchar(groups)]
      selected <- ref_selected_groups()
      if (!length(selected)) return(NULL)

      tagList(lapply(seq_along(selected), function(i) {
        fluidRow(
          column(
            width = 9,
            selectInput(
              session$ns(paste0("ref_group_", i)),
              label = paste("Grupp", i),
              choices = group_choice_for_index(groups, selected, i),
              selected = selected[i]
            )
          ),
          column(
            width = 3,
            br(),
            actionButton(
              session$ns(paste0("remove_ref_group_", i)),
              label = "Ta bort",
              class = "btn btn-default btn-sm"
            )
          )
        )
      }))
    })

    observe({
      lapply(ref_remove_observers, function(obs) obs$destroy())
      lapply(ref_select_observers, function(obs) obs$destroy())
      ref_remove_observers <<- list()
      ref_select_observers <<- list()

      selected <- ref_selected_groups()
      if (!length(selected)) return()

      ref_remove_observers <<- lapply(seq_along(selected), function(i) {
        observeEvent(input[[paste0("remove_ref_group_", i)]], {
          current <- ref_selected_groups()
          if (length(current) <= 1) {
            showNotification("Minst en grupp måste vara vald.", type = "message", duration = 3)
            return()
          }
          current <- current[-i]
          ref_selected_groups(current)
        }, ignoreInit = TRUE)
      })

      ref_select_observers <<- lapply(seq_along(selected), function(i) {
        observeEvent(input[[paste0("ref_group_", i)]], {
          value <- input[[paste0("ref_group_", i)]]
          current <- ref_selected_groups()
          if (i > length(current) || is.null(value) || !nzchar(value)) return()

          if (value %in% current[-i]) {
            return()
          }
          current[i] <- value
          ref_selected_groups(current)
        }, ignoreInit = TRUE)
      })
    })

    observeEvent(input$add_ref_group, {
      ref_df <- ref_data()
      groups <- sort(unique(as.character(ref_df$group)))
      groups <- groups[!is.na(groups) & nzchar(groups)]

      current <- ref_selected_groups()
      if (!length(groups)) {
        ref_selected_groups(character(0))
        return()
      }

      next_group <- setdiff(groups, current)
      if (!length(next_group)) {
        showNotification("Alla tillgängliga grupper är redan valda.", type = "warning", duration = 3)
        return()
      }
      ref_selected_groups(c(current, next_group[1]))
    }, ignoreInit = TRUE)

    output$ref_plot <- renderPlot({
      ref_df <- ref_data()
      selected <- ref_selected_groups()
      req(length(selected) >= 1)

      plot_data <- ref_df[as.character(ref_df$group) %in% selected, , drop = FALSE]
      req(nrow(plot_data) > 0)

      build_summary_plot(plot_data, "group", selected)
    })

    # User data plot
    user_selected_groups <- reactiveVal(character(0))
    user_remove_observers <- list()
    user_select_observers <- list()

    observe({
      if (is.null(data)) return()
      user_df <- data()
      req(is.data.frame(user_df), nrow(user_df) > 0)

      # Find likely grouping columns while tolerating mixed/complex column types.
      candidate_names <- names(user_df)
      if (!length(candidate_names)) return()

      is_group_like <- vapply(candidate_names, function(col_nm) {
        x <- user_df[[col_nm]]
        out <- tryCatch({
          x_chr <- trimws(as.character(x))
          x_chr <- x_chr[!is.na(x_chr) & nzchar(x_chr)]
          if (!length(x_chr)) return(FALSE)

          x_num <- suppressWarnings(as.numeric(x_chr))
          all_numeric <- !any(is.na(x_num))
          if (!all_numeric) {
            return(TRUE)
          }

          length(unique(x_num)) <= 10
        }, error = function(e) FALSE)

        isTRUE(out)
      }, logical(1))

      potential_group_cols <- candidate_names[is_group_like]
      
      if (length(potential_group_cols) > 0) {
        updateSelectInput(session, "group_col", choices = potential_group_cols, selected = potential_group_cols[1])
      }
    })

    observe({
      if (is.null(data) || is.null(input$group_col)) return()
      user_df <- data()
      req(is.data.frame(user_df), nrow(user_df) > 0, nzchar(input$group_col))
      
      if (input$group_col %in% names(user_df)) {
        groups <- sort(unique(as.character(user_df[[input$group_col]])))
        groups <- groups[groups != "" & !is.na(groups)]

        current <- user_selected_groups()
        has_valid_current <- length(intersect(current, groups)) > 0
        target_n <- if (has_valid_current) 1L else 2L
        user_selected_groups(normalize_group_selection(current, groups, target_n = target_n))
      }
    })

    output$user_groups_ui <- renderUI({
      if (is.null(data) || is.null(input$group_col)) return(NULL)
      user_df <- data()
      req(is.data.frame(user_df), nrow(user_df) > 0, nzchar(input$group_col))
      req(input$group_col %in% names(user_df))

      groups <- sort(unique(as.character(user_df[[input$group_col]])))
      groups <- groups[groups != "" & !is.na(groups)]
      selected <- user_selected_groups()
      if (!length(selected)) return(NULL)

      tagList(lapply(seq_along(selected), function(i) {
        fluidRow(
          column(
            width = 9,
            selectInput(
              session$ns(paste0("user_group_", i)),
              label = paste("Grupp", i),
              choices = group_choice_for_index(groups, selected, i),
              selected = selected[i]
            )
          ),
          column(
            width = 3,
            br(),
            actionButton(
              session$ns(paste0("remove_user_group_", i)),
              label = "Ta bort",
              class = "btn btn-default btn-sm"
            )
          )
        )
      }))
    })

    observe({
      lapply(user_remove_observers, function(obs) obs$destroy())
      lapply(user_select_observers, function(obs) obs$destroy())
      user_remove_observers <<- list()
      user_select_observers <<- list()

      selected <- user_selected_groups()
      if (!length(selected)) return()

      user_remove_observers <<- lapply(seq_along(selected), function(i) {
        observeEvent(input[[paste0("remove_user_group_", i)]], {
          current <- user_selected_groups()
          if (length(current) <= 1) {
            showNotification("Minst en grupp måste vara vald.", type = "message", duration = 3)
            return()
          }
          current <- current[-i]
          user_selected_groups(current)
        }, ignoreInit = TRUE)
      })

      user_select_observers <<- lapply(seq_along(selected), function(i) {
        observeEvent(input[[paste0("user_group_", i)]], {
          value <- input[[paste0("user_group_", i)]]
          current <- user_selected_groups()
          if (i > length(current) || is.null(value) || !nzchar(value)) return()

          if (value %in% current[-i]) {
            return()
          }
          current[i] <- value
          user_selected_groups(current)
        }, ignoreInit = TRUE)
      })
    })

    observeEvent(input$add_user_group, {
      if (is.null(data) || is.null(input$group_col)) return()
      user_df <- data()
      req(is.data.frame(user_df), nrow(user_df) > 0, nzchar(input$group_col))
      req(input$group_col %in% names(user_df))

      groups <- sort(unique(as.character(user_df[[input$group_col]])))
      groups <- groups[groups != "" & !is.na(groups)]

      current <- user_selected_groups()
      if (!length(groups)) {
        user_selected_groups(character(0))
        return()
      }

      next_group <- setdiff(groups, current)
      if (!length(next_group)) {
        showNotification("Alla tillgängliga grupper är redan valda.", type = "warning", duration = 3)
        return()
      }
      user_selected_groups(c(current, next_group[1]))
    }, ignoreInit = TRUE)

    output$user_plot <- renderPlot({
      if (is.null(data)) return(NULL)
      user_df <- data()
      selected <- user_selected_groups()

      req(
        is.data.frame(user_df),
        nrow(user_df) > 0,
        !is.null(input$group_col),
        nzchar(input$group_col),
        length(selected) >= 1
      )
      validate(need(
        !is.null(scores) && !is.null(scores()),
        "Ingen skalad poäng beräknad. Beräkna dem i fliken Likert-poäng."
      ))

      # Filter to selected groups, keeping matching score values by row index
      row_idx <- as.character(user_df[[input$group_col]]) %in% selected
      plot_data <- user_df[row_idx, ]
      req(nrow(plot_data) > 0)

      plot_scores <- suppressWarnings(as.numeric(scores()[row_idx]))

      group_col_name <- input$group_col
      grp_vals <- as.character(plot_data[[group_col_name]])
      split_scores <- split(plot_scores, grp_vals)

      summary_list <- lapply(names(split_scores), function(g) {
        x <- split_scores[[g]]
        x_clean <- x[!is.na(x)]
        if (!length(x_clean)) {
          return(data.frame(group = g, mean = NA_real_, sd = NA_real_, q_1_6 = NA_real_, q_5_6 = NA_real_, n = 0L, stringsAsFactors = FALSE))
        }

        data.frame(
          group = g,
          mean = mean(x_clean),
          sd = stats::sd(x_clean),
          q_1_6 = quantile(x_clean, probs = 1/6, na.rm = TRUE),
          q_5_6 = quantile(x_clean, probs = 5/6, na.rm = TRUE),
          n = length(x_clean),
          stringsAsFactors = FALSE
        )
      })

      summary_data <- do.call(rbind, summary_list)
      names(summary_data)[names(summary_data) == "group"] <- group_col_name

      build_summary_plot(summary_data, group_col_name, selected)
    })
  })
}
