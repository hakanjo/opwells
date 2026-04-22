mod_plot_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    tabsetPanel(
      id = ns("plot_tab"),
      tabPanel(
        "Referensdata",
        value = "ref",
        br(),
        fluidRow(
          column(
            width = 4,
            uiOutput(ns("ref_groups_ui")),
            actionButton(ns("add_ref_group"), "Lägg till grupp")
          )
        ),
        plotOutput(ns("ref_plot"), height = "500px", click = ns("ref_plot_click"))
      ),
      tabPanel(
        "Användardata",
        value = "user",
        br(),
        fluidRow(
          column(
            width = 4,
            selectInput(ns("group_col"), "Gruppdefinition", choices = NULL),
            uiOutput(ns("user_groups_ui")),
            actionButton(ns("add_user_group"), "Lägg till grupp")
          )
        ),
        plotOutput(ns("user_plot"), height = "500px", click = ns("user_plot_click"))
      )
    )
  )
}

mod_plot_server <- function(id, data = NULL, scores = NULL) {
  moduleServer(id, function(input, output, session) {

    current_scores <- reactive({
      if (is.null(scores)) {
        return(NULL)
      }

      value <- scores()
      if (is.list(value) && "scaled_scores" %in% names(value)) {
        return(value$scaled_scores)
      }

      value
    })

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

    build_click_targets <- function(summary_df, ordered_groups, raw_df = NULL, raw_value_col = NULL) {
      req(nrow(summary_df) > 0)

      ordered_groups <- as.character(ordered_groups)
      summary_df$group_label <- as.character(summary_df$group_label)
      summary_df <- summary_df[summary_df$group_label %in% ordered_groups, , drop = FALSE]
      req(nrow(summary_df) > 0)

      present_groups <- ordered_groups[ordered_groups %in% summary_df$group_label]
      req(length(present_groups) >= 1)

      summary_points <- data.frame(
        group_label = summary_df$group_label,
        x = summary_df$mean,
        y = as.numeric(factor(summary_df$group_label, levels = rev(present_groups))),
        stringsAsFactors = FALSE
      )

      raw_points <- data.frame(group_label = character(0), x = numeric(0), y = numeric(0), stringsAsFactors = FALSE)
      if (!is.null(raw_df) && nrow(raw_df) > 0 && !is.null(raw_value_col) && raw_value_col %in% names(raw_df)) {
        raw_df$group_label <- as.character(raw_df$group_label)
        raw_df <- raw_df[raw_df$group_label %in% present_groups, , drop = FALSE]
        if (nrow(raw_df) > 0) {
          x_values <- suppressWarnings(as.numeric(raw_df[[raw_value_col]]))
          keep <- !is.na(x_values)
          raw_points <- data.frame(
            group_label = raw_df$group_label[keep],
            x = x_values[keep],
            y = as.numeric(factor(raw_df$group_label[keep], levels = rev(present_groups))),
            stringsAsFactors = FALSE
          )
        }
      }

      list(summary = summary_points, raw = raw_points, present_groups = present_groups)
    }

    build_summary_plot <- function(plot_df, group_col_name, ordered_groups, exploded = FALSE, raw_df = NULL, raw_value_col = NULL) {
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
      plot_df$y_numeric <- as.numeric(plot_df$group_position)
      plot_df$point_color <- unname(color_map[plot_df$group_label])
      plot_df$point_shape <- unname(shape_map[plot_df$group_label])

      p <- ggplot(plot_df, aes(x = mean, y = group_position)) +
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

      if (exploded) {
        raw_plot <- data.frame()
        if (!is.null(raw_df) && nrow(raw_df) > 0 && !is.null(raw_value_col) && raw_value_col %in% names(raw_df)) {
          raw_plot <- raw_df
          raw_plot$group_label <- as.character(raw_plot$group_label)
          raw_plot <- raw_plot[raw_plot$group_label %in% present_groups, , drop = FALSE]
          raw_plot$score_value <- suppressWarnings(as.numeric(raw_plot[[raw_value_col]]))
          raw_plot <- raw_plot[!is.na(raw_plot$score_value), , drop = FALSE]
          if (nrow(raw_plot) > 0) {
            raw_plot$group_position <- factor(raw_plot$group_label, levels = rev(present_groups))
            raw_plot$point_color <- unname(color_map[raw_plot$group_label])
            p <- p +
              geom_point(
                data = raw_plot,
                aes(x = score_value, y = group_position, fill = point_color),
                position = position_nudge(
                  y = runif(nrow(raw_plot), -0.5, -0.05)
                ),
                size = 3,
                alpha = 0.5,
                shape = 21,
                stroke = 0.5
              ) +
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
                aes(label = paste0(group_label, "\n", round(mean, 0), " (", round(q_1_6, 0), "–", round(q_5_6, 0), ")")),
                vjust = -0.5,
                size = 5,
                color = "black",
                fontface = "bold"
              ) +
              scale_fill_identity() +
              scale_shape_identity()
          }
        }
      } else {
        p <- p +
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
            aes(label = paste0(group_label, "\n", round(mean, 0), " (", round(q_1_6, 0), "–", round(q_5_6, 0), ")")),
            vjust = -0.5,
            size = 5,
            color = "black",
            fontface = "bold"
          ) +
          scale_fill_identity() +
          scale_shape_identity()
      }

      p
    }

    # Reference data plot
    ref_data <- reactive({
      load_ref_data()
    })

    ref_selected_groups <- reactiveVal(character(0))
    ref_exploded <- reactiveVal(FALSE)
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

    observeEvent(ref_selected_groups(), {
      ref_exploded(FALSE)
    }, ignoreInit = TRUE)

    observeEvent(input$ref_plot_click, {
      click <- input$ref_plot_click
      if (is.null(click)) return()

      if (isTRUE(ref_exploded())) {
        ref_exploded(FALSE)
        return()
      }

      showNotification(
        "Referensdata innehåller endast sammanfattningsmått och saknar underliggande datapunkter.",
        type = "message",
        duration = 3
      )
    }, ignoreInit = TRUE)

    output$ref_plot <- renderPlot({
      ref_df <- ref_data()
      selected <- ref_selected_groups()
      req(length(selected) >= 1)

      plot_data <- ref_df[as.character(ref_df$group) %in% selected, , drop = FALSE]
      req(nrow(plot_data) > 0)
      plot_data$group_label <- as.character(plot_data$group)

      build_summary_plot(plot_data, "group", selected, exploded = isTRUE(ref_exploded()))
    })

    # User data plot
    user_selected_groups <- reactiveVal(character(0))
    user_exploded <- reactiveVal(FALSE)
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

    user_plot_payload <- reactive({
      if (is.null(data)) return(NULL)
      user_df <- data()
      selected <- user_selected_groups()

      if (!is.data.frame(user_df) || nrow(user_df) == 0 || ncol(user_df) == 0) {
        return(NULL)
      }

      if (is.null(current_scores())) {
        return(NULL)
      }

      req(
        !is.null(input$group_col),
        nzchar(input$group_col),
        input$group_col %in% names(user_df),
        length(selected) >= 1
      )

      row_idx <- as.character(user_df[[input$group_col]]) %in% selected
      plot_data <- user_df[row_idx, , drop = FALSE]
      req(nrow(plot_data) > 0)

      plot_scores <- suppressWarnings(as.numeric(current_scores()[row_idx]))
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
      summary_data$group_label <- as.character(summary_data[[group_col_name]])

      raw_data <- data.frame(
        group_label = grp_vals,
        score_value = plot_scores,
        stringsAsFactors = FALSE
      )
      raw_data <- raw_data[!is.na(raw_data$score_value), , drop = FALSE]

      list(
        summary = summary_data,
        raw = raw_data,
        selected = selected,
        group_col_name = group_col_name
      )
    })

    observeEvent(list(user_selected_groups(), input$group_col), {
      user_exploded(FALSE)
    }, ignoreInit = TRUE)

    observeEvent(input$user_plot_click, {
      click <- input$user_plot_click
      if (is.null(click)) return()

      payload <- user_plot_payload()
      if (is.null(payload)) return()

      if (isTRUE(user_exploded())) {
        user_exploded(FALSE)
        return()
      }

      if (nrow(payload$raw) > 0) {
        user_exploded(TRUE)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$plot_tab, {
      if (!identical(input$plot_tab, "user")) {
        return()
      }

      notification_id <- session$ns("user_plot_status")

      if (is.null(data)) {
        showNotification(
          "Ingen användardata laddad. Ladda data i fliken Ladda data för att visa ett diagram.",
          type = "warning",
          duration = 3,
          id = notification_id
        )
        return()
      }

      user_df <- data()
      if (!is.data.frame(user_df) || nrow(user_df) == 0 || ncol(user_df) == 0) {
        showNotification(
          "Ingen användardata laddad. Ladda data i fliken Ladda data för att visa ett diagram.",
          type = "warning",
          duration = 3,
          id = notification_id
        )
        return()
      }

      if (is.null(current_scores())) {
        showNotification(
          "Ingen skalad poäng beräknad. Beräkna dem i fliken Likert-poäng.",
          type = "warning",
          duration = 3,
          id = notification_id
        )
        return()
      }

      removeNotification(notification_id)
    }, ignoreInit = TRUE)

    output$user_plot <- renderPlot({
      payload <- user_plot_payload()
      if (is.null(payload)) return(NULL)

      build_summary_plot(
        payload$summary,
        payload$group_col_name,
        payload$selected,
        exploded = isTRUE(user_exploded()),
        raw_df = payload$raw,
        raw_value_col = "score_value"
      )
    })
  })
}
