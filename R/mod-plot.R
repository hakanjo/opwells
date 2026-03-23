mod_plot_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    tabsetPanel(
      tabPanel(
        "Reference Plot",
        br(),
        fluidRow(
          column(
            width = 4,
            selectInput(ns("group1"), "Top group", choices = NULL),
            selectInput(ns("group2"), "Bottom group", choices = NULL)
          )
        ),
        plotOutput(ns("ref_plot"), height = "500px")
      ),
      tabPanel(
        "User Data",
        br(),
        fluidRow(
          column(
            width = 4,
            selectInput(ns("group_col"), "Column defining groups", choices = NULL),
            selectInput(ns("selected_group1"), "Top group", choices = NULL),
            selectInput(ns("selected_group2"), "Bottom group", choices = NULL)
          )
        ),
        plotOutput(ns("user_plot"), height = "500px")
      )
    )
  )
}

mod_plot_server <- function(id, data = NULL, scores = NULL) {
  moduleServer(id, function(input, output, session) {

    # Reference data plot
    ref_data <- reactive({
      load_ref_data()
    })

    observe({
      ref_df <- ref_data()
      groups <- sort(unique(ref_df$group))
      
      updateSelectInput(session, "group1", choices = groups, selected = groups[1])
      updateSelectInput(session, "group2", choices = groups, selected = if (length(groups) > 1) groups[2] else groups[1])
    })

    output$ref_plot <- renderPlot({
      ref_df <- ref_data()
      req(input$group1, input$group2)
      
      # Filter to selected groups
      plot_data <- ref_df[ref_df$group %in% c(input$group1, input$group2), ]
      req(nrow(plot_data) == 2)

      # Ensure group1 is on top and group2 is on bottom
      plot_data$group_position <- factor(
        plot_data$group,
        levels = c(input$group2, input$group1),
        labels = c("2", "1")
      )
      
      # Create color and shape mapping
      plot_data$point_color <- ifelse(plot_data$group_position == "1", "#5EAC34", "#4481BA")
      plot_data$point_shape <- ifelse(plot_data$group_position == "1", 21, 23)
      
      ggplot(plot_data, aes(x = mean, y = group_position)) +
      geom_segment(
        aes(
          x = mean - sd,
          xend = mean + sd,
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
        aes(label = paste0(group, "\n", round(mean, 0), " ± ", round(sd, 0))),
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
        caption = "Medelvärde ±1 standardavvikelse."
      ) +
      annotate(
        "text", label = "Lägst välbefinnande",
        x = 0, y = -Inf, vjust = -1.5, hjust = "left", size = 5,
        color = "grey70"
      ) +
      annotate("text", label = "Högst välbefinnande",
        x = 100, y = -Inf, , vjust = -1.5, hjust = "right", size = 5,
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
    })

    # User data plot
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
        
        if (length(groups) >= 2) {
          updateSelectInput(session, "selected_group1", choices = groups, selected = groups[1])
          updateSelectInput(session, "selected_group2", choices = groups, selected = groups[2])
        }
      }
    })

    output$user_plot <- renderPlot({
      if (is.null(data)) return(NULL)
      user_df <- data()
      req(
        is.data.frame(user_df),
        nrow(user_df) > 0,
        !is.null(input$group_col),
        !is.null(input$selected_group1),
        !is.null(input$selected_group2),
        nzchar(input$group_col),
        nzchar(input$selected_group1),
        nzchar(input$selected_group2)
      )
      validate(need(
        !is.null(scores) && !is.null(scores()),
        "No scaled scores computed. Please compute them in the Likert Score tab."
      ))

      # Filter to selected groups, keeping matching score values by row index
      row_idx <- as.character(user_df[[input$group_col]]) %in% c(input$selected_group1, input$selected_group2)
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
          return(data.frame(group = g, mean = NA_real_, sd = NA_real_, n = 0L, stringsAsFactors = FALSE))
        }

        data.frame(
          group = g,
          mean = mean(x_clean),
          sd = stats::sd(x_clean),
          n = length(x_clean),
          stringsAsFactors = FALSE
        )
      })

      summary_data <- do.call(rbind, summary_list)
      names(summary_data)[names(summary_data) == "group"] <- group_col_name
      
      # Assign group positions
      summary_data$group_position <- factor(
        summary_data[[group_col_name]],
        levels = c(input$selected_group2, input$selected_group1),
        labels = c("2", "1")
      )
      req(nrow(summary_data) >= 2, "Could not find both groups in data.")
      
      # Create color and shape mapping
      summary_data$group_label <- as.character(summary_data[[group_col_name]])
      summary_data$point_color <- ifelse(summary_data$group_position == "1", "#5EAC34", "#4481BA")
      summary_data$point_shape <- ifelse(summary_data$group_position == "1", 21, 23)

      ggplot(summary_data, aes(x = mean, y = group_position)) +
      geom_segment(
        aes(
          x = mean - sd,
          xend = mean + sd,
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
        aes(
          label = paste0(
            group_label, "\n",
            round(mean, 0), ifelse(!is.na(sd), paste0(" ± ", round(sd, 0)), "")
          )
        ),
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
        caption = "Medelvärde ±1 standardavvikelse."
      ) +
      annotate(
        "text", label = "Lägst välbefinnande",
        x = 0, y = -Inf, vjust = -1.5, hjust = "left", size = 5,
        color = "grey70"
      ) +
      annotate("text", label = "Högst välbefinnande",
        x = 100, y = -Inf, , vjust = -1.5, hjust = "right", size = 5,
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
    })
  })
}
