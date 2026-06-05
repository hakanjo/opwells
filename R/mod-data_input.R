mod_data_input_ui <- function(id, lang = i18n_default_language, left_extra_ui = NULL, right_extra_ui = NULL) {
  ns <- NS(id)
  tr <- function(key, ...) i18n_t(lang, key, ...)
  tr_md <- function(key, ...) i18n_t_markdown(lang, key, ...)

  fluidRow(
    column(
      width = 3,
      tags$div(
        style = "margin-bottom: 12px;",
        tr_md("data_input.help.template_prefix"),
        downloadLink(ns("download_template"), tr_md("data_input.help.template_link")),
        helpText(tr_md("data_input.help")),
        hr()
      ),
      
      fileInput(
        ns("upload_file"),
        tr("data_input.upload.label"),
        accept = c(".xlsx", ".csv", ".tsv", ".txt")
      ),
      uiOutput(ns("delimiter_ui")),
      left_extra_ui
    ),
    column(
      width = 9,
      uiOutput(ns("upload_status")),
      right_extra_ui
    )
  )
}

mod_data_input_server <- function(id, lang = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    resolved_lang <- if (is.null(lang)) reactive(i18n_default_language) else lang
    tr <- function(key, ...) i18n_t(resolved_lang(), key, ...)

    default_column_names <- function(n_cols) {
      vapply(seq_len(n_cols), function(idx) {
        label <- character()
        value <- idx

        while (value > 0) {
          remainder <- (value - 1) %% 26
          label <- c(LETTERS[remainder + 1], label)
          value <- (value - remainder - 1) %/% 26
        }

        paste0(label, collapse = "")
      }, character(1))
    }

    normalize_names <- function(nms, fallback_names = NULL) {
      if (is.null(fallback_names)) {
        fallback_names <- default_column_names(length(nms))
      }

      nms <- trimws(nms)
      blank <- nms == "" | is.na(nms)
      if (any(blank)) {
        nms[blank] <- fallback_names[blank]
      }
      make.unique(nms)
    }

    coerce_to_character_df <- function(df) {
      df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
      if (!ncol(df)) {
        return(df)
      }

      df[] <- lapply(df, function(col) {
        col_chr <- as.character(col)
        col_chr[is.na(col_chr)] <- ""
        col_chr
      })

      df
    }

    detect_delimiter <- function(path, default = ",") {
      first_line <- readLines(path, n = 1, warn = FALSE)

      if (!length(first_line)) {
        return(default)
      }

      counts <- c(
        "," = lengths(regmatches(first_line, gregexpr(",", first_line, fixed = TRUE))),
        "\t" = lengths(regmatches(first_line, gregexpr("\t", first_line, fixed = TRUE))),
        ";" = lengths(regmatches(first_line, gregexpr(";", first_line, fixed = TRUE)))
      )

      names(which.max(counts))
    }

    parse_uploaded_data <- function(file_path, ext, delimiter) {
      if (identical(ext, "xlsx")) {
        raw_df <- as.data.frame(
          readxl::read_excel(file_path, col_names = FALSE, guess_max = 10000),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      } else {
        sep <- if (identical(delimiter, "auto")) detect_delimiter(file_path) else delimiter
        raw_df <- read.table(
          file = file_path,
          header = FALSE,
          sep = sep,
          quote = '"',
          fill = TRUE,
          stringsAsFactors = FALSE,
          check.names = FALSE,
          na.strings = c("NA", "NaN")
        )
      }

      raw_df <- coerce_to_character_df(raw_df)

      if (!ncol(raw_df)) {
        return(raw_df)
      }

      header_row <- if (nrow(raw_df) > 0) {
        as.character(raw_df[1, , drop = TRUE])
      } else {
        rep("", ncol(raw_df))
      }
      header_row[is.na(header_row)] <- ""

      df <- if (nrow(raw_df) > 1) raw_df[-1, , drop = FALSE] else raw_df[FALSE, , drop = FALSE]
      names(df) <- normalize_names(header_row, default_column_names(ncol(raw_df)))

      df
    }

    trim_empty_rows_cols <- function(df) {
      if (!ncol(df)) {
        return(df)
      }

      is_blank_col <- vapply(df, function(col) {
        col_chr <- trimws(as.character(col))
        all(is.na(col_chr) | col_chr == "")
      }, logical(1))

      if (any(is_blank_col)) {
        df <- df[, !is_blank_col, drop = FALSE]
      }

      if (!ncol(df)) {
        return(df)
      }

      is_blank_row <- apply(df, 1, function(row_vals) {
        row_chr <- trimws(as.character(row_vals))
        all(is.na(row_chr) | row_chr == "")
      })

      df <- df[!is_blank_row, , drop = FALSE]
      rownames(df) <- NULL
      df
    }

    auto_select_cols <- function(cols) {
      cols[
        grepl("^(?:OPWELLS|F|Q)_?[0-9]+$", cols, ignore.case = TRUE, perl = TRUE)
      ]
    }

    output$download_template <- downloadHandler(
      filename = function() {
        paste0("OPWELLS-", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        file.copy(file.path("data", "upload_template.xlsx"), file)
      }
    )

    current_data <- reactiveVal(data.frame())
    selected_cols <- reactiveVal(character(0))
    scores <- reactiveVal(NULL)
    delimiter_choice <- reactiveVal("auto")
    uploaded_file_info <- reactiveVal(NULL)

    output$delimiter_ui <- renderUI({
      file_info <- uploaded_file_info()
      if (is.null(file_info)) {
        return(NULL)
      }

      ext <- tolower(tools::file_ext(file_info$name))
      if (identical(ext, "xlsx")) {
        return(NULL)
      }

      selectInput(
        ns("delimiter"),
        tr("data_input.delimiter.label"),
        choices = stats::setNames(
          c("auto", ",", "\t", ";"),
          c(
            tr("data_input.delimiter.auto"),
            tr("data_input.delimiter.comma"),
            tr("data_input.delimiter.tab"),
            tr("data_input.delimiter.semicolon")
          )
        ),
        selected = delimiter_choice()
      )
    })

    load_file <- function(file_info, delimiter) {
      ext <- tolower(tools::file_ext(file_info$name))

      parsed <- tryCatch(
        parse_uploaded_data(file_info$datapath, ext, delimiter),
        error = function(e) NULL
      )

      if (is.null(parsed) || ncol(parsed) == 0) {
        showNotification(tr("data_input.validation.parse_fail"), type = "error", duration = 15)
        return()
      }

      df <- trim_empty_rows_cols(parsed)
      current_data(df)

      auto_cols <- auto_select_cols(names(df))
      selected_cols(auto_cols)

      if (length(auto_cols) > 0 && nrow(df) > 0) {
        scores(compute_scaled_score(df, auto_cols))
      } else {
        scores(NULL)
      }
    }

    observeEvent(input$upload_file, {
      if (is.null(input$upload_file)) {
        uploaded_file_info(NULL)
        current_data(data.frame())
        selected_cols(character(0))
        scores(NULL)
        return()
      }

      ext <- tolower(tools::file_ext(input$upload_file$name))
      if (!ext %in% c("xlsx", "csv", "tsv", "txt")) {
        showNotification(tr("data_input.validation.file_type"), type = "error", duration = 15)
        return()
      }

      delimiter_choice("auto")
      uploaded_file_info(input$upload_file)
      load_file(input$upload_file, "auto")
    })

    observeEvent(input$delimiter, {
      req(input$delimiter)
      delimiter_choice(input$delimiter)
      file_info <- uploaded_file_info()
      req(file_info)
      load_file(file_info, input$delimiter)
    }, ignoreInit = TRUE)

    output$upload_status <- renderUI({
      df <- current_data()

      if (!is.data.frame(df) || nrow(df) == 0) {
        return(NULL)
      }

      NULL
    })

    last_notification_state <- reactiveVal(NULL)
    no_cols_notification_shown <- reactiveVal(FALSE)

    observe({
      df <- current_data()
      sel <- selected_cols()
      scr <- scores()

      if (!is.data.frame(df) || nrow(df) == 0) {
        last_notification_state(NULL)
        no_cols_notification_shown(FALSE)
        return()
      }

      if (!length(sel)) {
        if (!isTRUE(no_cols_notification_shown())) {
          showNotification(
            tr("data_input.status.no_cols"),
            type = "warning",
            duration = 15
          )
          no_cols_notification_shown(TRUE)
        }
        last_notification_state(NULL)
        return()
      }

      no_cols_notification_shown(FALSE)

      current_state <- list(n_cols = length(sel), has_scores = !is.null(scr))
      last_state <- last_notification_state()

      if (!identical(current_state, last_state) && !is.null(scr)) {
        n <- length(sel)
        msg <- if (n == 10) {
          tr("data_input.status.score_exact", n, paste(sel, collapse = ", "))
        } else {
          tr("data_input.status.score_nonexact", n, paste(sel, collapse = ", "))
        }

        showNotification(msg, type = "message", duration = 15)
      }

      last_notification_state(current_state)
    })

    data <- reactive({
      current_data()
    })

    likert_state <- reactive({
      df <- current_data()
      sel <- selected_cols()

      list(
        scaled_scores = scores(),
        selected_columns = sel,
        numeric_items = if (is.data.frame(df) && nrow(df) > 0 && length(sel)) {
          get_likert_numeric_data(df, sel)
        } else {
          data.frame()
        }
      )
    })

    list(
      data = data,
      likert_state = likert_state
    )
  })
}