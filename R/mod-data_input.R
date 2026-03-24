mod_data_input_ui <- function(id) {
  ns <- NS(id)

  tagList(
    helpText(
      "Klistra in data direkt i kalkylbladet nedan eller ladda upp en fil (.xlsx, .csv, .tsv, eller .txt). Om första raden innehåller kolumnnamn kan du redigera eller klistra in dem direkt i bladet."
    ),
    uiOutput(ns("upload_file_ui")),
    uiOutput(ns("delimiter_ui")),
    checkboxInput(
      ns("first_row_headers"),
      "Första raden innehåller kolumnnamn",
      value = TRUE
    ),
    actionButton(ns("clear_sheet"), "Rensa kalkylblad"),
    uiOutput(ns("sheet_ui"))
  )
}

mod_data_preview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    helpText(
      "Förhandsgranska de första raderna av din data här."
    ),
    tableOutput(ns("preview"))
  )
}

mod_data_input_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    make_empty_sheet <- function(n_rows = 20, n_cols = 18) {
      header_names <- default_column_names(n_cols)
      df <- as.data.frame(
        matrix("", nrow = n_rows, ncol = n_cols),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      names(df) <- header_names
      return(df)
    }

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

    normalize_colnames <- function(df) {
      names(df) <- normalize_names(names(df), default_column_names(ncol(df)))
      return(df)
    }

    coerce_sheet_df <- function(df) {
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

    sync_sheet_headers <- function(df, use_first_row_headers = TRUE) {
      df <- coerce_sheet_df(df)

      if (!ncol(df)) {
        return(df)
      }

      if (use_first_row_headers && nrow(df) > 0) {
        header_values <- normalize_names(
          as.character(df[1, , drop = TRUE]),
          default_column_names(ncol(df))
        )
        names(df) <- header_values
        return(df)
      }

      names(df) <- default_column_names(ncol(df))
      df
    }

    as_sheet_display <- function(df, min_rows = 20, min_cols = 18, header_row = NULL) {
      df <- normalize_colnames(df)
      df <- coerce_sheet_df(df)

      n_cols <- max(ncol(df), min_cols)
      header_row_values <- character(n_cols)
      if (!is.null(header_row)) {
        supplied <- as.character(header_row)
        supplied[is.na(supplied)] <- ""
        upto <- min(length(supplied), n_cols)
        if (upto > 0) {
          header_row_values[seq_len(upto)] <- supplied[seq_len(upto)]
        }
      } else if (ncol(df) > 0) {
        header_row_values[seq_len(ncol(df))] <- names(df)
      }
      header_values <- normalize_names(header_row_values, default_column_names(n_cols))

      n_data_rows <- max(nrow(df), min_rows - 1)
      display <- as.data.frame(
        matrix("", nrow = n_data_rows + 1, ncol = n_cols),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      names(display) <- header_values
      display[1, ] <- header_row_values

      if (ncol(df) > 0 && nrow(df) > 0) {
        display[seq_len(nrow(df)) + 1, seq_len(ncol(df))] <- df
      }

      display
    }

    detect_delimiter <- function(path, default = ",") {
      first_line <- readLines(path, n = 1, warn = FALSE)

      if (!length(first_line))
        return(default)

      # Count occurrences of common delimiters
      counts <- c(
        "," = lengths(
          regmatches(first_line, gregexpr(",", first_line, fixed = TRUE))
        ),
        "\t" = lengths(
          regmatches(first_line, gregexpr("\t", first_line, fixed = TRUE))
        ),
        ";" = lengths(
          regmatches(first_line, gregexpr(";", first_line, fixed = TRUE))
        )
      )

      # Return the delimiter with the highest count
      return(names(which.max(counts)))
    }

    detect_delimiter_from_lines <- function(lines, default = ",") {
      lines <- lines[!is.na(lines) & nzchar(trimws(lines))]

      if (!length(lines)) {
        return(default)
      }

      sample_line <- lines[[1]]
      counts <- c(
        "," = lengths(
          regmatches(sample_line, gregexpr(",", sample_line, fixed = TRUE))
        ),
        "\t" = lengths(
          regmatches(sample_line, gregexpr("\t", sample_line, fixed = TRUE))
        ),
        ";" = lengths(
          regmatches(sample_line, gregexpr(";", sample_line, fixed = TRUE))
        )
      )

      if (all(counts == 0)) {
        return(default)
      }

      names(which.max(counts))
    }

    resolve_sheet_delimiter <- function(choice, df, default = "\t") {
      if (!identical(choice, "auto")) {
        return(choice)
      }

      df <- coerce_sheet_df(df)
      if (!nrow(df) || !ncol(df)) {
        return(default)
      }

      row_vectors <- lapply(seq_len(nrow(df)), function(idx) {
        row_vals <- as.character(df[idx, , drop = TRUE])
        row_vals[is.na(row_vals)] <- ""
        row_vals
      })

      single_cell_lines <- unlist(lapply(row_vectors, function(row_vals) {
        non_empty <- which(trimws(row_vals) != "")
        if (length(non_empty) == 1) {
          return(row_vals[non_empty])
        }
        character(0)
      }), use.names = FALSE)

      detected <- detect_delimiter_from_lines(single_cell_lines, default = default)
      if (length(single_cell_lines)) {
        return(detected)
      }

      has_multi_col_rows <- any(vapply(row_vectors, function(row_vals) {
        sum(trimws(row_vals) != "") > 1
      }, logical(1)))

      if (isTRUE(has_multi_col_rows)) {
        return("\t")
      }

      default
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

      raw_df <- coerce_sheet_df(raw_df)

      if (!ncol(raw_df)) {
        return(list(data = raw_df, header_row = character(0)))
      }

      header_row <- if (nrow(raw_df) > 0) {
        as.character(raw_df[1, , drop = TRUE])
      } else {
        rep("", ncol(raw_df))
      }
      header_row[is.na(header_row)] <- ""

      df <- if (nrow(raw_df) > 1) raw_df[-1, , drop = FALSE] else raw_df[FALSE, , drop = FALSE]
      names(df) <- normalize_names(header_row, default_column_names(ncol(raw_df)))

      list(
        data = df,
        header_row = header_row
      )
    }

    sheet_to_data <- function(df, use_first_row_headers = TRUE) {
      df <- coerce_sheet_df(df)

      if (!ncol(df)) {
        return(df)
      }

      if (use_first_row_headers) {
        header_values <- if (nrow(df) > 0) {
          as.character(df[1, , drop = TRUE])
        } else {
          names(df)
        }
        body <- if (nrow(df) > 1) df[-1, , drop = FALSE] else df[FALSE, , drop = FALSE]
        names(body) <- normalize_names(header_values, default_column_names(ncol(df)))
        return(body)
      }

      normalize_colnames(df)
    }

    trim_empty_rows_cols <- function(df) {
      if (!ncol(df))
        return(df)

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

    sheet_data <- reactiveVal(make_empty_sheet())
    uploaded_file <- reactiveVal(NULL)
    uploaded_sheet_snapshot <- reactiveVal(NULL)
    sheet_was_edited <- reactiveVal(FALSE)
    delimiter_choice <- reactiveVal("auto")
    pasted_applied_delimiter <- reactiveVal("auto")
    upload_input_nonce <- reactiveVal(0)

    output$upload_file_ui <- renderUI({
      upload_input_nonce()
      fileInput(
        ns("upload_file"),
        "Ladda upp datafil:",
        accept = c(".xlsx", ".csv", ".tsv", ".txt")
      )
    })

    show_delimiter <- reactive({
      if (isTRUE(sheet_was_edited())) {
        return(TRUE)
      }

      file_info <- uploaded_file()
      if (is.null(file_info) || is.null(file_info$name)) {
        return(FALSE)
      }

      ext <- tolower(tools::file_ext(file_info$name))
      !identical(ext, "xlsx")
    })

    output$delimiter_ui <- renderUI({
      if (!isTRUE(show_delimiter())) {
        return(NULL)
      }

      selectInput(
        ns("delimiter"),
        "Avgränsare",
        choices = c("Automatisk" = "auto", "Komma (CSV)" = ",", "Tab (TSV)" = "\t", "Semikolon" = ";"),
        selected = delimiter_choice()
      )
    })

    refresh_uploaded_sheet <- function(update_snapshot = FALSE) {
      file_info <- uploaded_file()
      req(file_info)

      ext <- tolower(tools::file_ext(file_info$name))
      validate(
        need(ext %in% c("xlsx", "csv", "tsv", "txt"), "Please upload xlsx, csv, or tsv/txt files.")
      )

      delimiter <- delimiter_choice()

      parsed <- tryCatch(
        parse_uploaded_data(file_info$datapath, ext, delimiter),
        error = function(e) NULL
      )

      validate(
        need(!is.null(parsed) && !is.null(parsed$data), "Could not parse the uploaded file."),
        need(!is.null(parsed$data) && ncol(parsed$data) > 0, "Uploaded file must contain at least one column.")
      )

      refreshed_sheet <- as_sheet_display(parsed$data, header_row = parsed$header_row)
      sheet_data(refreshed_sheet)

      if (isTRUE(update_snapshot)) {
        uploaded_sheet_snapshot(refreshed_sheet)
      }
    }

    reprocess_pasted_sheet <- function(target_choice, source_choice = NULL) {
      current <- coerce_sheet_df(sheet_data())

      if (!nrow(current) || !ncol(current)) {
        return()
      }

      if (is.null(source_choice)) {
        source_choice <- pasted_applied_delimiter()
      }

      source_sep <- resolve_sheet_delimiter(source_choice, current)
      target_sep <- resolve_sheet_delimiter(target_choice, current)

      row_vectors <- lapply(seq_len(nrow(current)), function(idx) {
        row_vals <- as.character(current[idx, , drop = TRUE])
        row_vals[is.na(row_vals)] <- ""
        row_vals
      })

      single_cell_lines <- unlist(lapply(row_vectors, function(row_vals) {
        non_empty <- which(trimws(row_vals) != "")
        if (length(non_empty) == 1) {
          return(row_vals[non_empty])
        }
        character(0)
      }), use.names = FALSE)

      parsed_rows <- lapply(row_vectors, function(row_vals) {
        non_empty <- which(trimws(row_vals) != "")

        if (!length(non_empty)) {
          return(character(0))
        }

        upto <- max(non_empty)
        row_text <- paste(row_vals[seq_len(upto)], collapse = source_sep)
        pieces <- strsplit(row_text, target_sep, fixed = TRUE)[[1]]
        trimws(pieces)
      })

      parsed_rows <- parsed_rows[lengths(parsed_rows) > 0]
      if (!length(parsed_rows)) {
        return()
      }

      n_cols <- max(lengths(parsed_rows))
      raw_df <- as.data.frame(
        matrix("", nrow = length(parsed_rows), ncol = n_cols),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      for (idx in seq_along(parsed_rows)) {
        parts <- parsed_rows[[idx]]
        raw_df[idx, seq_along(parts)] <- parts
      }

      header_row <- as.character(raw_df[1, , drop = TRUE])
      header_row[is.na(header_row)] <- ""
      body <- if (nrow(raw_df) > 1) raw_df[-1, , drop = FALSE] else raw_df[FALSE, , drop = FALSE]
      names(body) <- normalize_names(header_row, default_column_names(ncol(raw_df)))

      sheet_data(as_sheet_display(body, header_row = header_row))
      pasted_applied_delimiter(target_sep)
      sheet_was_edited(TRUE)
    }

    output$sheet_ui <- renderUI({
      rhandsontable::rHandsontableOutput(ns("sheet"), width = "100%", height = 450)
    })

    if (requireNamespace("rhandsontable", quietly = TRUE)) {
      output$sheet <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(
          sheet_data(),
          rowHeaders = TRUE,
          colHeaders = names(sheet_data()),
          stretchH = "all",
          minRows = 20,
          minCols = 18,
          colWidths = 100
        ) |>
          rhandsontable::hot_table(
            contextMenu = TRUE,
            manualRowResize = TRUE,
            manualColumnResize = TRUE,
            allowInvalid = TRUE
          )
      })
    }

    observeEvent(input$sheet, {
      req(requireNamespace("rhandsontable", quietly = TRUE))
      updated <- rhandsontable::hot_to_r(input$sheet)
      validate(need(!is.null(updated), "Could not read spreadsheet data."))
      synced <- sync_sheet_headers(updated, isTRUE(input$first_row_headers))
      current <- sheet_data()

      # rHandsontable may emit input updates during normal rendering; only
      # treat it as an edit when the sheet content actually changes.
      if (identical(synced, current)) {
        return()
      }

      baseline <- uploaded_sheet_snapshot()
      changed_from_uploaded <-
        is.null(uploaded_file()) || is.null(baseline) || !identical(synced, baseline)

      if (isTRUE(changed_from_uploaded)) {
        sheet_was_edited(TRUE)
      }

      sheet_data(synced)
    }, ignoreNULL = TRUE)

    observeEvent(input$upload_file, {
      if (is.null(input$upload_file)) {
        uploaded_file(NULL)
        uploaded_sheet_snapshot(NULL)
        return()
      }

      delimiter_choice("auto")
      pasted_applied_delimiter("auto")
      uploaded_file(input$upload_file)
      sheet_was_edited(FALSE)
      refresh_uploaded_sheet(update_snapshot = TRUE)
    })

    observeEvent(input$delimiter, {
      if (is.null(input$delimiter)) {
        return()
      }

      previous_choice <- delimiter_choice()
      delimiter_choice(input$delimiter)

      # If the user has pasted/edited the sheet, delimiter changes should
      # reprocess the current sheet content rather than reloading the file.
      if (isTRUE(sheet_was_edited()) || is.null(uploaded_file())) {
        reprocess_pasted_sheet(delimiter_choice(), source_choice = previous_choice)
        return()
      }

      sheet_was_edited(FALSE)
      refresh_uploaded_sheet(update_snapshot = TRUE)
    }, ignoreInit = TRUE)

    observeEvent(input$clear_sheet, {
      sheet_data(make_empty_sheet())
      uploaded_file(NULL)
      uploaded_sheet_snapshot(NULL)
      sheet_was_edited(FALSE)
      delimiter_choice("auto")
      pasted_applied_delimiter("auto")
      upload_input_nonce(upload_input_nonce() + 1)
    })

    observeEvent(input$first_row_headers, {
      sheet_data(sync_sheet_headers(sheet_data(), isTRUE(input$first_row_headers)))
    }, ignoreInit = TRUE)

    data <- reactive({
      df <- sheet_to_data(sheet_data(), isTRUE(input$first_row_headers))
      req(df)
      trim_empty_rows_cols(df)
    })

    output$preview <- renderTable({
      validate(
        need(
          ncol(data()) > 0 && nrow(data()) > 0,
          "Ingen data att förhandsgranska ännu. Klistra in eller ladda upp data under fliken 'Ladda data'."
        )
      )
      head(data(), 20)
    })

    data
  })
}
