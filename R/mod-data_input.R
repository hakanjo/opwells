mod_data_input_ui <- function(id) {
  ns <- NS(id)

  tagList(
    helpText(
      "Klistra in data direkt i kalkylbladet nedan eller ladda upp en fil (.xlsx, .csv, .tsv, eller .txt)."
    ),
    fileInput(
      ns("upload_file"),
      "Ladda upp datafil:",
      accept = c(".xlsx", ".csv", ".tsv", ".txt")
    ),
    selectInput(
      ns("delimiter"),
      "Avgränsare",
      choices = c("Automatisk" = "auto", "Komma (CSV)" = ",", "Tab (TSV)" = "\t", "Semikolon" = ";"),
      selected = "auto"
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

    make_empty_sheet <- function(n_rows = 20, n_cols = 12) {
      df <- as.data.frame(
        matrix("", nrow = n_rows, ncol = n_cols), stringsAsFactors = FALSE
      )
      names(df) <- paste0("col_", seq_len(n_cols))
      return(df)
    }

    normalize_colnames <- function(df) {
      nms <- names(df)
      nms <- trimws(nms)
      blank <- nms == "" | is.na(nms)
      if (any(blank))
        nms[blank] <- paste0("col_", which(blank))
      names(df) <- make.unique(nms)
      return(df)
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

    parse_uploaded_data <- function(file_path, ext, delimiter) {
      if (identical(ext, "xlsx")) {
        df <- as.data.frame(
          readxl::read_excel(file_path, guess_max = 10000), stringsAsFactors = FALSE
        )
      } else {
        sep <- if (identical(delimiter, "auto")) detect_delimiter(file_path) else delimiter
        df <- read.table(
          file = file_path,
          header = TRUE,
          sep = sep,
          quote = '"',
          fill = TRUE,
          stringsAsFactors = FALSE,
          check.names = FALSE,
          na.strings = c("", "NA", "NaN")
        )
      }

      return(normalize_colnames(df))
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

    output$sheet_ui <- renderUI({
      rhandsontable::rHandsontableOutput(ns("sheet"), width = "100%", height = 450)
    })

    if (requireNamespace("rhandsontable", quietly = TRUE)) {
      output$sheet <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(
          sheet_data(),
          rowHeaders = TRUE,
          stretchH = "all",
          minRows = 20,
          minCols = 12,
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
      sheet_data(as.data.frame(updated, stringsAsFactors = FALSE, check.names = FALSE))
    }, ignoreNULL = TRUE)

    observeEvent(input$upload_file, {
      req(input$upload_file)

      ext <- tolower(tools::file_ext(input$upload_file$name))
      validate(
        need(ext %in% c("xlsx", "csv", "tsv", "txt"), "Please upload xlsx, csv, or tsv/txt files.")
      )

      parsed <- tryCatch(
        parse_uploaded_data(input$upload_file$datapath, ext, input$delimiter),
        error = function(e) NULL
      )

      validate(
        need(!is.null(parsed), "Could not parse the uploaded file."),
        need(ncol(parsed) > 0, "Uploaded file must contain at least one column.")
      )

      sheet_data(parsed)
    })

    observeEvent(input$clear_sheet, {
      sheet_data(make_empty_sheet())
    })

    data <- reactive({
      df <- sheet_data()
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
