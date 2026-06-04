load_ref_data <- function() {
  filename <- "ref_pop-2026-03-20.xlsx"
  candidate_paths <- c(
    file.path("data", filename),
    file.path("..", "..", "data", filename)
  )
  ref_path <- candidate_paths[file.exists(candidate_paths)][1]

  if (is.na(ref_path) || !nzchar(ref_path)) {
    stop(sprintf("Reference file not found: %s", filename))
  }

  population_raw <- readxl::read_excel(
    ref_path,
    col_names = FALSE,
    skip = 1
  )

  if (ncol(population_raw) < 5) {
    stop(sprintf("Reference file has too few columns: %d", ncol(population_raw)))
  }

  parse_num <- function(x) {
    suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(x)), fixed = TRUE)))
  }

  parse_range <- function(x) {
    txt <- gsub("\u00A0", " ", as.character(x), fixed = TRUE)
    txt <- gsub(",", ".", txt, fixed = TRUE)
    parts <- strsplit(txt, "\\s*[-\\u2013]\\s*")

    low <- suppressWarnings(as.numeric(vapply(parts, function(p) p[[1]], character(1))))
    high <- suppressWarnings(as.numeric(vapply(parts, function(p) {
      if (length(p) >= 2) p[[2]] else NA_character_
    }, character(1))))

    list(low = low, high = high)
  }

  range_vals <- parse_range(population_raw[[5]])
  q_1_6 <- if (ncol(population_raw) >= 7) parse_num(population_raw[[6]]) else range_vals$low
  q_5_6 <- if (ncol(population_raw) >= 7) parse_num(population_raw[[7]]) else range_vals$high

  population <- data.frame(
    group = trimws(as.character(population_raw[[1]])),
    mean = parse_num(population_raw[[2]]),
    sd = parse_num(population_raw[[3]]),
    q_1_6 = q_1_6,
    q_5_6 = q_5_6,
    stringsAsFactors = FALSE
  )

  population <- population[!is.na(population$group) & nzchar(population$group), , drop = FALSE]
  return(population)
}
