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

  raw_population <- readxl::read_excel(
    ref_path,
    col_names = FALSE,
    skip = 1
  )

  if (ncol(raw_population) < 8) {
    stop("Reference file has an unexpected format.")
  }

  population <- data.frame(
    group = raw_population[[1]],
    mean = raw_population[[2]],
    sd = raw_population[[3]],
    `2_3_range` = raw_population[[5]],
    q_1_6 = raw_population[[6]],
    q_5_6 = raw_population[[7]],
    n = raw_population[[8]],
    stringsAsFactors = FALSE
  )

  return(population)
}
