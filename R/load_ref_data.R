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

  population <- readxl::read_excel(
    ref_path,
    col_names = c(
      "group", "mean", "sd", "median", "2_3_range", "q_1_6", "q_5_6"
    ),
    skip = 1
  )
  return(population)
}
