load_ref_data <- function() {
  population <- read_excel(
    "data/ref_pop-2026-03-20.xlsx",
    col_names = c(
      "group", "mean", "sd", "median", "2_3_range", "q_1_6", "q_5_6"
    ),
    skip = 1
  )
  return(population)
}
