# Source app functions into a dedicated environment to avoid naming conflicts.
repo_root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)
r_dir <- file.path(repo_root, "R")

app_env <- new.env(parent = baseenv())
file_list <- list.files(
  path = r_dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE
)

for (file in file_list) {
  source(file, local = app_env)
}
