test_that("app.R builds a shiny app object", {
  app_file <- file.path(repo_root, "app.R")
  app_local <- new.env(parent = globalenv())

  app_obj <- source(app_file, local = app_local, chdir = TRUE)$value

  expect_s3_class(app_obj, "shiny.appobj")
  expect_true(exists("ui", envir = app_local, inherits = FALSE))
  expect_true(exists("server", envir = app_local, inherits = FALSE))
})
