mod_about_ui <- function(id, lang = i18n_default_language) {
  tr_md <- function(key, ...) i18n_t_markdown(lang, key, ...)
  tr <- function(key, ...) i18n_t(lang, key, ...)

  tabPanel(
    tr("app.tab.about"),
    value = "about",
    tags$div(
      style = "margin: 16px 0 20px 0;",
      tr_md("about.description")
    )
  )
}