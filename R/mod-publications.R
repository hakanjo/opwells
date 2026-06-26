mod_publications_ui <- function(id, lang = i18n_default_language) {
  tr_md <- function(key, ...) i18n_t_markdown(lang, key, ...)
  tr <- function(key, ...) i18n_t(lang, key, ...)

  tabPanel(
    tr("app.tab.publications"),
    value = "publications",
    tags$div(
      style = "max-width: 60%; margin: 16px auto 20px auto;",
      tr_md("publications.description")
    )
  )
}
