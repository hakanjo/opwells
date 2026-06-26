mod_about_ui <- function(id, lang = i18n_default_language) {
  tr_md <- function(key, ...) i18n_t_markdown(lang, key, ...)
  tr <- function(key, ...) i18n_t(lang, key, ...)

  tabPanel(
    tr("app.tab.about"),
    value = "about",
    tags$div(
      style = "max-width: 60%; margin: 16px auto 20px auto;",
      tags$style("
        #about-content li { margin-bottom: 0.6em; }
      "),
      tags$div(id = "about-content", tr_md("about.description"))
    )
  )
}