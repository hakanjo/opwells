mod_about_ui <- function(id, lang = i18n_default_language) {
  tr <- function(key, ...) i18n_t(lang, key, ...)

  tabPanel(
    "Om OPWELLS",
    tags$div(
      style = "margin: 16px 0 20px 0;",
      tags$ul(
        tags$li(tr("app.intro.li1")),
        tags$li(tr("app.intro.li2")),
        tags$li(tr("app.intro.li3")),
        tags$li(tr("app.intro.li4")),
        tags$li(
          tr("app.intro.project_prefix"),
          tags$a(tr("app.intro.qa_link"), href = "#"),
          tr("app.intro.or_contact"),
          tags$a("jeanette.melin@lnu.se", href = "mailto:jeanette.melin@lnu.se"),
          "."
        )
      ),
      tags$p(
        tags$em(tr("app.intro.ref_note"))
      )
    )
  )
}