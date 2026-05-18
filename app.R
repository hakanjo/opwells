# Load packages
library(shiny)

# Load additional functions
file_list <- list.files(
  path = "R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE
)
for (file in file_list) { source(file) }

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/flags.css")),
  uiOutput("app_ui")
)

server <- function(input, output, session) {
  current_language <- reactive({
    i18n_normalize_language(input$app_language)
  })

  output$app_ui <- renderUI({
    lang <- current_language()
    tr <- function(key, ...) i18n_t(lang, key, ...)

    tagList(
      titlePanel(
        title = div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          tr("app.title"),
          div(
            id = "language-toggle-flags",
            style = "display: flex; gap: 8px;",
            lapply(i18n_supported_languages, function(l) {
              flag_src <- switch(l,
                "sv" = "sv_flag.svg", # Place these SVGs in www/
                "en" = "gb_flag.svg"
              )
              tags$img(
                src = flag_src, # Removed 'www/' prefix
                class = paste0("flag-icon", if (lang == l) " active" else " inactive"),
                style = "width: 32px; height: 22px; cursor: pointer;",
                onclick = sprintf("Shiny.setInputValue('app_language', '%s', {priority: 'event'})", l),
                title = i18n_locales[[l]][[paste0('lang.', ifelse(l == 'sv', 'swedish', 'english'))]]
              )
            })
          )
        ),
        windowTitle = tr("app.title")
      ),
      tabsetPanel(
        id = "main_tab",
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
        ),
        tabPanel(
          tr("app.tab.data_input"),
          value = "data_input",
          tagList(
            mod_data_input_ui("data_input", lang = lang),
            uiOutput("data_input_export_ui")
          )
        ),
        tabPanel(
          tr("app.tab.define_groups"),
          value = "define_groups",
          mod_define_groups_ui("define_groups", lang = lang)
        ),
        tabPanel(
          tr("app.tab.plot"),
          value = "plot",
          mod_plot_ui("plot", lang = lang)
        ),
        tabPanel(
          tr("app.tab.statistics"),
          value = "statistics",
          mod_statistics_ui("statistics", lang = lang)
        )
        
      )
    )
  })

  data_input_r <- mod_data_input_server("data_input", lang = current_language)
  data_r <- data_input_r$data
  likert_state_r <- data_input_r$likert_state

  output$data_input_export_ui <- renderUI({
    state <- likert_state_r()
    scaled <- if (!is.null(state)) state$scaled_scores else NULL

    if (is.null(scaled)) {
      return(NULL)
    }

    tagList(
      tags$hr(),
      mod_export_ui("export", lang = current_language())
    )
  })

  shared_group_state <- create_group_selection_state()

  mod_export_server(
    "export",
    data = data_r,
    likert_state = likert_state_r,
    lang = current_language
  )
  mod_statistics_server(
    "statistics",
    data = data_r,
    likert_state = likert_state_r,
    active_tab = reactive(input$main_tab),
    group_state = shared_group_state,
    lang = current_language
  )
  mod_define_groups_server(
    "define_groups",
    data = data_r,
    likert_state = likert_state_r,
    group_state = shared_group_state,
    lang = current_language
  )
  mod_plot_server(
    "plot",
    data = data_r,
    scores = likert_state_r,
    item_cols = likert_state_r,
    group_state = shared_group_state,
    active_tab = reactive(input$main_tab),
    lang = current_language
  )
}

shinyApp(ui = ui, server = server)
