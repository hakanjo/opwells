# Load packages
library(shiny)
library(bslib)
library(readxl)
library(rhandsontable)
library(ggplot2)

# Load additional functions
file_list <- list.files(
  path = "R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE
)
for (file in file_list) { source(file) }

ui <- fluidPage(
  titlePanel("Äldres välbefinnande – OPWELLS"),
  tags$div(
    style = "margin: 16px 0 20px 0;",
    tags$ul(
      tags$li("OPWELLS är en enkät baserad på 10 frågor för att mäta äldres välbefinnande. OPWELLS är en förkortning för enkätens engelska benämning, Older Persons Wellbeing Scale."),
      tags$li("Denna webbapplikation möjliggör att ta fram mått på välbefinnande från individers enkätsvar."),
      tags$li("Webbapplikationen möjliggör också illustrationer och statistiska jämförelser inom ditt dataset samt mot referenspopulationen*"),
      tags$li("Nedan kan du ladda upp dina enkätsvar tillsammans med ytterligare variabler som du vill kunna göra jämförelser för."),
      tags$li(
        "OPWELLS och webbapplikationen är framtagna inom ramen för projektet Mätning av äldres välbefinnande och har finansierats av Familjen Kamprads Stiftelse. Om du har frågor om OPWELLS kan du antingen läsa mer här ",
        tags$a("länk till Q&A", href = "#"),
        " eller kontakta ",
        tags$a("jeanette.melin@lnu.se", href = "mailto:jeanette.melin@lnu.se"),
        "."
      )
    ),
    tags$p(
      tags$em("*Referensgruppen består av de äldre som under utvecklingen av OPWELLS deltog. De kommer från nio svenska projektparts- och samverkanskommuner.")
    )
  ),
  tabsetPanel(
    id = "main_tab",
    tabPanel(
      "Ladda upp data",
      value = "data_input",
      mod_data_input_ui("data_input")
    ),
    tabPanel(
      "Illustrationer",
      value = "plot",
      mod_plot_ui("plot")
    ),
    tabPanel(
      "Statistiska jämförelser",
      value = "statistics",
      mod_statistics_ui("statistics")
    ),
    tabPanel(
      "Exporter",
      value = "export",
      mod_export_ui("export")
    )
  )
)

server <- function(input, output, session) {
  data_input_r <- mod_data_input_server("data_input")
  data_r <- data_input_r$data
  likert_state_r <- data_input_r$likert_state
  mod_export_server("export", data = data_r, likert_state = likert_state_r, active_tab = reactive(input$main_tab))
  mod_statistics_server("statistics", data = data_r, likert_state = likert_state_r, active_tab = reactive(input$main_tab))
  mod_plot_server("plot", data = data_r, scores = likert_state_r)
}

shinyApp(ui = ui, server = server)
