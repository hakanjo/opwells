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
  titlePanel("Äldres välbefinnande"),
  tabsetPanel(
    id = "main_tab",
    tabPanel(
      "Ladda data",
      value = "data_input",
      mod_data_input_ui("data_input")
    ),
    tabPanel(
      "Poäng",
      value = "likert",
      mod_likert_ui("likert")
    ),
    tabPanel(
      "Plotta data",
      value = "plot",
      mod_plot_ui("plot")
    ),
    tabPanel(
      "Statistik",
      value = "statistics",
      mod_statistics_ui("statistics")
    )
  )
)

server <- function(input, output, session) {
  data_r <- mod_data_input_server("data_input")
  likert_state_r <- mod_likert_server("likert", data = data_r, active_tab = reactive(input$main_tab))
  mod_statistics_server("statistics", data = data_r, likert_state = likert_state_r, active_tab = reactive(input$main_tab))
  mod_plot_server("plot", data = data_r, scores = likert_state_r)
}

shinyApp(ui = ui, server = server)
