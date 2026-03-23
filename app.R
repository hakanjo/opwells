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
    tabPanel(
      "Load Data",
      mod_data_input_ui("data_input")
    ),
    tabPanel(
      "Plot Data",
      mod_plot_ui("plot")
    )
  )
)

server <- function(input, output, session) {
  data_r <- mod_data_input_server("data_input")
  mod_plot_server("plot", data = data_r)
}

shinyApp(ui = ui, server = server)
