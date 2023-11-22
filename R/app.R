library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(waiter)
library(shinyFeedback)
library(tidyverse)
library(pROC)
library(ROCpAI)
library(dplyr)
library(tools)
library(readr)
library(htmltools)

# Manages app autoreload
config <- function(...) {
    options(shiny.host = "127.0.0.1")
    options(shiny.port = 8000)
    options(shiny.autoreload = TRUE)
}

my_app <- function(...) {
    ui <- main_ui
    server <- main_server
    shinyApp(
        ui = ui,
        server = server,
    )
}
