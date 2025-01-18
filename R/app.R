# INSTALLED
# library(formattable)
# library(shiny)
# library(shinydashboard)
# library(shinydashboardPlus)
# library(shinyFeedback)
# library(shinyWidgets)
# library(waiter)

# NOT INSTALLED
# library(shinythemes)
# library(pROC)
# library(ROCpAI)
# library(dplyr)
# library(tools)
# library(readr)
# library(htmltools)

# Manages app autoreload
config <- function(...) {
    options(shiny.host = "127.0.0.1")
    options(shiny.port = 8000)
    options(shiny.autoreload = TRUE)
    options(shiny.launch.browser = FALSE)
}

#' @importFrom shiny shinyApp fluidPage
roc_app <- function(...) {
    # DEV configuration
    config()
    ui <- main_ui("main")
    server <- function(input, output, session) {
        main_server("main")
    }
    shinyApp(ui, server)
}
