#' @importFrom shiny NS
#' @importFrom DT DTOutput
view_ui <- function(id, title) {
  ns <- NS(id)
  box(
    class = "full-box",
    width = 12,
    title = title,
    status = "primary",
    headerBorder = FALSE,
    # TODO: extract dependency
    full_box_dep(),
    shiny::fluidRow(
      shiny::column(
        width = 8,
        offset = 2,
        shiny::sliderInput(
          inputId = ns("column_slider"),
          label = "Columns to display",
          min = 1,
          max = 1,
          value = c(1, 1),
          step = 1
        ),
        numRangeInput(
          id = ns("column_range"),
          value = c(1, 1),
          min = 1,
          max = 1,
          step = 1
        )
      )
    ),
    DTOutput(
      outputId = ns("dataset")
    )
  )
}

#' @importFrom shiny moduleServer
view_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    dataset <- reactive({
      lower_index <- input$column_slider[[1]]
      upper_index <- input$column_slider[[2]]
      dplyr::select(data(), lower_index:upper_index)
    })
    shiny::observe(
      {
        column_min <- 1
        column_max <- ncol(data())
        if (is_large_dataset(data(), limit = 100)) {
          value <- c(1, 100)
        } else {
          value <- c(1, column_max)
        }
        shiny::updateSliderInput(
          inputId = "column_slider",
          min = column_min,
          max = column_max,
          value = value,
          step = 1
        )
        updateNumRangeInput(
          id = "column_range",
          min = column_min,
          max = column_max,
          value = value,
          step = 1
        )
      },
      priority = -1
    )
    observeEvent(input$column_range, {
      shiny::updateSliderInput(
        inputId = "column_slider",
        value = input$column_range
      )
    })
    observeEvent(input$column_slider, {
      updateNumRangeInput(
        id = "column_range",
        value = input$column_slider
      )
    })
    output$dataset <- DT::renderDT(
      dataset(),
      extensions = "Responsive",
      options = list(
        dom = "tip"
      )
    )
  })
}

#' @importFrom shiny fluidPage shinyApp
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboardPlus dashboardPage dashboardHeader dashboardSidebar
view_app <- function() {
  ui <- dashboardPage(
    header = dashboardHeader(title = NULL),
    sidebar = dashboardSidebar(minified = FALSE, collapsed = FALSE),
    body = dashboardBody(
      view_ui("test", "test-title")
    )
  )
  server <- function(input, output, session) {
    test_dataset <- shiny::reactiveVal(numbers)
    view_server("test", test_dataset)
  }
  shinyApp(ui, server)
}

#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody
view_large_app <- function() {
  ui <- dashboardPage(
    header = dashboardHeader(title = NULL),
    sidebar = dashboardSidebar(minified = FALSE, collapsed = FALSE),
    body = dashboardBody(
      view_ui("test", "test-title")
    )
  )
  server <- function(input, output, session) {
    test_dataset <- shiny::reactiveVal(repaired_colon)
    view_server("test", test_dataset)
  }
  shinyApp(ui, server)
}

is_large_dataset <- function(dataset, limit = 100) {
  ncol(dataset) > limit
}

numRangeInput <- function(
    id,
    value,
    min = NULL,
    max = NULL,
    step = NULL) {
  shiny::div(
    id = id,
    class = "shiny-input-container form-group numeric-range-input",
    numRangeDeps(),
    shiny::div(
      class = "input-group",
      shiny::tags$input(
        id = stringr::str_glue("{id}_lower"),
        class = "form-control",
        type = "number",
        min = min,
        max = max,
        value = value[[1]],
        step = 1
      ),
      shiny::span(
        class = "input-group-addon input-group-text",
        "to"
      ),
      shiny::tags$input(
        id = stringr::str_glue("{id}_upper"),
        class = "form-control",
        type = "number",
        min = min,
        max = max,
        value = value[[2]],
        step = 1
      )
    )
  )
}

numRangeDeps <- function() {
  htmltools::htmlDependency(
    name = "num_range_dependency",
    version = "1.0.0",
    src = c(file = "./www/js/"),
    script = "numRangeInputBinding.js"
  )
}

#' @importFrom purrr discard
#' @importFrom stringr str_glue
updateNumRangeInput <- function(
    session = shiny::getDefaultReactiveDomain(),
    id,
    value = NULL,
    min = NULL,
    max = NULL,
    step = NULL) {
  message <- list(
    value = value,
    min = min,
    max = max,
    step = step
  )
  message <- discard(message, is.null)
  session$sendInputMessage(id, message)
}
