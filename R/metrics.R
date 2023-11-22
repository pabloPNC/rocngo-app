metrics_ui <- function(id, title) {
    list(
        h1(title),
        p(text),
        fluidRow(
            column(
                width = 8,
                uiOutput(NS(id, "plot-space"))
            ),
            column(
                width = 4,
                box(
                    width = 12,
                    height = 200,
                    title = "Configuration",
                    collapsible = TRUE,
                    selectInput(
                        inputId = "gold_config_sensitivity",
                        label = "Select Gold Standard",
                        choices = NULL
                    ),
                    selectInput(
                        inputId = "scores_config_sensitivity",
                        label = "Select Test Scores",
                        choices = NULL
                    ),
                    sliderInput(
                        inputId = "threshold_sensitivity",
                        label = "Select threshold",
                        value = 50,
                        max = 100,
                        min = 0
                    )
                ),
                uiOutput("sensitivity_scores")
            )
        )
    )
}


metrics_server <- function(id) {
    moduleServer(id, function(input, output, session) {

    })
}


metrics_app <- function(id) {
    ui <- "PLACEHOLDER"

    server <- function(input, output, session) {
        print("PLACEHOLDER")
    }
}