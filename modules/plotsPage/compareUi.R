
compareContents <- list(
    fluidRow(
        column(
            width = 6,
            box(
                title = "Selector",
                width = NULL,
                pickerInput(
                inputId = "graphicSelector",
                label = "Select plots to show",
                choices = NULL,
                multiple = TRUE
                ),
                textInput(
                    label = "Introduce plot name",
                    inputId = "combinedPlotName",
                ),
                actionButton(
                    inputId = "plotButton",
                    label = "Plot!",
                    icon = icon("check"),
                    class = "btn-block btn-success"
                )
            ),
            offset = 3
        ),
        style = "padding-top: 30px"
    )
)
