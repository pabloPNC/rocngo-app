
compareContents <- list(
    h1("Compare"),
    box(
        title = "Selector",
        pickerInput(
          inputId = "graphicSelector",
          label = "Select plots to show",
          choices = NULL,
          multiple = T
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
    )
)
