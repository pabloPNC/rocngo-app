plot_comparison_ui <- function(id) {
    plotOuput(NS(id, "plot"))
}


plot_comparison_server <- function(id) {
    moduleServer(id, function(id, output, session) {

    })
}

# Old compareServer + compareUI

#compareContents <- list(
    #fluidRow(
        #column(
            #width = 6,
            #box(
                #title = "Selector",
                #width = NULL,
                #pickerInput(
                #inputId = "graphicSelector",
                #label = "Select plots to show",
                #choices = NULL,
                #multiple = TRUE
                #),
                #textInput(
                    #label = "Introduce plot name",
                    #inputId = "combinedPlotName",
                #),
                #actionButton(
                    #inputId = "plotButton",
                    #label = "Plot!",
                    #icon = icon("check"),
                    #class = "btn-block btn-success"
                #)
            #),
            #offset = 3
        #),
        #style = "padding-top: 30px"
    #)
#)


#to_observe <- reactive({
    #list(
        #stored_plots[["sensitivity"]],
        #stored_plots[["specificity"]]
    #)
#})


#observeEvent(
    #to_observe(),
    #{
        #sensitivity_plot_names <- names(stored_plots[["sensitivity"]])
        #specificity_plot_names <- names(stored_plots[["specificity"]])
        #print(sensitivity_plot_names)
        #print(specificity_plot_names)
        #condition <- length(sensitivity_plot_names) > 1 & length(specificity_plot_names) > 1
        #updatePickerInput(
            #session,
            #"graphicSelector",
            #choices = list(
                #Sensitivity = as.list(names(stored_plots[["sensitivity"]])),
                #Specificity = as.list(names(stored_plots[["specificity"]]))
            #)
        #)
    #}
#)
