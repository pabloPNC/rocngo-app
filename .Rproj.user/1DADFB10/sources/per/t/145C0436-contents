
# Update selector with stored Plots ---------------------------------------

to_observe <- reactive({
    list(
        stored_plots[["sensitivity"]], 
        stored_plots[["specificity"]]
    )
})


observeEvent(
    to_observe(), 
    {
        sensitivity_plot_names <- names(stored_plots[["sensitivity"]])
        specificity_plot_names <- names(stored_plots[["specificity"]])
        print(sensitivity_plot_names)
        print(specificity_plot_names)
        condition <- length(sensitivity_plot_names) > 1 & length(specificity_plot_names) > 1
        updatePickerInput(
            session,
            "graphicSelector",
            choices = list(
                Sensitivity = as.list(names(stored_plots[["sensitivity"]])),
                Specificity = as.list(names(stored_plots[["specificity"]]))
            )
        )
    }
)


# Create graph --------------------------------------------------------------


