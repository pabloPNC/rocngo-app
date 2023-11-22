# general content ---------------------------------------------------------

plot_cover <- box(
    width = 12,
    title = "Parameters",
    "Lore Ipsum"
)


# sensitivity contents ----------------------------------------------------
sensitivity_plot_content <- box(
    width = 12,
    title = "Plot",
    plotOutput("sensitivity_plot", height = 320)
)

sensitivity_configuration_content <- box(
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
)

sensitivity_save_plot_menu <- box(
    width = 12,
    title = "Save Plot",
    textInput(
        label = "Introduce plot name",
        inputId = "plot_name_sensitivity",
    ),
    actionButton(
        inputId = "save_plot_sensitivity",
        label = "Save Plot",
        icon = icon("floppy-o"),
        class = "btn-block"
    )
)

# TODO: Included in main.R
#sensitivity_contents <- list(
    #useShinyFeedback(),
    #h1("Sensitivity Contents"),
    #p("Texto descriptivo del apartado de Plot"),
    #fluidRow(
        #column(
            #width = 8,
            #uiOutput("sensitivity_plot_space")
        #),
        #column(
            #width = 4,
            #sensitivity_configuration_content,
            #uiOutput("sensitivity_scores")
        #)
    #)
#)


# specificity contents ----------------------------------------------------

specificity_plot_content <- box(
    width = 12,
    title = "Plot",
    plotOutput("specificity_plot", height = 320)
)

# TODO: Included in main.R
#specificity_configuration_content <- box(
    #collapsible = TRUE,
    #width = 12,
    #height = 200,
    #title = "Configuration",
    #selectInput(
        #inputId = "gold_config_specificity",
        #label = "Select Gold Standard",
        #choices = NULL
    #),
    #selectInput(
        #inputId = "scores_config_specificity",
        #label = "Select Test Scores",
        #choices = NULL
    #),
    #sliderInput(
        #inputId = "threshold_specificity",
        #label = "Select threshold",
        #value = 50,
        #max = 100,
        #min = 0
    #)
#)

specificity_save_plot_menu <- box(
    width = 12,
    title = "Save Plot",
    textInput(
        label = "Introduce plot name",
        inputId = "plot_name_specificity"
    ),
    actionButton(
        inputId = "save_plot_specificity",
        label = "Save Plot",
        icon = icon("floppy-o", verify_fa = FALSE),
        class = "btn-block"
    )
)

# TODO: Included in main.R
#specificityContents <- list(
    #h1("Specificity Contents"),
    #p("Texto descriptivo del apartado de Plot"),
    #fluidRow(
        #column(
            #width = 8,
            #uiOutput("specificity_plot_space")
        #),
        #column(
            #width = 4,
            #specificity_configuration_content,
            #uiOutput("specificity_scores")
        #)
    #)
#)




# Tenemos que introducir varios puntos:
#   - pAUC

#   - SpAUC <- Especificidad. Paper antiguo, tendría que hacerlo a mano
#   - TpAUC <- Especificadad. Arregla los desastres del anterior. Esta en ROCpAI

#   - NpAUC <- Sensibilidad. Paper antiguo, tendria que hacerlo a mano
#   - FpAUC <- Sensibilidad. Arregla el anterior. En el material suplementario
