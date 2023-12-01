general_panel_content <- list(
    selectInput(
        "database",
        label = "Dataset Selection",
        choices = NULL
    )
)

main_ui <- dashboardPage(
    preloader = list(
        html = tagList(
            spin_1()
        ),
        color = "#3c8dbc"
    ),
    header = dashboardHeader(title = "FpAUC"),
    sidebar = dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            general_panel_content,
            tabsetPanel(
                id = "switcher",
                type = "hidden",
                selected = "emptyPanel",
                tabPanelBody("emptyPanel", ""),
                tabPanelBody("managePanel", manage_panel_contens),
                tabPanelBody("viewPanel", view_panel_contens)
            ),
            menuItem(
                "Data",
                tabName = "data",
                icon = icon("table"),
                menuSubItem("Manage", tabName = "manage"),
                menuSubItem("View", tabName = "view"),
                menuSubItem("Explore", tabName = "explore"),
                menuSubItem("Upload", tabName = "upload")
            ),
            menuItem(
                "Plot",
                tabName = "plot",
                icon = icon("chart-line"),
                menuSubItem("Sensitivity", tabName = "sensitivity"),
                menuSubItem("Specificity", tabName = "specificity"),
                menuSubItem("Compare", tabName = "compare")
            ),
            menuItem(
                "Report",
                tabName = "report",
                icon = icon("book")
            ),
            menuItem(
                "About",
                tabName = "about",
                icon = icon("info")
            )
        ),
        collapsed = FALSE,
        minified = FALSE
    ),
    body = dashboardBody(
        tabItems(
            tabItem(
                tabName = "manage",
                manage_contents
            ),
            tabItem(
                tabName = "view",
                view_data_ui("dataset-preview")
            ),
            tabItem(
                tabName = "explore",
                explore_data_ui("explore-page")
            ),
            tabItem(
                tabName = "upload",
                upload_data_ui("upload-page")
            ),
            tabItem(
                tabName = "sensitivity",
                list(
                    useShinyFeedback(),
                    h1("Sensitivity Contents"),
                    p("Texto descriptivo del apartado de Plot"),
                    fluidRow(
                        column(
                            width = 8,
                            uiOutput(
                                "sensitivity_plot_space"
                            )
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
            ),
            tabItem(
                tabName = "specificity",
                list(
                    h1("Specificity Contents"),
                    p("Texto descriptivo del apartado de Plot"),
                    fluidRow(
                        column(
                            width = 8,
                            uiOutput("specificity_plot_space")
                        ),
                        column(
                            width = 4,
                            box(
                                collapsible = TRUE,
                                width = 12,
                                height = 200,
                                title = "Configuration",
                                selectInput(
                                    inputId = "gold_config_specificity",
                                    label = "Select Gold Standard",
                                    choices = NULL
                                ),
                                selectInput(
                                    inputId = "scores_config_specificity",
                                    label = "Select Test Scores",
                                    choices = NULL
                                ),
                                sliderInput(
                                    inputId = "threshold_specificity",
                                    label = "Select threshold",
                                    value = 50,
                                    max = 100,
                                    min = 0
                                )
                            ),
                            uiOutput("specificity_scores")
                        )
                    )
                )
            ),
            tabItem(tabName = "report", h1("Report contents here")),
            tabItem(tabName = "about", h1("About contents here"))
        )
    ),
    controlbar = dashboardControlbar(),
    title = "FpAUC"
)


main_server <- function(input, output, session) {
    # TODO: Fix rds files from /data
    data_tabs <- c("manage", "view", "emptyPanel", "managePanel", "viewPanel",
        "explorePanel", "visualizePanel"
    )

    # TODO: Read RDS files
    data <- reactiveValues(
        default_datasets = list(
            avengers = avengers,
            colon = colon,
            diamonds = diamonds,
            fast_colon = fast_colon,
            publishers = publishers,
            titanic = titanic
        ),
        uploaded_datasets = list(),
        plots = list(
            sensitivity = list(),
            specificity = list()
        )
    )

    observe({
        updateSelectInput(
            session,
            inputId = "database",
            choices = names(c(data$default_datasets, data$uploaded_datasets))
        )
    })

    # Selects contents in the menu
    observeEvent(input$tabs, updateTabsetPanel(
            session,
            input = "switcher",
            paste0(input$tabs, "Panel")
        )
    )

    observeEvent(
        input$tabs, {
        if (!(input$tabs %in% data_tabs)) {
            updateTabsetPanel(
                session,
                input = "switcher",
                selected = "emptyPanel"
            )
        }
    })

    # data.R server
    dataset <- reactive({
        req(input$database)
        if (input$database %in% names(data$uploaded_datasets)) {
            data$uploaded_datasets[[input$database]]
        } else if (input$database %in% names(data$default_datasets)) {
            data$default_datasets[[input$database]]
        }
    })

    filtered_dataset <- reactive({
        req(input$database)
        if (length(input$select_variables) > 0) {
            select(dataset(), input$select_variables)
        } else if (ncol(dataset()) > 10) {
            dataset()[, c(1:10)]
        } else {
            dataset()
        }
    })

    observeEvent(input$previewOption, updateTabsetPanel(
        session,
        "preview_type",
        paste0("manage_", input$previewOption)
    ))

    # TODO: Should be removed -> Manage screen not used
    output$preview_output <- renderTable({
        if (nrow(dataset()) > 10) {
            dataset()[1:10, ]
        } else {
            dataset()
        }
    })

    output$str_output <- renderPrint({
        str(dataset())
    })

    output$summary_output <- renderPrint({
        summary(dataset())
    })

    observe({
        updateSelectInput(
            session,
            inputId = "select_variables",
            choices = colnames(dataset())
        )
    })

    # TODO: dt resizing when window changes
    # https://datatables.net/forums/discussion/40852/columns-dont-resize-on-window-resize-unless-scrollx-is-true
    # TODO: fix huge datasets long time to load
    view_data_server("dataset-preview", filtered_dataset)
    explore_data_server("explore-page", filtered_dataset)
    # TODO: delete file extension from uploads
    # TODO: Fix uploading two files > only uploads one
    # TODO: add download buttns
    upload_data_server("upload-page", data)
    # TODO: Create modules for sensitivity/specificity



    # values ------------------------------------------------------------------
    stored_plots <- reactiveValues(
        sensitivity = list(),
        specificity = list()
    )


    # sensitivity -------------------------------------------------------------

    observeEvent(input$save_plot_sensitivity, {
        gold_column <- input$gold_config_sensitivity
        column <- input$scores_config_sensitivity
        dataset <- select(dataset(), column, gold_column)
        plot_name <- input$plot_name_sensitivity
        plot_exists <- !is.null(stored_plots[["sensitivity"]][[plot_name]])
        threshold <- input$threshold_sensitivity

        if (!plot_exists) {
            hideFeedback("plot_name_sensitivity")
            stored_plots[["sensitivity"]][[plot_name]] <- list(
                dataset = dataset,
                roc_points = ROCpoints(dataset[[gold_column]], dataset[[column]]),
                threshold = threshold
            )
            updateTextInput(session, "plot_name_sensitivity", value = "")
            print(stored_plots[["sensitivity"]])
        } else {
            showFeedbackWarning(
                inputId = "plot_name_sensitivity",
                text = "Plot name already in use"
            )
        }
    })

    observeEvent(input$gold_config_sensitivity, {
        dataset <- dataset()
        column <- input$gold_config_sensitivity
        condition <- CheckGoldStandard(dataset[[column]]) || input$gold_config_sensitivity == "-"

        if (!condition) {
            showFeedbackWarning(
            inputId = "gold_config_sensitivity",
            text = "Gold Standard must be binary"
            )
        } else {
            hideFeedback("gold_config_sensitivity")
        }
    })

    observeEvent(input$scores_config_sensitivity, {
        dataset <- dataset()
        column <- input$scores_config_sensitivity
        condition <- is.numeric(dataset[[column]]) || input$scores_config_sensitivity == "-"
        if (!condition) {
            showFeedbackWarning(
            inputId = "scores_config_sensitivity",
            text = "Score must be numeric"
            )
        } else {
            hideFeedback("scores_config_sensitivity")
        }
    })


    output$sensitivity_plot_space <- renderUI({
        dataset <- dataset()
        gold_column <- input$gold_config_sensitivity
        column <- input$scores_config_sensitivity

        condition1 <- CheckGoldStandard(dataset[[gold_column]]) || input$gold_config_sensitivity == "-"
        condition2 <- is.numeric(dataset[[column]])

        if (condition1 && condition2) {
            list(sensitivity_plot_content, sensitivity_save_plot_menu)
        } else {
            plot_cover
        }
    })

    output$sensitivity_plot <- renderPlot({
        dataset <- dataset()
        gold_column <- input$gold_config_sensitivity
        scores_column <- input$scores_config_sensitivity
        threshold <- input$threshold_sensitivity / 100
        roc_points <- as.data.frame(
            ROCpoints(dataset[[gold_column]],
            dataset[[scores_column]])
        )
        hs_roc_points <- as.data.frame(
            pHSpoints(dataset[[gold_column]],
            dataset[[scores_column]], threshold)
        )

        ggplot() +
            geom_point(data = roc_points, mapping = aes(x = fpr, y = tpr)) +
            geom_point(
                data = hs_roc_points,
                mapping = aes(x = fprp, y = tprp),
                color = "red"
            )
    })


    output$sensitivity_scores <- renderUI({
        dataset <- dataset()
        gold_column <- input$gold_config_sensitivity
        column <- input$scores_config_sensitivity

        condition1 <- CheckGoldStandard(dataset[[gold_column]]) || input$gold_config_sensitivity == "-"
        condition2 <- is.numeric(dataset[[column]])

        if (condition1 && condition2) {
            auc_result <- round(
                as.numeric(auc(dataset[[gold_column]],
                dataset[[column]])),
                3)
            pauc_result <- round(
            as.numeric(
                auc(dataset[[gold_column]],
                    dataset[[column]],
                    partial.auc = c(input$threshold_sensitivity/100, 1),
                    partial.auc.focus = "sensitivity"
                    ) 
                ),
            3)
            fpauc_result <- round(
            as.numeric(
                FpaucHS(dataset[[gold_column]],
                    dataset[[column]],
                    input$threshold_sensitivity/100
                    )
                ),
            3)
            box(
            title = "Metrics",
            width = 12,
            tags$p(
                HTML(paste(tags$b("AUC:"), auc_result, sep = " "))
            ),
            tags$p(
                HTML(paste(tags$b("pAUC:"), pauc_result, sep = " "))
            ),
            tags$p(
                HTML(paste(tags$b("FpAUC:"), fpauc_result, sep = " "))
            )
            )
        } 
    })


    observeEvent(dataset(), {
        updateSelectInput(session, "gold_config_sensitivity", choices = c(colnames(dataset()), "-"), selected = "-")
    })

    observeEvent(dataset(), {
        updateSelectInput(session, "scores_config_sensitivity", choices = c(colnames(dataset()), "-"), selected = "-")
    })





    # specificity -------------------------------------------------------------

    observeEvent(dataset(), {
        updateSelectInput(session, "gold_config_specificity", choices = c(colnames(dataset()), "-"), selected = "-")
    })

    observeEvent(dataset(), {
        updateSelectInput(session, "scores_config_specificity", choices = c(colnames(dataset()), "-"), selected = "-")
    })


    observeEvent(input$gold_config_specificity, {
        dataset <- dataset()
        column <- input$gold_config_specificity
        condition <- CheckGoldStandard(dataset[[column]]) || input$gold_config_specificity == "-"

        if (!condition) {
            showFeedbackWarning(
                inputId = "gold_config_specificity",
                text = "Gold Standard must be binary"
            )
        } else {
            hideFeedback("gold_config_specificity")
        }
    })


    observeEvent(input$scores_config_specificity, {
        dataset <- dataset()
        column <- input$scores_config_specificity
        condition <- is.numeric(dataset[[column]]) || input$scores_config_specificity == "-"

        if (!condition) {
            showFeedbackWarning(
                inputId = "scores_config_specificity",
                text = "Score must be numeric"
            )
        } else {
            hideFeedback("scores_config_specificity")
        }
    })


    output$specificity_plot_space <- renderUI({
        dataset <- dataset()
        gold_column <- input$gold_config_specificity
        column <- input$scores_config_specificity

        condition1 <- CheckGoldStandard(dataset[[gold_column]]) || input$gold_config_specificity == "-"
        condition2 <- is.numeric(dataset[[column]])

        if (condition1 && condition2) {
            list(specificity_plot_content, specificity_save_plot_menu)
        } else {
            plot_cover
        }
    })


    output$specificity_plot <- renderPlot({
        dataset <- dataset()
        gold_column <- input$gold_config_specificity
        scores_column <- input$scores_config_specificity
        threshold <- input$threshold_specificity / 100
        roc_points <- as.data.frame(
            ROCpoints(dataset[[gold_column]], dataset[[scores_column]])
        )
        # x = gold_standar, y = scores
        hsp_roc_points <- as.data.frame(pHSPEpoints(roc_points, threshold))
        ggplot() +
            geom_point(data = roc_points, mapping = aes(x = fpr, y = tpr)) +
            geom_point(data = hsp_roc_points, mapping = aes(x = fpr, y = tpr), color = "red")
    })


    # TODO: Show metrics
    output$specificity_scores <- renderUI({
        dataset <- dataset()
        gold_column <- input$gold_config_specificity
        column <- input$scores_config_specificity

        condition1 <- CheckGoldStandard(dataset[[gold_column]]) || input$gold_config_specificity == "-"
        condition2 <- is.numeric(dataset[[column]])

        if (condition1 && condition2) {
            auc_result <- round(
                as.numeric(auc(dataset[[gold_column]],
                dataset[[column]])),
                3
            )
            pauc_result <- round(
            as.numeric(
                auc(dataset[[gold_column]],
                    dataset[[column]],
                    partial.auc = c(input$threshold_specificity/100, 1),
                    partial.auc.focus = "specificity"
                    )
                ),
            3)

            filtered_dataset <- select(dataset, gold_column, column)

            tpauc.ext <- tpAUC(
            dataset = filtered_dataset,
            plot = FALSE,
            low.value = input$threshold_specificity/100,
            up.value = 1
            )
            tpauc.assays <- assays(tpauc.ext)
            Tpauc_result <- round(tpauc.assays[[1]]$St_pAUC[[1]],3)
            box(
                title = "Metrics",
                width = 12,
                tags$p(
                    HTML(paste(tags$b("AUC:"), auc_result, sep = " "))
                ),
                tags$p(
                    HTML(paste(tags$b("pAUC:"), pauc_result, sep = " "))
                ),
                tags$p(
                    HTML(paste(tags$b("TpAUC:"), Tpauc_result, sep = " "))
                )
            )
        }
    })

    observeEvent(input$save_plot_specificity, {
        gold_column <- input$gold_config_specificity
        column <- input$scores_config_specificity
        dataset <- select(dataset(), column, gold_column)
        plot_name <- input$plot_name_specificity
        plot_exists <- !is.null(stored_plots[["specificity"]][[plot_name]])
        threshold <- input$threshold_specificity

        if (!plot_exists) {
            hideFeedback("plot_name_specificity")
            stored_plots[["specificity"]][[plot_name]] <- list(
                dataset = dataset,
                roc_points = ROCpoints(dataset[[gold_column]], dataset[[column]]),
                threshold = threshold
            )
            updateTextInput(session, "plot_name_specificity", value = "")
        } else {
            showFeedbackWarning(
                inputId = "plot_name_specificity",
                text = "Plot name already in use"
            )
        }
    })
}
