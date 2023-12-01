
# UI - Body/Main conents of Data/Manage
manage_contents <- list(
    h1("Data Manage"),
    tabsetPanel(
        id = "preview_type",
        type = "hidden",
        selected = "manage_preview",
        tabPanelBody(
            "manage_preview",
            tableOutput("preview_output")
        ),
        tabPanelBody(
            "manage_str",
            verbatimTextOutput("str_output")
        ),
        tabPanelBody(
            "manage_summary",
            verbatimTextOutput("summary_output")
        )
    )
)


# Contains HTML for side options if
# Data/Manage option is picked
manage_panel_contens <- list(
    radioButtons(
        "previewOption",
        "Display",
        choiceNames = c(
            "Preview",
            "str",
            "summary"
        ),
        choiceValues = c(
            "preview",
            "str",
            "summary"
        )
    )
)


view_contents <- list(
    h1("View Data"),
    fluidRow(
        column(
            width = 9,
            offset = 1,
            box(
                width = 12,
                dataTableOutput("view_output")
            )
        )
    )
)

view_panel_contens <- list(
    selectInput(
        "select_variables",
        label = "Select variables to show",
        choices = NULL,
        multiple = TRUE
    )
)

# View ------------------------------------------------------------------------
view_data_ui <- function(id) {
    list(
        fluidRow(
            column(
                width = 12,
                box(
                    dataTableOutput(NS(id, "dataset")),
                    title = "View",
                    status = "primary",
                    width = 12,
                    headerBorder = FALSE
                )
            )
        )
    )
}

# TODO: Show table again. Not showing because of data read
view_data_server <- function(id, input_dataset) {
    moduleServer(id, function(input, output, session) {
        output$dataset <- renderDataTable(
            input_dataset(),
            options = list(
                pageLength = 15,
                lengthMenu = list(
                    c(5, 10, 15),
                    c("5", "10", "15")
                )
            )
        )
    })
}


view_data_app <- function() {

    test_dataset <- reactive({
        data("iris")
        iris
    })

    ui <- fluidPage(
        view_data_ui("test")
    )

    server <- function(input, output, session) {
        view_data_server("test", test_dataset)
    }
    shinyApp(ui, server)
}

# Explore --------------------------------------------------------------------
numeric_summary <- function(x) {
  c(
    Min = min(x), Max = max(x), `1stQu` = quantile(x, 0.25),
    Median = median(x), Mean = mean(x),
    `3rdQu` = quantile(x, 0.75)
  )
}


explore_data_ui <- function(id) {
    list(
        h1("Explore data"),
        fluidRow(
            column(
                width = 12,
                box(
                    selectInput(
                        inputId = NS(id, "variables"),
                        label = "Variables",
                        choices = NULL,
                        multiple = TRUE
                    ),
                    selectInput(
                        inputId = NS(id, "operation"),
                        label = "Operation",
                        choices = c("Summary", "str")
                    ),
                    width = 4
                ),
                box(
                    uiOutput(NS(id, "result")),
                    width = 8
                )
            )
        )
    )
}


explore_data_server <- function(id, input_dataset) {
    moduleServer(id, function(input, output, session) {

        column_names <- reactive({
            colnames(input_dataset())
        })

        numeric_variables <- reactive({
            colnames(
                select_if(
                    input_dataset(),
                    is.numeric
                )
            )
        })

        factor_variables <- reactive({
            colnames(
                select_if(
                    input_dataset(),
                    is.factor
                )
            )
        })


        observeEvent(column_names(), {
            updateSelectInput(
                session,
                inputId = "variables",
                choices = column_names(),
                selected = column_names()[1]
            )
        })


        output$result <- renderUI(
            if (input$operation == "Summary") {
                list(
                    tableOutput(
                        NS(id, "number_summary")
                    ),
                    tableOutput(
                        NS(id, "factor_summary")
                    )
                )
            } else {
                h1("Aqui no hay nah de nah")
            }
        )

        output$number_summary <- renderTable({
            var_names <- input$variables[
                input$variables %in% numeric_variables()
            ]
            df <- select(input_dataset(), var_names)
            summaries <- lapply(df, numeric_summary)
            data.frame(summaries)
        }, rownames = TRUE)


        output$factor_summary <- renderTable({
            var_names <- input$variables[
                input$variables %in% factor_variables()
            ]
            df <- select(input_dataset(), var_names)
            if (length(var_names) > 0) {
                table(df)
            }
        }, rownames = FALSE)
    })
}


explore_data_app <- function() {
    test_dataset <- reactive({
        data("iris")
        iris_mod <- iris
        iris_mod$species_2 <- factor(rep(c(1, 2), 75))
        iris_mod
    })
    ui <- fluidPage(
        explore_data_ui("test_explore")
    )
    server <- function(input, output, session) {
        explore_data_server("test_explore", test_dataset)
    }
    shinyApp(ui, server)
}


# Upload --------------------------------------------------------------------
upload_data_ui <- function(id) {
    list(
        fluidRow(
            column(
                width = 12,
                box(
                    list(
                        tags$p(
                            "Select the files you want to use. 
                            Uploaded files must be in one of the following
                            formats:"
                        ),
                        formattableOutput(
                            outputId = NS(id, "file_formats")
                        ),
                        fileInput(
                            inputId = NS(id, "upload"),
                            label = "Upload your file",
                            buttonLabel = "Upload",
                            multiple = TRUE
                        ),
                        uiOutput(NS(id, "dataset_list"))
                    ),
                    title = "Upload data",
                    headerBorder = FALSE,
                    width = 12,
                    status = "primary"
                )
            )
        )
    )
}


upload_data_server <- function(id, data) {
    moduleServer(id, function(input, output, session) {

        observeEvent(input$upload, {

            for (pos in seq_along(length(input$upload$name))) {
                dataset_name <- input$upload$name[pos]
                del_index <- paste0("delete_", dataset_name)

                data$uploaded_datasets[[dataset_name]] <- read.csv(
                    input$upload$datapath[pos]
                )

                observeEvent(input[[del_index]], {
                    data$uploaded_datasets[[dataset_name]] <- NULL
                    updateSelectInput(
                        session,
                        "select_dataset",
                        choices = names(data$uploaded_datasets)
                    )
                }, once = TRUE, ignoreInit = TRUE)

            }
        })

        # PLACEHOLDER: dataset_container_server
        data_inside <- reactiveValues(
            delete_bttn_ids = list(),
            download_bttn_ids = list()
        )

        output$dataset_list <- renderUI({
            for (dataset_name in names(data$uploaded_datasets)) {
                down_index <- paste0("download_", dataset_name)
                del_index <- paste0("delete_", dataset_name)
                data_inside$delete_bttn_ids[[del_index]] <- del_index
            }
            lapply(
                X = names(data$uploaded_datasets),
                FUN = function(i) {
                    uploaded_file_container(id, i)
                }
            )
        })

        output$file_formats <- renderFormattable(
            formattable(
                data.frame(Name = c("JSON"), Extension = "*.json")
            )
        )
    })
}


upload_data_app <- function() {
    ui <- fluidPage(
        upload_data_ui("test")
    )
    server <- function(input, output, session) {
        data(iris)
        data <- reactiveValues(
            uploaded_datasets = list()
        )
        upload_data_server("test", data)
    }
    shinyApp(ui, server)
}

uploaded_file_container <- function(id, dataset_name) {
    ui <- function(...) {
        withTags(
            div(
                id = NS(id, paste0(dataset_name, "_", "container")),
                class = "list-group list-group-horizontal",
                div(dataset_name, class = "list-group-item"),
                downloadButton(
                    outputId = NS(id, paste0("download_", dataset_name)),
                    label = "Download",
                    icon = icon("floppy-disk"),
                    class = "btn-success list-group-item"
                ),
                actionButton(
                    inputId = NS(id, paste0("delete_", dataset_name)),
                    label = "Delete",
                    icon = icon("trash"),
                    class = "btn-danger list-group-item"
                )
            )
        )
    }

    dependency <- function(...) {
        htmlDependency(
            name = "css_dependency",
            version = "1.0",
            src = c(file = "./www/css/"),
            stylesheet = "uploaded_files_list.css"
        )
    }
    tagList(ui(), dependency())
}

dataset_container_app <- function(...) {
    data(iris)
    library(ggplot2)
    mpg <- ggplot2::mpg

    dataset_storage <- reactiveValues(
        dataset_1 = iris,
        dataset_2 = mpg
    )

    ui <- fluidPage(
        dataset_container_ui("test")
    )
    server <- function(input, output, session) {
        dataset_container_server("test", dataset_storage)
    }

    shinyApp(ui, server)
}
