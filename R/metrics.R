metrics_ui <- function(id, title) {
    ns <- NS(id)
    box(
        width = 12,
        title = title,
        status = "primary",
        headerBorder = FALSE,
        useShinyFeedback(),
        useWaiter(),
        full_box_dep(),
        metrics_dep(),
        plots_dep(),
        class = "full-box",
        shiny::div(
            class = "plot-container",
            shiny::div(
                class = "plot-element",
                plotOutput(
                    outputId = ns("plot")
                )
            )
        ),
        fluidRow(
            column(
                width = 8,
                offset = 2,
                sliderInput(
                    inputId = ns("threshold"),
                    label = "Select threshold",
                    value = 50,
                    max = 100,
                    min = 0
                )
            )
        ),
        fluidRow(
            column(
                width = 4,
                offset = 2,
                selectInput(
                    inputId = ns("gold_standard"),
                    label = "Select gold standard",
                    choices = NULL
                ),
                selectInput(
                    inputId = ns("predictor"),
                    label = "Select predictor",
                    choices = NULL
                )
            ),
            column(
                width = 4,
                selectInput(
                    inputId = ns("index"),
                    label = "Select index",
                    choices = NULL
                ),
                shiny::checkboxGroupInput(
                    inputId = ns("options"),
                    label = "Plot options",
                    choices = list(
                        "Hide bounds" = "bound",
                        "Hide threshold" = "threshold",
                        "Hide chance line" = "chance_line"
                    )
                )
            )
        ),
        fluidRow(
            shiny::uiOutput(
                class = "metrics-container",
                outputId = ns("metrics")
            )
        )
    )
}


metrics_server <- function(id, dataset, functions, bound_func, ratio) {
    moduleServer(id, function(input, output, session) {
        dataset_variables <- shiny::reactive({
            colnames(dataset())
        })
        functions_data <- reactiveValues(
            functions_names = names(functions)
        )
        hide_bounds <- shiny::reactive({
            "bound" %in% input$options
        })
        hide_threshold <- shiny::reactive({
            "threshold" %in% input$options
        })
        hide_chance_line <- shiny::reactive({
            "chance_line" %in% input$options
        })
        auc <- shiny::reactive({
            var_type_req(input$gold_standard, input$predictor, dataset())
            pROC::auc(
                response = dataset()[[input$gold_standard]],
                predictor = dataset()[[input$predictor]],
                direction = "<",
                quiet = TRUE
            )
        })
        pauc <- shiny::reactive({
            if (ratio == "tpr") {
                transformed_ratio <- "sens"
                bounds <- c(input$threshold / 100, 1)
            } else if (ratio == "fpr") {
                transformed_ratio <- "spec"
                bounds <- c((1 - input$threshold / 100), 1)
            }
            as.double(
                pROC::auc(
                    response = dataset()[[input$gold_standard]],
                    predictor = dataset()[[input$predictor]],
                    direction = "<",
                    quiet = TRUE,
                    partial.auc = bounds,
                    partial.auc.focus = transformed_ratio
                )
            )
        })
        partial_index <- shiny::reactive({
            if (ratio == "tpr") {
                functions[[input$index]](
                    dataset(),
                    .data[[input$gold_standard]],
                    .data[[input$predictor]],
                    input$threshold / 100
                )
            } else if (ratio == "fpr") {
                functions[[input$index]](
                    dataset(),
                    .data[[input$gold_standard]],
                    .data[[input$predictor]],
                    0,
                    input$threshold / 100
                )
            }
        })
        curve_shape <- shiny::reactive({
            if (ratio == "tpr") {
                lower_threshold = input$threshold / 100
                upper_threshold = 1
            } else if (ratio == "fpr") {
                lower_threshold = 0
                upper_threshold = input$threshold / 100
            }
            ROCnGO::calc_curve_shape(
                dataset(),
                response = .data[[input$gold_standard]],
                predictor = .data[[input$predictor]],
                lower_threshold = lower_threshold,
                upper_threshold = upper_threshold,
                ratio = ratio
            )
        })
        shiny::observe({
            # TEMP observer to check values
            var_type_req(input$gold_standard, input$predictor, dataset())
            print(
                ROCnGO::summarize_predictor(
                    dataset(),
                    response = .data[[input$gold_standard]],
                    predictor = .data[[input$predictor]],
                    ratio = "tpr",
                    threshold = input$threshold / 100
                )
            )
            print(
                ROCnGO::summarize_predictor(
                    dataset(),
                    response = .data[[input$gold_standard]],
                    predictor = .data[[input$predictor]],
                    ratio = "fpr",
                    threshold = input$threshold / 100
                )
            )
        })
        shiny::observe({
            shiny::req(input$gold_standard)
            if (!is_binary(input$gold_standard, dataset())) {
                shinyFeedback::showFeedbackWarning(
                    inputId = "gold_standard",
                    text = "Variable should be a factor"
                )
            } else {
                shinyFeedback::hideFeedback("gold_standard")
            }
        })
        shiny::observe({
            shiny::req(input$predictor)
            if (!is_continuous(input$predictor, dataset())) {
                shinyFeedback::showFeedbackWarning(
                    inputId = "predictor",
                    text = "Variable should be continuous"
                )
            } else {
                shinyFeedback::hideFeedback("predictor")
            }
        })
        observe({
            updateSelectInput(
                inputId = "gold_standard",
                choices = dataset_variables()
            )
            updateSelectInput(
                inputId = "predictor",
                choices = dataset_variables()
            )
        })
        observeEvent(functions_data$functions_names, {
            updateSelectInput(
                inputId = "index",
                choices = functions_data$functions_names
            )
        })
        output$plot <- renderPlot({
            req(input$gold_standard)
            req(dataset())
            if (is_binary(input$gold_standard, dataset()) &
                is_continuous(input$predictor, dataset())) {
                draw_roc_plot(
                    dataset(),
                    input$predictor,
                    input$gold_standard,
                    input$threshold,
                    ratio,
                    draw_chance_line = !hide_chance_line(),
                    draw_bounds = !hide_bounds(),
                    draw_threshold = !hide_threshold(),
                    bound_fun = bound_func[[input$index]]
                )
            } else {
                draw_empty_plot(
                    input$threshold,
                    ratio,
                    !hide_chance_line(),
                    !hide_threshold()
                )
            }},
            res = 100
        )
        output$metrics <- shiny::renderUI({
            if (var_type_cond(input$gold_standard, input$predictor, dataset())) {
                draw_metric_boxes(
                    input$index,
                    format_metric(auc()),
                    format_metric(pauc()),
                    format_metric(partial_index()),
                    curve_shape()
                )
            } else {
                draw_metric_boxes(input$index,"-","-","-","-")
            }
        })
    })
}

metrics_tpr_app <- function(id) {
    ui <- dashboardPage(
        header = dashboardHeader(title = NULL),
        sidebar = dashboardSidebar(minified = FALSE, collapsed = TRUE),
        body = dashboardBody(
            metrics_ui("test", "Test TPR")
        )
    )
    server <- function(input, output, session) {
        test_dataset <- shiny::reactive(fast_colon)
        metrics_server(
            "test",
            test_dataset,
            list(
                "NpAUC" = ROCnGO::np_auc,
                "FpAUC" = ROCnGO::fp_auc
            ),
            list(
                "NpAUC" = ROCnGO::add_npauc_normalized_lower_bound,
                "FpAUC" = ROCnGO::add_fpauc_lower_bound
            ),
            "tpr"
        )
    }
    shinyApp(ui, server)
}

metrics_fpr_app <- function(id) {
    ui <- dashboardPage(
        header = dashboardHeader(title = NULL),
        sidebar = dashboardSidebar(minified = FALSE, collapsed = TRUE),
        body = dashboardBody(
            metrics_ui("test", "Test FPR")
        )
    )
    server <- function(input, output, session) {
        metrics_server(
            "test",
            fast_colon,
            list(
                "SpAUC" = ROCnGO::sp_auc,
                "TpAUC" = ROCnGO::tp_auc
            ),
            list(
                "SpAUC" = ROCnGO::add_spauc_lower_bound,
                "TpAUC" = ROCnGO::add_tpauc_lower_bound
            ),
            "fpr"
        )
    }
    shinyApp(ui, server)
}

is_binary <- function(variable, dataset) {
    is.factor(dataset[[variable]])
}

is_continuous <- function(variable, dataset) {
    is.numeric(dataset[[variable]])
}

var_type_cond <- function(response, predictor, dataset) {
    is_binary(response, dataset) & is_continuous(predictor, dataset)
}

var_type_req <- function(response, predictor, dataset) {
    req(response, predictor)
    req(
        var_type_cond(response, predictor, dataset)
    )
}

draw_chance_line <- function(expr) {
    if (expr == TRUE) {
        add_chance_line()
    } else {
        NULL
    }
}

draw_threshold_line <- function(expr, threshold, ratio) {
    if (expr == TRUE) {
        add_threshold_line(threshold/100, ratio)
    } else {
        NULL
    }
}

draw_bounds <- function(expr, fun, data, response, predictor, threshold, ratio) {
    if (expr == TRUE) {
        if (ratio == "tpr") {
            fun(
                data,
                response = {{ response }},
                predictor = {{ predictor }},
                threshold = threshold / 100
            )
        } else if (ratio == "fpr") {
            fun(
                data,
                response = {{ response }},
                predictor = {{ predictor }},
                lower_threshold = 0,
                upper_threshold = threshold / 100
            )
        }
    } else {
        NULL
    }
}

draw_empty_plot <- function(
        threshold,
        ratio,
        draw_chance_line = TRUE,
        draw_threshold = TRUE) {
    ggplot() +
        xlim(0, 1) +
        ylim(0, 1) +
        labs(x = "FPR", y = "TPR") +
        geom_blank() +
        draw_chance_line(draw_chance_line) +
        draw_threshold_line(draw_threshold, threshold, ratio)
}

draw_roc_plot <- function(
        dataset,
        predictor,
        response,
        threshold,
        ratio,
        draw_chance_line = TRUE,
        draw_threshold = TRUE,
        draw_bounds = TRUE,
        bound_fun) {
    plot_roc_curve(
        data = dataset,
        predictor = .data[[predictor]],
        response = .data[[response]]
    ) +
        draw_chance_line(draw_chance_line) +
        draw_threshold_line(draw_threshold, threshold, ratio) +
        draw_bounds(
            draw_bounds,
            bound_fun,
            dataset,
            .data[[response]],
            .data[[predictor]],
            threshold,
            ratio
        ) +
        hide_legend()
}

draw_metric_boxes <- function(
    partial_index_name,
    auc,
    pauc,
    partial_index,
    curve_shape) {
    list(
        valueBox(
            subtitle = "AUC",
            value = stringr::str_glue("{auc}"),
            color = "light-blue",
            icon = shiny::icon("percent"),
            width = 3
        ),
        valueBox(
            subtitle = "pAUC",
            value = stringr::str_glue("{pauc}"),
            color = "aqua",
            icon = shiny::icon("percent"),
            width = 3
        ),
        valueBox(
            subtitle = stringr::str_glue("{partial_index_name}"),
            value = stringr::str_glue("{partial_index}"),
            color = "light-blue",
            icon = shiny::icon("percent"),
            width = 3
        ),
        valueBox(
            subtitle = "Curve shape",
            value = stringr::str_glue("{curve_shape}"),
            color = "aqua",
            icon = shiny::icon("chart-area"),
            width = 3
        )
    )
}

format_metric <- function(metric, digits = 3) {
    format(round(metric, digits), nsmall = digits)
}

full_box_dep <- function() {
    htmltools::htmlDependency(
        name = "full-box",
        version = "1.0",
        src = c(file = "./www/css/"),
        stylesheet = "full_box.css"
    )
}

metrics_dep <- function() {
    htmltools::htmlDependency(
        name = "metrics",
        version = "1.0",
        src = c(file = "./www/css/"),
        stylesheet = "metrics.css"
    )
}

plots_dep <- function() {
    htmltools::htmlDependency(
        name = "plots",
        version = "1.0",
        src = c(file = "./www/css/"),
        stylesheet = "plots.css"
    )
}
