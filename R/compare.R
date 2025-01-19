compare_ui <- function(id, title) {
  ns <- NS(id)
  box(
    id = "box-container",
    class = "full-box",
    width = 12,
    title = title,
    status = "primary",
    headerBorder = FALSE,
    useShinyFeedback(),
    full_box_dep(),
    box_dep(),
    compare_dep(),
    button_dep(),
    fluidRow(
      column(
        width = 4,
        selectInput(
          inputId = ns("predictors"),
          label = "Select predictors",
          multiple = TRUE,
          choices = NULL
        ),
        selectizeInput(
          inputId = ns("response"),
          label = "Select response",
          choices = NULL
        ),
        selectizeInput(
          inputId = ns("index"),
          label = "Select index",
          choices = list(
            "Sensitivity" = c("NpAUC", "FpAUC"),
            "Specificitiy" = c("SpAUC", "TpAUC")
          )
        ),
        numericInput(
          inputId = ns("threshold"),
          label = "Select threshold",
          value = 50.00,
          step = 0.01,
          min = 0,
          max = 100
        ),
        shiny::uiOutput(
          outputId = ns("btn_container")
        )
      ),
      column(
        width = 8,
        tags$div(
          class = "plot-container",
          tags$div(
            class = "plot-element",
            plotOutput(
              outputId = ns("plot")
            )
          )
        )
      )
    ),
    tags$div(
      tags$h3("Summary", class = "comp-subtitle")
    ),
    fluidRow(
      column(
        width = 4,
        uiOutput(
          outputId = ns("metrics_boxes")
        )
      ),
      column(
        width = 8,
        tags$div(
          class = "comp-table-container",
          reactableOutput(
            outputId = ns("metrics_table")
          )
        )
      )
    )
  )
}

compare_server <- function(id, selected_dataset) {
  moduleServer(id, function(input, output, session) {
    comp_plot <- reactiveVal(
      draw_comp_empty_plot(threshold_val = 50, ratio = "tpr")
    )
    comp_table <- reactiveVal({
      draw_empty_metrics_table()
    })
    ratio <- reactive({
      req(input$index)
      if (input$index %in% c("NpAUC", "FpAUC")) "tpr" else "fpr"
    })
    predictors_cond <- reactive({
      all(
        map_lgl(input$predictors, \(x) is_numeric_var(selected_dataset()[[x]]))
      )
    })
    response_cond <- reactive({
      is_cat_var(selected_dataset()[[input$response]])
    })
    observe({
      vars <- colnames(selected_dataset())
      updateSelectizeInput(
        inputId = "predictors",
        choices = vars,
        server = TRUE
      )
      updateSelectizeInput(
        inputId = "response",
        choices = vars,
        server = TRUE
      )
    })
    observe({
      req(input$response)
      feedbackWarning(
        inputId = "response",
        show = !response_cond(),
        text = "Variable should be categorical"
      )
    })
    observe({
      feedbackWarning(
        inputId = "predictors",
        show = !predictors_cond(),
        text = "Variable should be numeric"
      )
    })
    observeEvent(input$calculate, {
      if (length(input$predictors) > 0) {
        new_plot <- draw_comp_plot(
          selected_dataset(),
          predictors = input$predictors,
          response = input$response,
          threshold_val = input$threshold,
          ratio = ratio()
        )
        new_table <- draw_metrics_table(
          data = selected_dataset(),
          predictors = input$predictors,
          response = input$response,
          index_choice = input$index,
          threshold_val = input$threshold
        )
        comp_plot(new_plot)
        comp_table(new_table)
      }
    })
    output$metrics_boxes <- renderUI({
      if ("metric" %in% colnames(comp_table())) {
        metric_name <- "Metric"
        max_auc <- "-"
        metric_title <- "-"
        metric_value <- "-"
      } else {
        max_auc <- slice_max(
          comp_table(),
          order_by = auc,
          n = 1
        )[["predictor"]]
        metric_name <- isolate(input$index)
        metric_value <- slice_max(
          comp_table(),
          order_by = .data[[transform_index_name(metric_name)]],
          n = 1
        )[["predictor"]]
      }
      list(
        valueBox(
          max_auc,
          "Max AUC",
          icon = icon("percent"),
          width = 12,
          color = "light-blue"
        ),
        valueBox(
          metric_value,
          str_glue("Max {metric_name}"),
          icon = icon("percent"),
          width = 12,
          color = "aqua"
        )
      )
    })
    output$btn_container <- renderUI({
      btn <- actionButton(
        inputId = session$ns("calculate"),
        label = "Calculate",
        icon = icon("gear"),
        class = "btn-primary custom-btn trans-btn"
      )
      if (!predictors_cond() || !response_cond()) {
        tags$div(
          class = "center-box",
          btn %>% tagAppendAttributes(disabled = "disabled")
        )
      } else {
        tags$div(
          class = "center-box",
          btn
        )
      }
    })
    output$metrics_table <- renderReactable({
      if ("metric" %in% colnames(comp_table())) {
        metric_col <- "metric"
        metric_name <- "Metric"
      } else {
        metric_col <- transform_index_name(isolate(input$index))
        metric_name <- isolate(input$index)
      }
      reactable(
        comp_table(),
        compact = TRUE,
        columns = list2(
          predictor = colDef(name = "Predictor"),
          auc = colDef(name = "AUC", format = colFormat(digits = 3)),
          pauc = colDef(name = "pAUC", format = colFormat(digits = 3)),
          "{metric_col}" := colDef(
            name = metric_name,
            format = colFormat(
              digits = 3
            )
          ),
          curve_shape = colDef(name = "Curve shape")
        )
      )
    })
    output$plot <- renderPlot({
      comp_plot()
    })
  })
}

compare_app <- function() {
  ui <- test_ui(
    compare_ui("test", "test-title")
  )
  server <- function(input, output, session) {
    compare_server("test", fast_colon)
  }
  shinyApp(ui, server)
}

draw_comp_plot <- function(
    data,
    predictors,
    response,
    threshold_val,
    ratio,
    chance_line = TRUE,
    threshold = TRUE) {
  result <- draw_comp_empty_plot(
    threshold_val = threshold_val,
    ratio = ratio
  )
  if (length(predictors) > 0) {
    for (predictor in predictors) {
      result <- result + add_roc_curve(
        data = data,
        response = .data[[response]],
        predictor = .data[[predictor]]
      )
    }
  }
  result
}

draw_comp_empty_plot <- function(
    threshold_val,
    ratio,
    chance_line = TRUE,
    threshold = TRUE) {
  result <- ggplot() +
    xlim(0, 1) +
    ylim(0, 1) +
    labs(x = "FPR", y = "TPR", color = "Predictor") +
    geom_blank()
  if (chance_line == TRUE) {
    result <- result + add_chance_line()
  }
  if (threshold == TRUE) {
    result <- result + add_threshold_line(threshold_val / 100, ratio)
  }
  result
}

calc_pred_metrics <- function(
    data,
    predictor,
    response,
    ratio,
    threshold,
    metric_name) {
  metrics <- ROCnGO::summarize_predictor(
    data = data,
    predictor = .data[[predictor]],
    response = .data[[response]],
    ratio = ratio,
    threshold = threshold
  )
  list2(
    auc = metrics[["auc"]],
    pauc = metrics[["pauc"]],
    metric_name := metrics[[metric_name]],
    curve_shape = metrics[["curve_shape"]]
  )
}

calc_metrics_table <- function(
    data,
    response,
    predictor,
    threshold_val,
    index_choice) {
  ratio <- case_match(
    index_choice,
    c("NpAUC", "FpAUC") ~ "tpr",
    c("SpAUC", "TpAUC") ~ "fpr"
  )
  summarize_predictor(
    data,
    {{ predictor }},
    {{ response }},
    ratio,
    threshold_val / 100
  )
}

draw_metrics_table <- function(
    data,
    response,
    predictors,
    index_choice,
    threshold_val) {
  predictors_metrics <- tibble()
  for (predictor in predictors) {
    predictor_metrics <- calc_metrics_table(
      data,
      {{ response }},
      {{ predictor }},
      threshold_val,
      index_choice
    )
    predictor_metrics <- bind_cols(
      predictor = predictor,
      predictor_metrics[c(
        "auc", "pauc", transform_index_name(index_choice), "curve_shape"
      )]
    )
    predictors_metrics <- bind_rows(predictors_metrics, predictor_metrics)
  }
  predictors_metrics
}

draw_empty_metrics_table <- function() {
  tibble(
    predictor = "-",
    auc = "-",
    pauc = "-",
    metric = "-",
    curve_shape = "-"
  )
}

compare_dep <- function() {
  htmltools::htmlDependency(
    name = "compare-dep",
    version = "1.0.0",
    src = c(file = "./www/css/"),
    stylesheet = "compare.css"
  )
}