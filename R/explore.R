# TODO: update values of numRangeInput
explore_ui <- function(id, title) {
  ns <- NS(id)
  shinydashboard::box(
    id = "box-container",
    class = "full-box",
    width = 12,
    title = title,
    status = "primary",
    headerBorder = FALSE,
    useShinyFeedback(),
    full_box_dep(),
    summary_dep(),
    numRangeDeps(),
    explore_dep(),
    box_dep(),
    button_dep(),
    shiny::fluidRow(
      shiny::div(
        class = "exp-var-col-4",
        selectInput(
          inputId = ns("summary_var"),
          label = "Variable selection",
          choices = NULL
        ),
        uiOutput(
          outputId = ns("second_var_container")
        ),
        selectInput(
          inputId = ns("transformation"),
          label = "Transformations",
          choices = NULL
        ),
        textBtnInput(
          ns("save_data"),
          placeholder = "Save changes as new dataset...",
          btn_text = "Save",
          btn_icon = "floppy-disk"
        ),
        textInput(
          inputId = ns("new_var"),
          label = NULL,
          placeholder = "New variable name"
        ),
        uiOutput(
          outputId = ns("transf_opts")
        ),
        shiny::uiOutput(
          class = "center-container",
          outputId = ns("transf_container")
        )
      ),
      shiny::div(
        class = "exp-plot-col-8",
        shiny::div(
          class = "summary-plot-container",
          shiny::div(
            class = "summary-plot-element",
            plotlyOutput(
              outputId = ns("summary_plot"),
              height = "550px"
            )
          )
        ),
        infoBoxOutput(
          outputId = ns("var_type"),
          width = 12
        ) %>% tagAppendAttributes(
          class = "center-box"
        )
      )
    )
  )
}

explore_server <- function(id, selected_dataset, data_storage) {
  moduleServer(id, function(input, output, session) {
    transformations <- list(
      num = list(
        "Transform to categoric" = "to_cat"
      ),
      cat = list(
        "Relabel categories" = "relab_cat",
        "Merge categories" = "merge_cat"
      ),
      other = list(
        "-" = "-"
      )
    )
    temp_dataset <- reactiveVal(NULL)
    var <- reactive({
      req(input$summary_var)
      temp_dataset()[[input$summary_var]]
    })
    var_type <- reactive({
      req(input$summary_var)
      pillar::type_sum(temp_dataset()[[input$summary_var]])
    })
    var_class <- reactive({
      req(var())
      if (is_numeric_var(var())) {
        return("num")
      } else if (is_cat_var(var())) {
        return("cat")
      } else {
        return("other")
      }
    })
    second_var <- reactive({
      req(input$second_var)
      temp_dataset()[[input$second_var]]
    })
    opts_number <- reactiveVal(2)
    observeEvent(selected_dataset(), {
      updateSelectInput(
        inputId = "summary_var",
        choices = names(selected_dataset())
      )
      temp_dataset(selected_dataset())
    })
    observeEvent(temp_dataset(), {
      updateSelectInput(
        inputId = "summary_var",
        choices = names(temp_dataset())
      )
    })
    observeEvent(var(), {
      var_class()
      if (var_class() == "num") {
        choices <- transformations[["num"]]
      } else if (var_class() == "cat") {
        choices <- transformations[["cat"]]
      } else if (var_class() == "other") {
        choices <- "-"
      }
      updateSelectInput(
        inputId = "transformation",
        choices = choices
      )
    })
    observeEvent(input$second_var, {
      is_cat <- is_cat_var(second_var())
      is_null <- input$second_var == "-"
      if (is_cat | is_null) {
        shinyFeedback::hideFeedback(
          inputId = "second_var"
        )
      } else {
        shinyFeedback::showFeedbackWarning(
          inputId = "second_var",
          text = "Second variable should be categorical"
        )
      }
    })
    observeEvent(input$add_option, {
      opts_number(opts_number() + 1)
    })
    observeEvent(input$remove_option, {
      if (opts_number() > 1) {
        opts_number(opts_number() - 1)
      } else {
        showNotification("Can't remove last option!", type = "error")
      }
    })
    observeEvent(input$cancel, {
      removeModal()
    })
    observeEvent(input$continue, {
      removeModal()
      result <- transf_var(
        input = input,
        transformation = input$transformation,
        number = opts_number(),
        temp_dataset = temp_dataset(),
        sum_var = input$summary_var,
        new_var = input$new_var
      )
      if (!is.null(result)) {
        temp_dataset(result)
        showNotification(
          str_glue("Variable `{input$new_var}` created"),
          type = "message"
        )
      }
    })
    observeEvent(input$transform, {
      req(input$transformation)
      if (input$new_var %in% names(temp_dataset())) {
        showModal(
          modal_override_variable(
            var_name = input$new_var,
            session = session
          )
        )
      } else {
        result <- transf_var(
          input = input,
          transformation = input$transformation,
          number = opts_number(),
          temp_dataset = temp_dataset(),
          sum_var = input$summary_var,
          new_var = input$new_var
        )
        if (!is.null(result)) {
          temp_dataset(result)
          showNotification(
            str_glue("Variable `{input$new_var}` created"),
            type = "message"
          )
        }
      }
    })
    observeEvent(input$cancel_dataset, {
      removeModal()
    })
    observeEvent(input$continue_dataset, {
      removeModal()
      extend_reactive_list(
        data_storage,
        list2("{input$save_data_name}" := isolate(temp_dataset()))
      )
      showNotification(
        str_glue("Dataset `{input$save_data_name}` created"),
        type = "message"
      )
    })
    observeEvent(input$save_data_btn, {
      req(input$save_data_name)
      if (input$save_data_name %in% names(data_storage())) {
        showModal(
          modal_override_dataset(
            dataset_name = input$save_data_name,
            session = session
          )
        )
      } else {
        extend_reactive_list(
          data_storage,
          list2("{input$save_data_name}" := isolate(temp_dataset()))
        )
        showNotification(
          str_glue("Dataset `{input$save_data_name}` created"),
          type = "message"
        )
      }
    })
    output$second_var_container <- renderUI({
      if (is_cat_var(var())) {
        NULL
      } else if (is_numeric_var(var())) {
        selectInput(
          inputId = session$ns("second_var"),
          label = "Second variable (plot)",
          choices = c("-", names(temp_dataset()))
        )
      }
    })
    output$transf_container <- renderUI({
      btn <- actionButton(
        class = "btn btn-primary custom-btn trans-btn",
        inputId = session$ns("transform"),
        label = "Transform",
        icon = icon("gear")
      )
      if (input$transformation != "-") {
        btn
      } else {
        btn %>% tagAppendAttributes(
          disabled = "disabled"
        )
      }
    })
    output$summary_plot <- renderPlotly({
      req(var())
      if (var_class() == "cat") {
        result <- cat_plotly(
          temp_dataset(),
          input$summary_var
        )
      } else if (var_class() == "num") {
        req(input$second_var)
        result <- numeric_plotly(
          data = temp_dataset(),
          num_variable = input$summary_var,
          cat_variable = input$second_var
        )
      } else if (var_class() == "other") {
        result <- empty_plotly()
      }
      result
    })
    output$var_type <- renderInfoBox({
      shinydashboard::infoBox(
        title = "Variable Type",
        value = var_type(),
        icon = icon("code"),
        color = "aqua"
      )
    })
    output$transf_opts <- renderUI({
      req(
        input$summary_var %in% colnames(temp_dataset()),
        input$transformation %in% transformations[[var_class()]]
      )
      if (input$transformation == "to_cat") {
        result <- draw_cat_opts(opts_number(), session)
      } else if (input$transformation == "relab_cat") {
        result <- draw_relabel_opts(
          session = session,
          temp_dataset = temp_dataset(),
          sum_var = input$summary_var
        )
      } else if (input$transformation == "merge_cat") {
        result <- draw_merge_opts(
          session = session,
          temp_dataset = temp_dataset(),
          sum_var = input$summary_var
        )
      } else if (input$transformation == "-") {
        result <- draw_unknown_opts(
          var_name = input$summary_var,
          var_type = var_type()
        )
      }
      result
    })
  })
}

explore_app <- function() {
  ui <- dashboardPage(
    header = dashboardHeader(title = NULL),
    sidebar = dashboardSidebar(
      minified = FALSE,
      collapsed = FALSE,
      selectInput(
        inputId = "test_data_sel",
        label = "Select dataset",
        choices = NULL
      )
    ),
    body = dashboardBody(
      explore_ui("test", "test-title")
    )
  )
  server <- function(input, output, session) {
    test_dataset <- fast_colon %>% mutate(
      test_chr_var = case_when(
        Hsa.3004 <= 4877.3647 ~ "cat_1",
        4877.3647 <= Hsa.3004 & Hsa.3004 <= 7015 ~ "cat_2",
        Hsa.3004 >= 7015 ~ "cat_3",
      ),
      test_fct_var = factor(
        case_when(
          Hsa.3004 <= 4877.3647 ~ "cat_1",
          4877.3647 <= Hsa.3004 & Hsa.3004 <= 7015 ~ "cat_2",
          Hsa.3004 >= 7015 ~ "cat_3",
        )
      )
    )
    data_storage <- reactiveVal(list(
      fast_colon = fast_colon,
      test_dataset = test_dataset
    ))
    selected_dataset <- reactive({
      data_storage()[[input$test_data_sel]]
    })
    observe({
      shiny::updateSelectInput(
        inputId = "test_data_sel",
        choices = names(data_storage())
      )
    })
    explore_server("test", selected_dataset, data_storage)
  }
  shinyApp(ui, server)
}

numeric_var_summary <- function(num_variable) {
  quantiles <- quantile(num_variable, probs = c(0.25, 0.75))
  list(
    min = min(num_variable),
    max = max(num_variable),
    median = median(num_variable),
    mean = mean(num_variable),
    quantile_1 = quantiles[[1]],
    quantile_3 = quantiles[[2]]
  )
}

categorical_var_summary <- function(cat_variable) {
  var_name <- names(cat_variable)[1]
  count(cat_variable, .data[[var_name]])
}

is_numeric_var <- function(var) {
  var_type <- pillar::type_sum(var)
  numeric_types <- c("dbl", "int")
  if (var_type %in% numeric_types) TRUE else FALSE
}

is_cat_var <- function(var) {
  var_type <- pillar::type_sum(var)
  cat_types <- c("lgl", "chr", "fct")
  if (var_type %in% cat_types) TRUE else FALSE
}

cat_rows <- function(summary_dataset) {
  table_rows <- tagList()
  summary_dataset[[1]] <- as.character(summary_dataset[[1]])
  for (i in seq_along(1:nrow(summary_dataset))) {
    table_rows <- tagAppendChild(
      table_rows,
      tags$tr(
        tags$td(str_glue("{summary_dataset[i,1]}")),
        tags$td(str_glue("{summary_dataset[i,2]}"))
      )
    )
  }
  return(table_rows)
}

textBtnInput <- function(
    id,
    value = NULL,
    placeholder = NULL,
    btn_text,
    btn_icon) {
  shiny::div(
    id = id,
    class = "input-group text-button",
    button_dep(),
    text_btn_dep(),
    textInput(
      inputId = str_glue("{id}_name"),
      placeholder = placeholder,
      value = value,
      label = NULL
    ) %>% add_text_input_class("form-control"),
    tags$span(
      class = "input-group-btn",
      shiny::actionButton(
        inputId = str_glue("{id}_btn"),
        class = "btn btn-primary custom-btn",
        label = btn_text,
        icon = icon(btn_icon)
      )
    )
  )
}

numeric_plot <- function(data, num_variable, cat_variable) {
  conditions <- c(
    is.null(cat_variable),
    cat_variable == "-",
    !is_cat_var(data[[cat_variable]])
  )
  if (any(conditions)) {
    num_plot <- ggplot(data = data) +
      ggplot2::geom_violin(
        mapping = ggplot2::aes(
          y = .data[[{{ num_variable }}]],
          x = 1
        )
      ) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          y = .data[[{{ num_variable }}]],
          x = 1
        )
      ) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
  } else {
    num_plot <- ggplot(data = data) +
      ggplot2::geom_violin(
        mapping = ggplot2::aes(
          y = .data[[{{ num_variable }}]],
          x = .data[[{{ cat_variable }}]]
        )
      ) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          y = .data[[{{ num_variable }}]],
          x = .data[[{{ cat_variable }}]]
        )
      )
  }
  num_plot
}

numeric_plotly <- function(data, num_variable, cat_variable) {
  conditions <- c(
    is.null(cat_variable),
    cat_variable == "-",
    !is_cat_var(data[[cat_variable]])
  )
  num_variable <- rlang::new_formula(NULL, sym(num_variable))
  cat_variable <- rlang::new_formula(NULL, sym(cat_variable))
  if (any(conditions)) {
    plotly::plot_ly(
      data, y = num_variable, type = "violin", name = "", points = "all",
      box = list(visible = TRUE)
    ) %>%
      plotly::layout(
        yaxis = list(
          zeroline = FALSE
        ),
        xaxis = list(
          showticklabels = FALSE
        )
      ) %>%
      plotly::config(
        displayModeBar = FALSE
      )
  } else {
    plotly::plot_ly(
      data,
      y = num_variable,
      x = cat_variable,
      type = "violin",
      points = "all",
      box = list(visible = TRUE)
    ) %>% plotly::layout(
      yaxis = list(
        zeroline = FALSE
      )
    ) %>% plotly::config(
      displayModeBar = FALSE
    )
  }
}

cat_plot <- function(data, cat_variable) {
  ggplot(data = data) +
    ggplot2::geom_bar(
      ggplot2::aes(x = .data[[cat_variable]])
    )
}

cat_plotly <- function(data, cat_variable) {
  cat_variable <- sym(cat_variable)
  cat_formula <- rlang::new_formula(NULL, cat_variable)
  counts_data <- data %>%
    count(.data[[cat_variable]]) %>%
    mutate(!!cat_variable := as.character(!!cat_variable)) %>%
    replace_na(list2(!!cat_variable := "NA"))
  plot_ly(
    counts_data,
    y = ~n,
    x = cat_formula,
    type = "bar"
  ) %>%
    layout(
      yaxis = list(
        title = "Count"
      )
    ) %>%
    plotly::config(
      displayModeBar = FALSE
    )
}

empty_plotly <- function() {
  axis_conf <- list2(
    showline = TRUE,
    showgrid = FALSE,
    showticklabels = TRUE,
    linecolor = 'rgb(204, 204, 204)',
    linewidth = 2,
    ticks = 'outside',
    tickcolor = 'rgb(204, 204, 204)',
    tickwidth = 2,
    ticklen = 5,
    tickfont = list(family = "Arial", size = 12, color = "rgb(82, 82, 82)"),
    zeroline = FALSE,
    rangemode = "tozero"
  )
  fig <- plot_ly(type = "scatter", mode = "markers") %>%
    plotly::layout(
      xaxis = list2(title = "x", !!!axis_conf),
      yaxis = list2(title = "y", !!!axis_conf),
      plot_bgcolor = "rgb(255, 255, 255)",
      showlegend = FALSE
    ) %>%
    plotly::config(displayModeBar = FALSE)
  fig
}

draw_cat_opt <- function(number, session) {
  # TODO: update with max and min values of function
  list(
    shiny::textInput(
      inputId = session$ns(str_glue("cat_opt_{number}")),
      placeholder = str_glue("Category {number} name"),
      label = NULL
    ),
    numRangeInput(
      id = session$ns(str_glue("cat_ran_{number}")),
      value = c(10, 20)
    )
  )
}

draw_cat_opts <- function(number = reactiveVal(1), session) {
  cats <- tagList()
  for (n in seq_along(1:number)) {
    cats <- tagAppendChild(
      cats,
      draw_cat_opt(n, session)
    )
  }
  list(
    tags$div(
      class = "options-container",
      cats
    ),
    tags$div(
      button_dep(),
      class = "btn-group options-btn-group",
      role = "group",
      shiny::actionButton(
        inputId = session$ns("add_option"),
        class = "btn-success custom-btn option-btn",
        label = "Add option",
        icon = icon("plus")
      ),
      shiny::actionButton(
        inputId = session$ns("remove_option"),
        class = "btn-danger custom-btn option-btn",
        label = "Remove option",
        icon = icon("trash")
      )
    )
  )
}

transf_cat <- function(
    input,
    number,
    temp_dataset,
    sum_var,
    new_var) {
  cat_exprs <- exprs()
  if (is.null(new_var) | new_var == "") {
    return(showModal(modal_missing_var()))
  }
  for (n in seq_along(1:number)) {
    cat_opt <- input[[str_glue("cat_opt_{n}")]]
    cat_val <- input[[str_glue("cat_ran_{n}")]]
    name_not_null <- !(is.null(cat_opt) | cat_opt == "")
    val_not_null <- !is.null(cat_val)
    sum_var <- sym(sum_var)
    new_var <- sym(new_var)
    if (name_not_null && val_not_null) {
      exp <- expr(
        !!sum_var <= !!cat_val[2] & !!sum_var >= !!cat_val[1] ~ !!cat_opt
      )
      cat_exprs <- append(cat_exprs, exp)
    }
  }
  if (length(cat_exprs) == 0) {
    showModal(modal_missing_input())
  } else {
    temp_dataset %>% mutate(
      !!new_var := factor(
        case_when(!!!cat_exprs)
      )
    )
  }
}

draw_relabel_opt <- function(session, value) {
  tags$div(
    class = "input-group exp-rename-opt",
    tags$span(
      class = "input-group-addon",
      value
    ),
    shiny::textInput(
      inputId = session$ns(str_glue("relab_{value}")),
      label = NULL,
      placeholder = str_glue("New name for `{value}`")
    ) %>% add_text_input_class("form-control")
  )
}

draw_relabel_opts <- function(
    session,
    temp_dataset,
    sum_var) {
  opts <- tagList()
  sum_var <- sym(sum_var)
  values <- temp_dataset %>% distinct(!!sum_var) %>% pull(!!sum_var)
  for (value in values) {
    opts <- tagAppendChild(opts, draw_relabel_opt(session, value))
  }
  tags$div(
    class = "options-container",
    opts
  )
}

transf_relabel <- function(
    input,
    temp_dataset,
    sum_var,
    new_var) {
  if (is.null(new_var) | new_var == "") {
    return(showModal(modal_missing_var()))
  }
  sum_var <- sym(sum_var)
  new_var <- sym(new_var)
  values <- temp_dataset %>% distinct(!!sum_var) %>% pull(!!sum_var)
  relab_exprs <- exprs()
  for (value in values) {
    new_name <- input[[str_glue("relab_{value}")]]
    if (!is.null(new_name) && new_name != "") {
      print("[*]In expr")
      relab_expr <- expr(!!new_name := !!value)
      relab_exprs <- append(relab_exprs, relab_expr)
    } else {
      shiny::showNotification(
        str_glue("No new name defined for `{value}`"),
        type = "warning"
      )
    }
  }
  if (length(relab_exprs) > 0) {
    temp_dataset %>% mutate(
      !!new_var := fct_recode(!!sum_var, !!!relab_exprs)
    )
  } else {
    showModal(modal_missing_all_cats())
  }
}

draw_merge_opt <- function(
    session,
    value) {
  tags$div(
    class = "input-group exp-rename-opt",
    tags$span(
      class = "input-group-addon",
      value
    ),
    shiny::textInput(
      inputId = session$ns(str_glue("merge_{value}")),
      label = NULL,
      placeholder = str_glue("New category for `{value}`")
    ) %>% add_text_input_class("form-control")
  )
}

draw_merge_opts <- function(
    session,
    temp_dataset,
    sum_var) {
  opts <- tagList()
  sum_var <- sym(sum_var)
  values <- temp_dataset %>% distinct(!!sum_var) %>% pull(!!sum_var)
  for (value in values) {
    opts <- tagAppendChild(opts, draw_merge_opt(session, value))
  }
  tags$div(
    class = "options-container",
    opts
  )
}

draw_unknown_opts <- function(
  var_name,
  var_type) {
  div(
    box(
      headerBorder = FALSE,
      status = "warning",
      width = 12,
      tags$p(
        str_glue("Can't plot or transform `{var_name}`. Variable is of type "),
        tags$code(var_type),
        " which can't be used by the app."
      )
    ) %>% remove_box_header()
  )
}

transf_merge <- function(
    input,
    temp_dataset,
    sum_var,
    new_var) {
  if (is.null(new_var) || new_var == "") {
    return(showModal(modal_missing_var()))
  }
  sum_var <- sym(sum_var)
  new_var <- sym(new_var)
  values <- temp_dataset %>% distinct(!!sum_var) %>% pull(!!sum_var)
  merge_exprs <- exprs()
  for (value in values) {
    new_name <- input[[str_glue("merge_{value}")]]
    if (!is.null(new_name) && new_name != "") {
      merge_expr <- expr(!!new_name := !!value)
      merge_exprs <- append(merge_exprs, merge_expr)
    } else {
      showNotification(
        str_glue("No new category defined for `{value}`"),
        type = "warning"
      )
    }
  }
  if (length(merge_exprs) > 0) {
    temp_dataset %>% mutate(
      !!new_var := fct_collapse(!!sum_var, !!!merge_exprs)
    )
  } else {
    showModal(modal_missing_all_cats())
  }
}

transf_var <- function(
  input,
  transformation,
  number,
  temp_dataset,
  sum_var,
  new_var
) {
  if (transformation == "to_cat") {
    result <- transf_cat(
      input = input,
      number = number,
      temp_dataset = temp_dataset,
      sum_var = sum_var,
      new_var = new_var
    )
  } else if (transformation == "relab_cat") {
    result <- transf_relabel(
      input = input,
      temp_dataset = temp_dataset,
      sum_var = sum_var,
      new_var = new_var
    )
  } else if (transformation == "merge_cat") {
    result <- transf_merge(
      input = input,
      temp_dataset = temp_dataset,
      sum_var = sum_var,
      new_var = new_var
    )
  }
  result
}

modal_override_variable <- function(var_name, session) {
  modalDialog(
    title = "Variable in use",
    str_glue(
      "`{var_name}` is already in use. Do you want to override it?"
    ),
    footer = tagList(
      actionButton(session$ns("cancel"), "Cancel"),
      actionButton(session$ns("continue"), "Continue", class = "btn btn-danger")
    ),
    easyClose = FALSE
  )
}

modal_override_dataset <- function(dataset_name, session) {
  modalDialog(
    title = "Dataset already in use",
    str_glue(
      "`{dataset_name}` is already stored. Do you want to override it?"
    ),
    footer = tagList(
      actionButton(
        session$ns("cancel_dataset"), "Cancel"
      ),
      actionButton(
        session$ns("continue_dataset"), "Continue", class = "btn btn-danger"
      )
    ),
    easyClose = FALSE
  )
}

modal_missing_input <- function() {
  modalDialog(
    title = "Missing inputs",
    "At least 1 input is required to transform the variable.",
    easyClose = TRUE
  )
}

modal_missing_var <- function() {
  modalDialog(
    title = "Missing variable name",
    "Please introduce a valid name for new variable"
  )
}

modal_missing_all_cats <- function() {
  modalDialog(
    title = "Missing category names",
    "At least one category should have a valid name.
    Please, check provided names are valid"
  )
}

summary_dep <- function() {
  htmltools::htmlDependency(
    name = "summary-dep",
    version = "1.0.0",
    src = c(file = "./www/css/"),
    stylesheet = "summary.css"
  )
}

explore_dep <- function() {
  htmlDependency(
    name = "explore-dep",
    version = "1.0.0",
    src = c(file = "./www/css/"),
    stylesheet = "explore.css"
  )
}

button_dep <- function() {
  htmlDependency(
    name = "button-dep",
    version = "1.0.0",
    src = c(file = "./www/css/"),
    stylesheet = "buttons.css"
  )
}

text_btn_dep <- function() {
  htmlDependency(
    name = "text-btn-dep",
    version = "1.0.0",
    src = c(file = "./www/css/"),
    stylesheet = "text_buttons.css"
  )
}

box_dep <- function() {
  htmlDependency(
    name = "box-dep",
    version = "1.0.0",
    src = c(file = "./www/css/"),
    stylesheet = "boxes.css"
  )
}


# library(plotly)
# library(rlang)
#   axis_conf <- list2(
#     showline = TRUE,
#     showgrid = FALSE,
#     showticklabels = TRUE,
#     linecolor = 'rgb(204, 204, 204)',
#     linewidth = 2,
#     ticks = 'outside',
#     tickcolor = 'rgb(204, 204, 204)',
#     tickwidth = 2,
#     ticklen = 5,
#     tickfont = list(family = "Arial", size = 12, color = "rgb(82, 82, 82)"),
#     zeroline = FALSE,
#     rangemode = "tozero"
#   )
#   fig <- plot_ly() %>%
#     plotly::layout(
#       xaxis = list2(title = "x", !!!axis_conf),
#       yaxis = list2(title = "y", !!!axis_conf),
#       plot_bgcolor = "rgb(255, 255, 255)",
#       showlegend = FALSE
#     ) %>%
#     config(displayModeBar = FALSE)
#   fig