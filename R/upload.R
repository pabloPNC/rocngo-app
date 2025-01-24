#' @importFrom shiny NS tags uiOutput
upload_ui <- function(id, title) {
  ns <- NS(id)
  shinydashboard::box(
    id = "box-container",
    class = "full-box",
    width = 12,
    title = title,
    status = "primary",
    headerBorder = FALSE,
    useWaiter(),
    full_box_dep(),
    shiny::h3("Valid formats"),
    shiny::p("During data uploading, some operations are performed over the data to ensure their usage in further steps. For this reason, only certain file formats are accepted. The list of allowed file formats is presented in the following table:"),
    tags$div(
      class = "table-responsive",
      shiny::tags$table(
        class = "table table-striped",
        tags$thead(
          tags$tr(
            tags$th("File"),
            tags$th("File extension"),
            tags$th("Notes"),
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Comma separated values"),
            tags$td(tags$code("*.csv")),
            tags$td("First row will correspond to column headers.")
          ),
          tags$tr(
            tags$td("Tab-separated values"),
            tags$td(tags$code("*.tsv")),
            tags$td("Same as ", tags$code("*.csv"), ".")
          ),
          tags$tr(
            tags$td("R Data Serialization"),
            tags$td(tags$code("*.rds")),
            tags$td(
              "Should contain a ",
              tags$code("SummarizedExperiment"),
              ", ",
              tags$code("data.frame"),
              "or ",
              tags$strong("tibble"),
              "(",
              tags$code("tbl_df"),
              ")",
              " object."
            )
          ),
        )
      )
    ),
    tags$h3("Uploaded files"),
    shiny::fileInput(
      inputId = ns("upload"),
      label = NULL,
      multiple = TRUE,
      buttonLabel = "Upload...",
      accept = c(".csv", ".tsv", ".rds")
    ),
    uiOutput(ns("stored"))
  )
}

#' @importFrom shiny moduleServer
#' @importFrom waiter Waiter transparent
upload_server <- function(id, data_storage) {
  moduleServer(id, function(input, output, session) {
    waiter <- Waiter$new(
      id = "box-container",
      html = waiter::spin_2(),
      color = transparent(alpha = 0.4)
    )
    data_storage_extensions <- reactiveVal(
      list()
    )
    observeEvent(input$upload, {
      waiter$show()
      import_datasets(
        upload_dataset = input$upload,
        storage = data_storage,
        storage_ext = data_storage_extensions,
        input = input,
        output = output
      )
      waiter$hide()
    })
    output$stored <- renderUI({
      draw_stored_table(
        data_storage,
        data_storage_extensions,
        id,
        session = session
      )
    })
  })
}

#' @importFrom shinydashboardPlus dashboardHeader dashboardPage dashboardSidebar
#' @importFrom shinydashboard dashboardBody
upload_app <- function() {
  ui <- dashboardPage(
    header = dashboardHeader(title = NULL),
    sidebar = dashboardSidebar(minified = FALSE, collapsed = FALSE),
    body = dashboardBody(
      upload_ui("test", "test title")
    )
  )
  server <- function(input, output, session) {
    uploaded_datasets <- shiny::reactiveVal({
      list()
    })
    upload_server("test", uploaded_datasets)
  }
  shinyApp(ui, server)
}

stored_file_row <- function(dataset_name, dataset_ext, ns_id, session) {
  tags$tr(
    download_button_dep(),
    delete_button_dep(),
    tags$td(str_glue("{dataset_name}")),
    tags$td(tags$code(str_glue("*.{dataset_ext}"))),
    tags$td(
      downloadButton(
        outputId = session$ns(str_glue("{dataset_name}_download")),
        class = "btn-success custom-download-btn",
        label = "Download"
      )
    ),
    tags$td(
      shiny::actionButton(
        inputId = session$ns(str_glue("{dataset_name}_delete")),
        label = "Delete",
        icon = icon("trash"),
        class = "btn btn-danger custom-delete-btn"
      )
    )
  )
}

draw_stored_table <- function(
    data_storage,
    data_storage_extensions,
    ns_id,
    session) {
  table_rows <- tagList()
  for (name in names(data_storage())) {
    table_rows <- tagAppendChild(
      table_rows,
      stored_file_row(name, data_storage_extensions()[[name]], ns_id, session)
    )
  }
  tags$div(
    class = "table-responsive",
    tags$table(
      class = "table table-striped table-hover",
      tags$thead(
        tags$tr(
          tags$th("Name"),
          tags$th("File type")
        )
      ),
      tags$tbody(
        tagList(table_rows)
      )
    )
  )
}

#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom rlang list2
import_datasets <- function(
    upload_dataset,
    storage,
    storage_ext,
    input,
    output) {
  for (i in 1:nrow(upload_dataset)) {
    dataset_info <- upload_dataset[i, ]
    dataset_name <- file_path_sans_ext(dataset_info[["name"]])
    dataset_ext <- file_ext(dataset_info[["name"]])
    dataset_path <- dataset_info[["datapath"]]
    valid_cond <- validate_dataset(
      dataset_name = dataset_name,
      dataset_ext = dataset_ext,
      dataset_path = dataset_path,
      storage = storage
    )
    if (valid_cond) {
      read_data <- read_data(dataset_path, dataset_ext)
      if (!is.null(read_data)) {
        load_data(
          read_data = read_data,
          data_name = dataset_name,
          data_ext = dataset_ext,
          storage = storage,
          storage_ext = storage_ext,
          input = input,
          output = output
        )
      } else if (is.null(read_data)) {
        showModal(modal_error_reading())
      }
    }
  }
}

#' @title
#' Reads rds files containing `SummarizedExperiment`, `data.frame`or `tibble`.
read_rds <- function(file) {
  file <- readRDS(file)
  types <- class(file)
  is_sumexp <- "SummarizedExperiment" %in% types
  is_rang_sumexp <- "RangedSummarizedExperiment" %in% types
  if (types %in% c("data.frame", "tbl_df")) {
    result <- as_tibble(file)
  } else if (is_sumexp && !(is_rang_sumexp)) {
    result <- sumexp_to_df(file)
  } else {
    stop("Couldn't read rds file")
  }
  result
}


#' @title
#' Tries to read a file based on its extension, otherwise returns no value.
read_data <- function(file, extension) {
  tryCatch(
    expr = switch(extension,
      "csv" = read_csv(file),
      "tsv" = read_tsv(file),
      "rds" = read_rds(file)
    ),
    error = function(condition) {
      return(NULL)
    }
  )
}

write_data <- function(object, file, extension) {
  switch(extension,
    "csv" = write_csv(object, file),
    "tsv" = write_tsv(object, file),
    "rds" = saveRDS(object, file)
  )
}

is_dataset_used <- function(name, storage) {
  if (name %in% names(storage())) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_valid_format <- function(ext, formats = c("csv", "tsv", "rds")) {
  if (ext %in% formats) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @title
#' Checks filename and its extension is valid.
validate_dataset <- function(
    dataset_name,
    dataset_ext,
    dataset_path,
    storage) {
  if (is_dataset_used(dataset_name, storage)) {
    showModal(modal_dataset_used(dataset_name))
    return(FALSE)
  }
  if (!is_valid_format(dataset_ext)) {
    showModal(modal_invalid_format(dataset_ext))
    return(FALSE)
  }
  return(TRUE)
}

modal_dataset_used <- function(dataset_name) {
  modalDialog(
    title = "Dataset already in use",
    str_glue(
      "'{dataset_name}' is already in use. Dataset names must be unique."
    ),
    easyClose = TRUE
  )
}

modal_invalid_format <- function(dataset_ext) {
  modalDialog(
    title = "Invalid format",
    str_glue(
      "Invalid format. '{dataset_ext}' not supported."
    ),
    easyClose = TRUE
  )
}

modal_invalid_object <- function(dataset_name) {
  modalDialog(
    title = "Invalid object",
    str_glue(
      "Invalid object. '{dataset_name}' object should have an appropiate type."
    ),
    easyClose = TRUE
  )
}

modal_error_reading <- function() {
  modalDialog(
    title = "Error",
    str_glue(
      "File couldn't be read. Please check file is correct."
    ),
    easyClose = TRUE
  )
}

create_button_observers <- function(
    data_name,
    data_ext,
    data_storage,
    input,
    output) {
  output[[str_glue("{data_name}_download")]] <- downloadHandler(
    filename = str_glue("{data_name}.{data_ext}"),
    content = function(file) {
      write_data(
        object = data_storage()[[data_name]],
        file = file,
        extension = data_ext
      )
    }
  )
  observeEvent(input[[stringr::str_c(data_name, "_delete")]],
    {
      delete_reactive_list(data_storage, data_name)
      showNotification(
        str_glue("'{data_name}' deleted"),
        duration = 2,
        type = "error"
      )
    },
    ignoreInit = TRUE,
    once = TRUE
  )
}

load_tibble <- function(
    read_data,
    data_name,
    data_ext,
    storage,
    storage_ext,
    input,
    output) {
  extend_reactive_list(storage, list2("{data_name}" := read_data))
  extend_reactive_list(storage_ext, list2("{data_name}" := data_ext))
  create_button_observers(data_name, data_ext, storage, input, output)
  showNotification(
    str_glue("'{dataset_name}' uploaded"),
    duration = 2,
    type = "message"
  )
}

load_tibble_list <- function(
    read_data,
    data_name,
    data_ext,
    storage,
    storage_ext,
    input,
    output) {
  for (element_name in names(read_data)) {
    element <- read_data[[element_name]]
    full_name <- str_glue("{data_name}_{element_name}")
    used_cond <- !is_dataset_used(full_name, storage)
    if (used_cond) {
      extend_reactive_list(storage, list2("{full_name}" := element))
      extend_reactive_list(storage_ext, list2("{full_name}" := data_ext))
      create_button_observers(full_name, data_ext, storage, input, output)
      showNotification(
        str_glue("'{full_name}' uploaded"),
        duration = 2,
        type = "message"
      )
    } else {
      showNotification(
        str_glue("Couldn't upload '{full_name}'. Name already in use."),
        duration = 2,
        type = "warning"
      )
    }
  }
}

load_data <- function(
    read_data,
    data_name,
    data_ext,
    storage,
    storage_ext,
    input,
    output) {
  data_class <- class(read_data)
  if ("list" %in% data_class) {
    load_tibble_list(
      read_data,
      data_name,
      data_ext,
      storage,
      storage_ext,
      input,
      output
    )
  } else if ("tibble" %in% data_class) {
    load_tibble(
      read_data,
      data_name,
      data_ext,
      storage,
      storage_ext,
      input,
      output
    )
  }
}

download_button_dep <- function() {
  htmltools::htmlDependency(
    name = "download-button-dep",
    version = "1.0",
    src = c(file = "./www/css/"),
    stylesheet = "download_button.css"
  )
}

delete_button_dep <- function() {
  htmltools::htmlDependency(
    name = "delete-button-dep",
    version = "1.0",
    src = c(file = "./www/css/"),
    stylesheet = "delete_button.css"
  )
}
