extend_reactive_list <- function(reactive_list, element) {
  reactive_list(append(reactive_list(), element))
}

delete_reactive_list <- function(reactive_list, name) {
  reactive_list(
    reactive_list() |> purrr::discard_at(name)
  )
}

add_text_input_class <- function(input, ...) {
  class_strings <- list(...)
  input$children[[2]] <- htmltools::tagAppendAttributes(
    input$children[[2]],
    class = str_flatten(class_strings, collapse = " ")
  )
}

test_ui <- function(contents) {
  dashboardPage(
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
      contents
    )
  )
}

transform_index_name <- function(index_name) {
  acronym_parts <- stringr::str_split(index_name, pattern = "(?<=^.{2})")[[1]]
  str_c(str_to_lower(acronym_parts[1]), "_", str_to_lower(acronym_parts[2]))
}

remove_box_header <- function(box) {
  query <- tagQuery(box)
  box <- query$find(".box-header")$remove()$allTags()
  box
}

#' @title
#' Extract sample data from a `data.frame`
#'
#' @description
#' `extract_coldata` returns a `data.frame` with variables describing samples.
extract_coldata <- function(.data, ..., .sample_id) {
  select(.data, !c(...), {{ .sample_id }})
}

#' @title
#' Extract matrix data from a `data.frame`
#'
#' @description
#' `extract_data` returns a `data.frame` with data for each feature and sample.
extract_data <- function(.data, ..., .sample_id) {
  se_data <- .data %>%
    select(c(...), {{ .sample_id }}) %>%
    pivot_longer(
      cols = c(...),
      names_to = "gene",
      values_to = "gene_data"
    ) %>%
    pivot_wider(
      names_from = {{ .sample_id }},
      values_from = "gene_data"
    ) %>%
    column_to_rownames(var = "gene")
  se_data
}

#' @title
#' Extract gene data from a `data.frame`
#'
#' @description
#' `extract_rowdata` returns a `data.frame` with variables describing features.
extract_rowdata <- function(.data, ...) {
  gene_names <- .data %>% select(c(...)) %>% colnames()
  row_data <- tibble(gene_name = gene_names) %>%
    column_to_rownames(var = "gene_name")
  row_data
}

#' @title
#' Create a `SummarizedExperiment` from `data.frame`.
#'
#' @description
#' `as_sum_exp` returns a `SummarizedExperiment` object containing specified
#' columns as features and the rest as sample metadata.
as_sum_exp <- function(.data, ..., .sample_id = NULL) {
  sample_exp <- enquo(.sample_id)
  if (quo_is_null(sample_exp)) {
    .data <- .data %>% rowid_to_column(var = "sample_id")
    .sample_id <- "sample_id"
  }
  col_data <- extract_coldata(.data, ..., .sample_id = {{ .sample_id }})
  se_data <- extract_data(.data,  ..., .sample_id = {{ .sample_id }})
  row_data <- extract_rowdata(.data, ...)
  se <- SummarizedExperiment(
    assays = list(assay_1 = se_data),
    rowData = row_data,
    colData = col_data
  )
  se
}

# TEST ------------------------------
sumexp_to_df <- function(se) {
  data <- assay(se)
  gene_data <- rowData(se)
  sample_data <- colData(se)
  # Include rownames in columns
  gene_data <- data.frame(gene_data) %>% rownames_to_column(var = "gene")
  sample_data <- data.frame(sample_data) %>% rownames_to_column(var = "sample")

  print(
    data.frame(data) %>%
      # Add gene name to rows
      bind_cols(gene_data) %>%
      # Merge samples name and values in 1 column
      pivot_longer(
        cols = !c(colnames(gene_data)),
        names_to = "sample",
        values_to = "gene_exp"
      ) %>%
      # Create variables for genes
      pivot_wider(
        values_from = "gene_exp",
        names_from = "gene"
      ) %>% bind_cols(
        sample_data
      )
  )
  # data <- data.frame(data) %>%
  #   bind_cols()

  # print(as_tibble(data))
  # print(
  #   bind_cols(
  #     as_tibble(gene_data),
  #     as_tibble(data)
  #   )
  # )
}
# sumexp_to_df(t)