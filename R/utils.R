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
  select(.data, !c(..., {{ .sample_id }}))

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
  sample_id_exp <- enquo(.sample_id)
  if (quo_is_null(sample_id_exp)) {
    .data <- .data %>% rowid_to_column(var = ".sample_id")
    .sample_id <- ".sample_id"
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

#' @title
#' Extracts `colData` from a `SummarizedExperiment` to a `data.frame`.
rowdata_to_df <- function(se) {
  rowData(se) %>% data.frame() %>% rownames_to_column(var = ".gene")
}

#' @title
#' Extracts `rowData` from a s `SummarizedExperiment` to a `data.frame`.
coldata_to_df <- function(se) {
  colData(se) %>% data.frame() %>% rownames_to_column(var = ".sample_id")
}

#' @title
#' Includes information in `rowData` into assay `data.frame`.
include_rowdata <- function(assay_data, row_data) {
  assay_df <- assay_data %>%
    bind_cols(row_data) %>%
    pivot_longer(
      cols = !c(colnames(row_data)),
      names_to = ".assay_sample",
      values_to = ".gene_metric"
    )
  assay_df
}

#' @title
#' Includes information in `colData` into assay `data.frame`
include_coldata <- function(assay_data, col_data) {
  assay_df <- assay_data %>%
    pivot_wider(
      values_from = ".gene_metric",
      names_from = ".gene"
    ) %>%
    bind_cols(col_data) %>%
    select(!c(".assay_sample"))
  assay_df
}

#' @title
#' Transform a `SummarizedExperiment` into a list of `data.frames`.
#'
#' @description
#' `sumexp_to_df` returns a [list()] where each element is an assay
#' transformed to a `data.frame`.
sumexp_to_df <- function(se) {
  assay_dfs <- list()
  row_data <- rowdata_to_df(se)
  col_data <- coldata_to_df(se)
  exp_assays <- assays(se)
  for (i in 1:seq_along(exp_assays)) {
    assay <- exp_assays[[i]]
    assay_df <- data.frame(assay) %>%
      include_rowdata(row_data) %>%
      include_coldata(col_data) %>%
      as_tibble()
    assay_dfs[[i]] <- assay_df
  }
  assay_dfs
}