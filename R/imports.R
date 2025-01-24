#' @import shiny
#' @importFrom tibble tibble as_tibble rowid_to_column column_to_rownames
#' rownames_to_column
#' @importFrom shinydashboard infoBoxOutput renderInfoBox box
#' @importFrom shinyWidgets virtualSelectInput
#' @importFrom shinyFeedback useShinyFeedback feedbackWarning
#' @importFrom ggplot2 theme element_blank
#' @importFrom reactable reactable reactableOutput renderReactable colDef
#' colFormat
#' @importFrom rlang exprs expr sym `:=` new_formula enquo quo_is_null
#' @importFrom magrittr `%>%`
#' @importFrom dplyr mutate case_when case_match count distinct pull bind_rows
#' bind_cols slice_max
#' @importFrom tidyr replace_na pivot_wider pivot_longer
#' @importFrom stringr str_flatten str_to_lower str_c
#' @importFrom purrr map_lgl
#' @importFrom htmltools tagList tagAppendChild tagAppendAttributes
#' htmlDependency tagQuery
#' @importFrom forcats fct_recode fct_collapse
#' @importFrom plotly plotlyOutput renderPlotly ggplotly layout config plot_ly
#' @importFrom ROCnGO add_roc_curve summarize_predictor
#' @importFrom readr read_csv read_tsv write_csv write_tsv
#' @importFrom SummarizedExperiment SummarizedExperiment assays assay colData
#' rowData
NULL