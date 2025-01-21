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
