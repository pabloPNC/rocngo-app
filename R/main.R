#' Main page UI
#' @importFrom shinydashboard sidebarMenu menuItem menuSubItem dashboardBody tabItem tabItems
#' @importFrom shinydashboardPlus dashboardPage dashboardHeader dashboardSidebar dashboardControlbar
#' @importFrom shiny tagList NS tabsetPanel tabPanelBody
#' @importFrom waiter spin_1
#' @importFrom shinyFeedback useShinyFeedback
main_ui <- function(id) {
  ns <- NS(id)
  dashboardPage(
    preloader = list(
      html = tagList(spin_1()),
      color = "#3c8dbc"
    ),
    header = dashboardHeader(title = "ROCnGO"),
    sidebar = dashboardSidebar(
      sidebarMenu(
        id = ns("tabs"),
        selectInput(
          ns("database"),
          label = "Dataset Selection",
          choices = NULL
        ),
        menuItem(
          "Data",
          tabName = "data",
          icon = shiny::icon("table"),
          menuSubItem("View", tabName = "view"),
          menuSubItem("Explore", tabName = "explore"),
          menuSubItem("Upload", tabName = "upload")
        ),
        menuItem(
          "Plot",
          tabName = "plot",
          icon = shiny::icon("chart-line"),
          menuSubItem("Sensitivity", tabName = "sensitivity"),
          menuSubItem("Specificity", tabName = "specificity"),
          menuSubItem("Compare", tabName = "compare")
        ),
        menuItem(
          "Report",
          tabName = "report",
          icon = shiny::icon("book")
        ),
        menuItem(
          "About",
          tabName = "about",
          icon = shiny::icon("info")
        )
      ),
      collapsed = FALSE,
      minified = FALSE
    ),
    body = dashboardBody(
      tabItems(
        tabItem(
          tabName = "view",
          view_ui(ns("view-page"), "View")
        ),
        tabItem(
          tabName = "explore",
          explore_ui(ns("explore-page"), "Explore")
        ),
        tabItem(
          tabName = "upload",
          upload_ui(ns("upload-page"), "Upload")
        ),
        tabItem(
          tabName = "sensitivity",
          metrics_ui(ns("sensitivity-page"), "Sensitivity")
        ),
        tabItem(
          tabName = "specificity",
          metrics_ui(ns("specificity-page"), "Specificity")
        ),
        tabItem(
          tabName = "compare",
          compare_ui(ns("compare-page"), "Compare")
        ),
        tabItem(tabName = "report", shiny::h1("Report contents here")),
        tabItem(tabName = "about", shiny::h1("About contents here"))
      ),
      controlbar = dashboardControlbar(),
      title = "ROCnGO"
    )
  )
}


#' Main page server
#' @importFrom shiny moduleServer reactiveValues
#' @importFrom dplyr select
main_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data_tabs <- c(
      "manage", "view", "emptyPanel", "managePanel",
      "viewPanel", "explorePanel", "visualizePanel"
    )
    data_storage <- list(
      default_datasets = reactiveVal(
        list(
          # TODO: repair colon to avoid duplicates
          # colon = colon,
          fast_colon = fast_colon,
          avengers = avengers,
          diamonds = diamonds,
          publishers = publishers,
          titanic = titanic
        )
      ),
      upload_datasets = reactiveVal(
        list(test = fast_colon)
      )
    )
    selected_dataset <- reactive({
      req(input$database)
      default_datasets <- names(data_storage[["default_datasets"]]())
      upload_datasets <- names(data_storage[["upload_datasets"]]())
      if (input$database %in% default_datasets) {
        data_storage[["default_datasets"]]()[[input$database]]
      } else if (input$database %in% upload_datasets) {
        data_storage[["upload_datasets"]]()[[input$database]]
      }
    })
    observe({
      updateSelectInput(
        session,
        inputId = "database",
        choices = names(
          c(
            data_storage[["default_datasets"]](),
            data_storage[["upload_datasets"]]()
          )
        )
      )
    })

    # Selects contents in the menu
    observeEvent(input$tabs, updateTabsetPanel(
      session,
      input = "switcher",
      paste0(input$tabs, "Panel")
    ))

    observeEvent(
      input$tabs,
      {
        if (!(input$tabs %in% data_tabs)) {
          updateTabsetPanel(
            session,
            input = "switcher",
            selected = "emptyPanel"
          )
        }
      }
    )

    output$str_output <- renderPrint({
      str(selected_dataset())
    })

    output$summary_output <- renderPrint({
      summary(selected_dataset())
    })

    observe({
      updateSelectInput(
        session,
        inputId = "select_variables",
        choices = colnames(selected_dataset())
      )
    })
    # New modules
    # TODO: fix data_storage form
    explore_server(
      id = "explore-page",
      selected_dataset = selected_dataset,
      data_storage = data_storage[["default_datasets"]]
    )
    view_server("view-page", selected_dataset)
    upload_server("upload-page", data_storage[["upload_datasets"]])
    metrics_server(
      id = "sensitivity-page",
      dataset = selected_dataset,
      functions = list(
        "NpAUC" = ROCnGO::np_auc,
        "FpAUC" = ROCnGO::fp_auc
      ),
      bound_func = list(
        "NpAUC" = ROCnGO::add_npauc_normalized_lower_bound,
        "FpAUC" = ROCnGO::add_fpauc_lower_bound
      ),
      ratio = "tpr"
    )
    metrics_server(
      id = "specificity-page",
      dataset = selected_dataset,
      functions = list(
        "SpAUC" = ROCnGO::sp_auc,
        "TpAUC" = ROCnGO::tp_auc
      ),
      bound_func = list(
        "SpAUC" = ROCnGO::add_spauc_lower_bound,
        "TpAUC" = ROCnGO::add_tpauc_lower_bound
      ),
      ratio = "fpr"
    )
    compare_server("compare-page", selected_dataset)
  })
}
