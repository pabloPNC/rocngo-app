# Body/Main contents of Data/Manage
manageContents <- list(
  h1("Data Manage"),
  tabsetPanel(
    id = "preview_type",
    type = "hidden",
    selected = "manage_preview", # Poner la que se selecciona por defecto
    
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
managePanelContents <- list(
  radioButtons("previewOption",
               "Display",
               
               choiceNames = c(
                 "Preview",
                 "str",
                 "summary"),
               
               choiceValues = c(
                 "preview", 
                 "str", 
                 "summary")
  )
)


viewContents <- list(
  h1("View Data"),
  dataTableOutput("view_output")
)

viewPanelContents <- list(
  selectInput("select_variables",
              label = "Select variables to show",
              choices = NULL,
              multiple = T
              )
)


exploreContents <- c()
explorePanelContents <- c()


visualizeContents <- c()
visualizePanelContents <- c()