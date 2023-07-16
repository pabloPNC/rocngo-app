library(dplyr)
library(tools)
library(readr)
library(waiter)

dataPageServer <- {
    
    data_path <- "./data/"
    
    selected_file_path <- reactive({
      paste0(data_path, input$database)
    }) 
  
    data_extension <- reactive({
    file_ext(input$database)
  })
  

    
  dataset <- reactive({
    if (data_extension() == "csv") {
      csv <- read_csv(selected_file_path())
      csv
    } else if (data_extension() == "rda") {
      load(paste0("./data/", input$database))
      get(file_path_sans_ext(input$database))

    } else if (data_extension() == "rds") {
      rds <- readRDS(selected_file_path())
    }
  })
  
  
  # dataset <- eventReactive(input$database, {
  #   waiter <- Waiter$new()
  #   waiter$show()
  #   on.exit(waiter$hide())
  #   
  #   
  #   if (data_extension() == "csv") {
  #     csv <- read_csv(selected_file_path())
  #     csv
  #   } else if (data_extension() == "rda") {
  #     load(paste0("./data/", input$database))
  #     get(file_path_sans_ext(input$database))  
  #     
  #   } else if (data_extension() == "rds") {
  #     rds <- readRDS(selected_file_path())
  #   }
  # })
  
  
  
  
  observeEvent(input$previewOption, 
                updateTabsetPanel(session, 
                                  "preview_type",
                                  paste0("manage_", input$previewOption)
                                  )
                )

 
  output$preview_output <- renderTable({
    # dataset()
    if (nrow(dataset()) > 10) {
      dataset()[1:10,]
    }
    else {
      dataset()
    }
    })
  
  output$str_output <- renderPrint({
    str(dataset())
    })
  
  output$summary_output <- renderPrint({
    summary(dataset())
    })
  
  
  filtered_dataset <- reactive({
    if (length(input$select_variables) > 0) {
      select(dataset(), input$select_variables)
    }
    else {
      dataset()
    }
  })
  
  output$view_output <- renderDataTable(
    filtered_dataset(),
    options = list(
      pageLength = 5,
      lengthMenu = list(c(5, 10, 20, 50, 100, -1), c("5", "10", "20", "50", "100", "All"))
      )
  )
  
  choices <- reactive({
    colnames(dataset())
  })
  
  observeEvent(choices(),{
    updateSelectInput(session, inputId = "select_variables", choices = choices())
  })
  
  
    
}