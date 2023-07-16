mainServer <- function(input, output, session) {

  # Selects the contents in the menu
  observeEvent(input$tabs, updateTabsetPanel(session, input = "switcher", paste0(input$tabs,"Panel")))

  # Reset to empty when out of Data
  data_tabs <- c("manage", "view", "explore", "visualize","emptyPanel",
                 "managePanel", "viewPanel", "explorePanel", "visualizePanel")
  
  observeEvent(input$tabs, {
    if (!(input$tabs %in% data_tabs)) {
      updateTabsetPanel(session,
                        input = "switcher",
                        selected = "emptyPanel")
    }
  })
  
  source("./modules/dataPage/dataPageServer.R", local = T)
  source("./modules/plotsPage/plotsPageServer.R", local = T)
  source("./modules/plotsPage/compareServer.R", local = T)
}
