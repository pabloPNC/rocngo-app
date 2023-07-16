library(shinyWidgets) 
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(waiter)

# source("./modules/plotsPage.R")
# source("./modules/reportPage.R")
# source("./modules/aboutPage.R")

source("./modules/dataPage/dataPageUi.R")
source("./modules/plotsPage/plotsPageUi.R")
source("./modules/plotsPage/compareUi.R")


body <- dashboardBody(
  tabItems(
    # tabItem(tabName = "data", h1("Data content here")), # Esta sobre porque Manage ya no es una vista
    tabItem(tabName = "manage", manageContents),
    tabItem(tabName = "view", viewContents),
    tabItem(tabName = "explore", h1("Data-explore content here")),
    tabItem(tabName = "visualize", h1("Data-visualize content here")),
    #tabItem(tabName = "plots", plotsContents),
    tabItem(tabName = "sensitivity", sensitivityContents),
    tabItem(tabName = "specificity", specificityContents),
    tabItem(tabName = "compare", compareContents),
    tabItem(tabName = "report", h1("Report contents here")),
    tabItem(tabName = "about", h1("About contents here"))
  )
)

generalPanelContent <- list(
  selectInput("database", label = "Dataset Selection", 
              # choices = tools::file_path_sans_ext(list.files("./data/")))
              choices = list.files("./data/"))
)

mainUi <- dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#3c8dbc"),
  header = dashboardHeader(title = "FpAUC"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      generalPanelContent, # Menu con contenidos que no van a cambiar
      tabsetPanel(
        id = "switcher",
        type = "hidden",
        selected = "emptyPanel",
        # Ya nos coge el observer, ahora tenemos que editar el contenido de los paneles
        # Cambiar los paneles según el tab selccionado
        tabPanelBody("emptyPanel", ""),
        tabPanelBody("managePanel", managePanelContents),
        tabPanelBody("viewPanel", viewPanelContents),
        tabPanelBody("explorePanel", "Explore Panel Content"),
        tabPanelBody("visualizePanel", "Vis. Panel Content"),
      ),
      menuItem("Data", tabName = "data", icon = icon("table"),
               menuSubItem("Manage", tabName = "manage"),
               menuSubItem("View", tabName = "view"),
               menuSubItem("Explore", tabName = "explore"),
               menuSubItem("Visualize", tabName = "visualize")
               ),
      menuItem("Plots", tabName = "plots", icon = icon("chart-line"),
               menuSubItem("Sensitivity", tabName = "sensitivity"),
               menuSubItem("Specificity", tabName = "specificity"),
               menuSubItem("Compare", tabName = "compare")
               ),
      menuItem("Report", tabName = "report", icon = icon("book")),
      menuItem("About", tabName = "about", icon = icon("info"))
    ),
    collapsed = F,
    minified = F
  ), 
  body = body,
  controlbar = dashboardControlbar(),
  title = "FpAUC"
)
