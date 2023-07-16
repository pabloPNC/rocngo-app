library(shinyFeedback)
library(tidyverse)
library(pROC)
library(ROCpAI)

source("./modules/plotsPage/plotsPageUi.R")
source("./R/fpauc_utils.R")
source("./R/tpauc_utils.R")


# values ------------------------------------------------------------------

stored_plots <- reactiveValues(
  sensitivity = list(),
  specificity = list()
)


# sensitivity -------------------------------------------------------------

observeEvent(input$save_plot_sensitivity, {
  gold_column <- input$gold_config_sensitivity
  column <- input$scores_config_sensitivity
  dataset <- select(dataset(), column, gold_column)
  plot_name <- input$plot_name_sensitivity
  plot_exists <- !is.null(stored_plots[["sensitivity"]][[plot_name]])
  threshold <- input$threshold_sensitivity
  
  
  if (!plot_exists) {
    hideFeedback("plot_name_sensitivity")
    stored_plots[["sensitivity"]][[plot_name]] <- list(
      dataset = dataset,
      roc_points = ROCpoints(dataset[[gold_column]], dataset[[column]]),
      threshold = threshold
    )
    updateTextInput(session, "plot_name_sensitivity", value = "")
    print(stored_plots[["sensitivity"]])
  } else {
    showFeedbackWarning(
      inputId = "plot_name_sensitivity",
      text = "Plot name already in use"
    )
  }
    
  
})

observeEvent(input$gold_config_sensitivity, {
  dataset <- dataset()
  column <- input$gold_config_sensitivity
  condition <- CheckGoldStandard(dataset[[column]]) || input$gold_config_sensitivity == "-"
  
  if (!condition) {
    showFeedbackWarning(
      inputId = "gold_config_sensitivity",
      text = "Gold Standard must be binary"
    )
  } else {
    hideFeedback("gold_config_sensitivity")
  }
})

observeEvent(input$scores_config_sensitivity, {
  dataset <- dataset()
  column <- input$scores_config_sensitivity
  condition <- is.numeric(dataset[[column]]) || input$scores_config_sensitivity == "-"
  
  if (!condition) {
    showFeedbackWarning(
      inputId = "scores_config_sensitivity",
      text = "Score must be numeric"
    )
  } else {
    hideFeedback("scores_config_sensitivity")
  }
})


output$sensitivity_plot_space <- renderUI({
  dataset <- dataset()
  gold_column <- input$gold_config_sensitivity
  column <- input$scores_config_sensitivity
  
  condition1 <- CheckGoldStandard(dataset[[gold_column]]) || input$gold_config_sensitivity == "-"
  condition2 <- is.numeric(dataset[[column]])
  
  if (condition1 && condition2) {
    list(sensitivity_plot_content, sensitivity_save_plot_menu)
  } else {
    plot_cover
  }
    
})

output$sensitivity_plot <- renderPlot({
  dataset <- dataset()
  gold_column <- input$gold_config_sensitivity
  scores_column <- input$scores_config_sensitivity
  threshold <- input$threshold_sensitivity / 100
  roc_points <- as.data.frame(ROCpoints(dataset[[gold_column]], dataset[[scores_column]]))
  hs_roc_points <- as.data.frame(pHSpoints(dataset[[gold_column]], dataset[[scores_column]], threshold))
  
  ggplot() +
    geom_point(data = roc_points, mapping = aes(x = fpr, y = tpr)) +
    geom_point(data = hs_roc_points, mapping = aes(x = fprp, y = tprp), color = "red")
  
})


output$sensitivity_scores <- renderUI({
  dataset <- dataset()
  gold_column <- input$gold_config_sensitivity
  column <- input$scores_config_sensitivity
  
  condition1 <- CheckGoldStandard(dataset[[gold_column]]) || input$gold_config_sensitivity == "-"
  condition2 <- is.numeric(dataset[[column]])
  
  if (condition1 && condition2) {
    auc_result <- round(as.numeric(auc(dataset[[gold_column]], dataset[[column]])),3)
    pauc_result <- round(
      as.numeric(
        auc(dataset[[gold_column]],
            dataset[[column]],
            partial.auc = c(input$threshold_sensitivity/100, 1),
            partial.auc.focus = "sensitivity"
            ) 
        ),
      3)
    fpauc_result <- round(
      as.numeric(
        FpaucHS(dataset[[gold_column]],
            dataset[[column]],
            input$threshold_sensitivity/100
            ) 
        ),
      3)
    box(
      title = "Metrics",
      width = 12,
      tags$p(
        HTML(paste(tags$b("AUC:"), auc_result, sep = " "))
      ),
      tags$p(
        HTML(paste(tags$b("pAUC:"), pauc_result, sep = " "))
      ), 
      tags$p(
        HTML(paste(tags$b("FpAUC:"), fpauc_result, sep = " "))
      )
    )
  } 
})


observeEvent(dataset(), {
  updateSelectInput(session, "gold_config_sensitivity", choices = c(colnames(dataset()), "-"), selected = "-")
})

observeEvent(dataset(), {
  updateSelectInput(session, "scores_config_sensitivity", choices = c(colnames(dataset()), "-"), selected = "-")
})





# specificity -------------------------------------------------------------

observeEvent(dataset(), {
  updateSelectInput(session, "gold_config_specificity", choices = c(colnames(dataset()), "-"), selected = "-")
})

observeEvent(dataset(), {
  updateSelectInput(session, "scores_config_specificity", choices = c(colnames(dataset()), "-"), selected = "-")
})


observeEvent(input$gold_config_specificity, {
  dataset <- dataset()
  column <- input$gold_config_specificity
  condition <- CheckGoldStandard(dataset[[column]]) || input$gold_config_specificity == "-"
  
  if (!condition) {
    showFeedbackWarning(
      inputId = "gold_config_specificity",
      text = "Gold Standard must be binary"
    )
  } else {
    hideFeedback("gold_config_specificity")
  }
})


observeEvent(input$scores_config_specificity, {
  dataset <- dataset()
  column <- input$scores_config_specificity
  condition <- is.numeric(dataset[[column]]) || input$scores_config_specificity == "-"
  
  if (!condition) {
    showFeedbackWarning(
      inputId = "scores_config_specificity",
      text = "Score must be numeric"
    )
  } else {
    hideFeedback("scores_config_specificity")
  }
})


output$specificity_plot_space <- renderUI({
  dataset <- dataset()
  gold_column <- input$gold_config_specificity
  column <- input$scores_config_specificity
  
  condition1 <- CheckGoldStandard(dataset[[gold_column]]) || input$gold_config_specificity == "-"
  condition2 <- is.numeric(dataset[[column]])
  
  if (condition1 && condition2) {
    list(specificity_plot_content, specificity_save_plot_menu)
  } else {
    plot_cover
  }
})


output$specificity_plot <- renderPlot({
  dataset <- dataset()
  gold_column <- input$gold_config_specificity
  scores_column <- input$scores_config_specificity
  threshold <- input$threshold_specificity / 100
  roc_points <- as.data.frame(ROCpoints(dataset[[gold_column]], dataset[[scores_column]]))
  # x = gold_standar, y = scores
  hsp_roc_points <- as.data.frame(pHSPEpoints(roc_points, threshold))
  ggplot() +
    geom_point(data = roc_points, mapping = aes(x = fpr, y = tpr)) +
    geom_point(data = hsp_roc_points, mapping = aes(x = fpr, y = tpr), color = "red")
})


# TODO:
#   + Cambiar esto para que salgan las métricas

output$specificity_scores <- renderUI({
  dataset <- dataset()
  gold_column <- input$gold_config_specificity
  column <- input$scores_config_specificity
  
  condition1 <- CheckGoldStandard(dataset[[gold_column]]) || input$gold_config_specificity == "-"
  condition2 <- is.numeric(dataset[[column]])
  
  if (condition1 && condition2) {
    auc_result <- round(as.numeric(auc(dataset[[gold_column]], dataset[[column]])),3)
    pauc_result <- round(
      as.numeric(
        auc(dataset[[gold_column]],
            dataset[[column]],
            partial.auc = c(input$threshold_specificity/100, 1),
            partial.auc.focus = "specificity"
            ) 
        ),
      3)
    filtered_dataset <- select(dataset, gold_column, column)
    
    tpauc.ext <- tpAUC(
      dataset = filtered_dataset,
      plot = F,
      low.value = input$threshold_specificity/100,
      up.value = 1
    )
    tpauc.assays <- assays(tpauc.ext)
    Tpauc_result <- round(tpauc.assays[[1]]$St_pAUC[[1]],3)
    box(
      title = "Metrics",
      width = 12,
      tags$p(
        HTML(paste(tags$b("AUC:"), auc_result, sep = " "))
      ),
      tags$p(
        HTML(paste(tags$b("pAUC:"), pauc_result, sep = " "))
      ), 
      tags$p(
        HTML(paste(tags$b("TpAUC:"), Tpauc_result, sep = " "))
      )
    )
  } 
})

observeEvent(input$save_plot_specificity, {
  gold_column <- input$gold_config_specificity
  column <- input$scores_config_specificity
  dataset <- select(dataset(), column, gold_column)
  plot_name <- input$plot_name_specificity
  plot_exists <- !is.null(stored_plots[["specificity"]][[plot_name]])
  threshold <- input$threshold_specificity
  
  
  if (!plot_exists) {
    hideFeedback("plot_name_specificity")
    stored_plots[["specificity"]][[plot_name]] <- list(
      dataset = dataset,
      roc_points = ROCpoints(dataset[[gold_column]], dataset[[column]]),
      threshold = threshold
    )
    updateTextInput(session, "plot_name_specificity", value = "")
  } else {
    showFeedbackWarning(
      inputId = "plot_name_specificity",
      text = "Plot name already in use"
    )
  }
})