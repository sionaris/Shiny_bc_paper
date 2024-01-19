# Select all button function
observe_select_all_ml <- function(model_num) {
  observe({
    input_name <- paste0("select_all_ml_model", model_num, "_breast")
    checkbox_name <- paste0("ml_breast_dataset_checkbox_", model_num)
    
    if(input[[input_name]] > 0){
      if (input[[input_name]] %% 2 == 0) {
        updateCheckboxGroupInput(session, checkbox_name,
                                 choices = unique(full_ml_set$Dataset),
                                 selected = unique(full_ml_set$Dataset),
                                 inline = TRUE)
      }
      else {
        updateCheckboxGroupInput(session, checkbox_name,
                                 choices = unique(full_ml_set$Dataset),
                                 selected = c(),
                                 inline = TRUE)
      }
    }
  })
}

# Reset button function
observe_reset_ml <- function(model_num) {
  reset_input_name <- paste0("reset_input_breast_ml_model", model_num)
  
  observeEvent(input[[reset_input_name]], {
    filters <- c("treatment", "timepoint", "location", "pam50", "rorS", "Mammaprint", "Meno", "ER")
    for (filter in filters) {
      filter_name <- paste0("breast_cancer_ml_", filter, "_filter_", model_num)
      shinyjs::reset(filter_name)
    }
  })
}

# Button calls
observe_select_all_ml(1)
observe_select_all_ml(2)
observe_select_all_ml(3)
observe_reset_ml(1)
observe_reset_ml(2)
observe_reset_ml(3)

# Create a waiter
w3 <- Waiter$new(id = "breast_cancer_ROC_plot", ml_loading_screen_orbit, "#010D1A")

# Plot ROC curve
output$breast_cancer_ROC_plot <- renderPlot({
  w3$show()
  input$predict_ml_breast_cancer
  results <- plot_breast_cancer_ROC()
  
  # Create an empty data frame for all plot data
  all_plot_data <- data.frame(sensitivities = numeric(),
                              specificities = numeric(),
                              model = factor(),
                              stringsAsFactors = FALSE)
  
  # Create a data frame for legend labels
  legend_data <- data.frame(model = factor(), label = character())
  name_inputs = c(input$breast_cancer_ml_legend_entry_1,
                  input$breast_cancer_ml_legend_entry_2,
                  input$breast_cancer_ml_legend_entry_3)
  model_names = c()

  for (i in 1:length(results)) {
    res <- results[[i]]
    df <- as.data.frame(cbind(res$model_roc$sensitivities, res$model_roc$specificities))
    colnames(df) <- c("sensitivities", "specificities")
    model_names <- c(model_names, res$Legends)
    df$model <- as.factor(model_names[i])
    all_plot_data <- rbind(all_plot_data, df)
    
    # Prepare legend label
    auc_value <- res$auc_values
    nsamples <- res$nsamples
    legend_label <- paste(paste0(model_names[i], ":"), "AUC =", paste0(auc_value, ","), 
                          "N =", nsamples, sep = " ")
    legend_data <- rbind(legend_data, data.frame(model = model_names[i], label = legend_label))
  }
  
  legend_data$model = as.factor(legend_data$model)
  # Predefined colors
  predefined_colors <- c("#2A5674", "#E34F6F", "#6C2167")
  
  # Initialize ggplot
  p <- ggplot(all_plot_data, aes(x = 1 - specificities, y = sensitivities)) +
    geom_line(aes(color = model), size = 1) +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "black") +
    scale_color_manual(values = predefined_colors, labels = legend_data$label, breaks = levels(legend_data$model)) +
    labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)", title = "ROC curve") +
    scale_x_continuous(limits = c(0, 1.01), breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1.01), breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
    theme_classic() +
    theme(
      axis.title = element_text(face = "bold", size = 15),
      axis.title.x = element_text(face = "bold", size = 15, margin = margin(t = 10, unit = "pt")),
      axis.title.y = element_text(face = "bold", size = 15, margin = margin(r = 10, unit = "pt")),
      axis.text = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 20),
      axis.line = element_line(colour = "black"),
      legend.position = "bottom",
      legend.text = element_text(size = 12)
    )+
    #coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))+
    guides(color = guide_legend(nrow = length(results), byrow = TRUE))
  
  p
})

# Downloadable output
output$download_plot_roc <- downloadHandler(
  filename = function() {
    paste("ROC-plot", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    # Set up PNG device
    png(file, width = 600, height = 600)
    
    # Plotting code (same as in renderPlot)
    results <- plot_breast_cancer_ROC()
    
    # Create an empty data frame for all plot data
    all_plot_data <- data.frame(sensitivities = numeric(),
                                specificities = numeric(),
                                model = factor(),
                                stringsAsFactors = FALSE)
    
    # Create a data frame for legend labels
    legend_data <- data.frame(model = factor(), label = character())
    name_inputs = c(input$breast_cancer_ml_legend_entry_1,
                    input$breast_cancer_ml_legend_entry_2,
                    input$breast_cancer_ml_legend_entry_3)
    model_names = c()
    
    for (i in 1:length(results)) {
      res <- results[[i]]
      df <- as.data.frame(cbind(res$model_roc$sensitivities, res$model_roc$specificities))
      colnames(df) <- c("sensitivities", "specificities")
      model_names <- c(model_names, res$Legends)
      df$model <- as.factor(model_names[i])
      all_plot_data <- rbind(all_plot_data, df)
      
      # Prepare legend label
      auc_value <- res$auc_values
      nsamples <- res$nsamples
      legend_label <- paste(paste0(model_names[i], ","), "AUC =", paste0(auc_value, ","), 
                            "N =", nsamples, sep = " ")
      legend_data <- rbind(legend_data, data.frame(model = model_names[i], label = legend_label))
    }
    
    legend_data$model = as.factor(legend_data$model)
    # Predefined colors
    predefined_colors <- c("#2A5674", "#E34F6F", "#6C2167")
    
    # Initialize ggplot
    p <- ggplot(all_plot_data, aes(x = 1 - specificities, y = sensitivities)) +
      geom_line(aes(color = model), size = 1) +
      geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "black") +
      scale_color_manual(values = predefined_colors, labels = legend_data$label, breaks = levels(legend_data$model)) +
      labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)", title = "ROC curve") +
      scale_x_continuous(limits = c(0, 1.01), breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 1.01), breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
      theme_classic() +
      theme(
        axis.title = element_text(face = "bold", size = 15),
        axis.title.x = element_text(face = "bold", size = 15, margin = margin(t = 10, unit = "pt")),
        axis.title.y = element_text(face = "bold", size = 15, margin = margin(r = 10, unit = "pt")),
        axis.text = element_text(face = "bold", size = 10),
        plot.title = element_text(face = "bold", size = 20),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.text = element_text(size = 12)
      )+
      #coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))+
      guides(color = guide_legend(nrow = length(results), byrow = TRUE))
    
    p
    print(p)
    
    # Turn off the device
    dev.off()
  }
)

# Print error metrics
output$breast_cancer_error_table <- DT::renderDataTable({
  w3$show()
  input$predict_ml_breast_cancer
  isolate({
    outputs = plot_breast_cancer_ROC()
    if (length(outputs) == 1) {
      outputs[[1]]$error_metrics
    } else if (length(outputs) == 2) {
      comb = cbind(outputs[[1]]$error_metrics, outputs[[2]]$error_metrics[, 2])
      colnames(comb) = c("Metric", input$breast_cancer_ml_legend_entry_1, 
                         input$breast_cancer_ml_legend_entry_2)
      comb
    } else {
      comb = cbind(outputs[[1]]$error_metrics, outputs[[2]]$error_metrics[, 2],
            outputs[[3]]$error_metrics[, 2])
      colnames(comb) = c("Metric", input$breast_cancer_ml_legend_entry_1, 
                         input$breast_cancer_ml_legend_entry_2, 
                         input$breast_cancer_ml_legend_entry_3)
      comb
    }})
})

# Pop-up info message, triggered when the user presses the Info button
output$ml_breast_info_text <- renderText({
  paste0("<br> &#8226 Support Vector Machines <b>do not produce ROC plots</b>, only error metrics.",
         "<br> &#8226 The final model we chose in our project is the <b>C5.0-ROC-optimised decision tree</b>.",
         "<br> &#8226 You can pick the model category and then subcategory you are interested in and then the study subset of interest.", 
         "<br> &#8226 You can also filter by multiple variables, for more specific model predictions. Excessive filtering may result in a NULL set.",
         "<br> &#8226 You can plot up to three ROC curves at the same plot, <b>but you have to click 'Apply' after you have specified your model and data subset and also provide an appropriate legend for each selection of yours</b>.",
         "<br> &#8226 After pressing 'Predict' and viewing your output, if you want to do multiple models again, you'll have to <b>press Apply</b> in <b>all</b> additional models before plotting.")})