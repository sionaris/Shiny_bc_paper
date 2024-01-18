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
  isolate({
    results = plot_breast_cancer_ROC()
    
    # Predefined colors
    predefined_colors = c("#2A5674", "#E34F6F", "#6C2167")
    
    # Initialize an empty plot
    plot(1, type="n", xlim=c(1,0), ylim=c(0,1), xlab="False Positive Rate", ylab="True Positive Rate", main="ROC curve")
    
    # Store legends
    legends = vector("character", length(results))
    
    # Iterate over results and add each ROC curve to the plot
    for(i in 1:length(results)) {
      res = results[[i]]
      if(!is.null(res$plot)) {
        # Store legend
        legends[i] = paste0(res$Legends, ", AUC=", res$auc_values, ", n=", res$nsamples)
        
        # Add ROC curve to the plot using predefined color
        lines(res$plot, col=predefined_colors[i], lwd=3)
      }
    }
    
    # Add legend
    legend(0.5, 0.25, legend=legends, col=predefined_colors[1:length(results)], lty=1, cex=0.8)
  })
})


# Print error metrics
output$breast_cancer_error_table <- DT::renderDataTable({
  w3$show()
  input$predict_ml_breast_cancer
  isolate({
    plot_breast_cancer_ROC()$error_metrics})
})

# Pop-up info message, triggered when the user presses the Info button
output$ml_breast_info_text <- renderText({
  paste0("<br> &#8226 Support Vector Machines <b>do not produce ROC plots</b>, only error metrics.",
         "<br> &#8226 The final model we chose in our project is the <b>C5.0-ROC-optimised decision tree</b>.",
         "<br> &#8226 You can pick the model category and then subcategory you are interested in and then the study subset of interest.", 
         "<br> &#8226 You can also filter by multiple variables, for more specific model predictions. Excessive filtering may result in a NULL set.",
         "<br> &#8226 You can plot up to three ROC curves at the same plot, <b>but you have to click 'Apply' after you have specified your model and data subset and also provide an appropriate legend for each selection of yours</b>.",
         "<br> &#8226 After pressing 'Predict' and viewing your output, if you want to do multiple models again, you'll have to <b>press Apply</b> in <b>all</b> additional models before plotting.")})