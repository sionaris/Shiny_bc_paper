# A function for dynamic conditional selectInput for the ML model subcategory

update_subcategory_radios <- function(model_num) {
  observeEvent(input[[paste0("breast_cancer_ml_model_category_", model_num)]], {
    category_input <- input[[paste0("breast_cancer_ml_model_category_", model_num)]]
    subcategory_id <- paste0("breast_cancer_ml_model_subcategory_", model_num)
    
    if(category_input == "Logistic Regression"){
      updateRadioButtons(inputId = subcategory_id,
                         choices = c("Backward", `Regularised` = "Lasso-regularised"),
                         selected = "Lasso-regularised")
    } else if(category_input == "Decision Trees"){
      updateRadioButtons(inputId = subcategory_id,
                         choices = sort(c(`C5.0 (tuned with Cohen's k)` = "C5.0 - k",
                                          `Bagging (100 iterations)` = "100X Bagging",
                                          `AdaBoost` = "Boosting",
                                          `Random Forest (tuned with Cohen's k)` = "RForest - k",
                                          `Random Forest (tuned with ROC)` = "RForest - ROC",
                                          `C5.0 (tuned with ROC)` = "C5.0 - ROC")),
                         selected = "C5.0 - ROC")
    } else if(category_input == "Support Vector Machines"){
      updateRadioButtons(inputId = subcategory_id,
                         choices = sort(c(`Linear kernel` = "Linear",
                                          `L2-regularised linear kernel` = "L2 Linear",
                                          `Radial Basis Function (RBF) kernel` = "RBF")),
                         selected = "RBF")
    }
  })
}

# Call the function for each model
update_subcategory_radios(1)
update_subcategory_radios(2)
update_subcategory_radios(3)

# Plot counter
counter = reactiveValues(counter = 1)

# Hide second model selection box
observeEvent(input$multiple_rocs_breast, {
  if(input$multiple_rocs_breast %% 2 == 1){
    shinyjs::show("model_select_breast_2")
  }
})

# If the user clicks apply then set the counter to 2
observeEvent(input$apply_comparison_breast_2, {
  counter$counter = 2
})

# Hide third model and variable selection box
observeEvent(input$multiple_rocs_breast_2, {
  if(input$multiple_rocs_breast_2 %% 2 == 1){
    shinyjs::show("model_select_breast_3")
    shinyjs::disable("multiple_rocs_breast_2_remove")
  }
})

# Additional button to remove the second model
observeEvent(input$multiple_rocs_breast_2_remove, {
  if(input$multiple_rocs_breast_2_remove %% 2 == 1){
    shinyjs::hide("model_select_breast_2")
  }
})

# Additional button to remove the third model
observeEvent(input$multiple_rocs_breast_3, {
  if(input$multiple_rocs_breast_3 %% 2 == 1){
    shinyjs::hide("model_select_breast_3")
    shinyjs::enable("multiple_rocs_breast_2_remove")
  }
})

# If the user clicks apply then set the counter to 2
observeEvent(input$apply_comparison_breast_3, {
  counter$counter = 3
})

observeEvent(input$breast_cancer_ROC_plot, {
  counter$counter = 1
})

err_metrics = function(CM) {
  TN =CM[1,1]
  TP =CM[2,2]
  FP =CM[1,2]
  FN =CM[2,1]
  precision =(TP)/(TP+FP)
  recall_score =(FP)/(FP+TN)
  f1_score=2*((precision*recall_score)/(precision+recall_score))
  accuracy_model  =(TP+TN)/(TP+TN+FP+FN)
  False_positive_rate =(FP)/(FP+TN)
  False_negative_rate =(FN)/(FN+TP)
  err_df = as.data.frame(cbind(c("Accuracy", "Precision", "Recall", "FPR", "FNR", "F1"),
                               c(accuracy_model, precision, recall_score, False_positive_rate,
                                 False_negative_rate, f1_score)))
  return(err_df)
}