process_data <- function(category, subcategory, input_filters, legend_entry, ML, full_ml_set, color) {
  
  # Variables for inputs
  ml_study_subset = full_ml_set$Dataset %in% input_filters$dataset_checkbox
  
  # Subset the data that the model will be assessed on
  dataset = full_ml_set[ml_study_subset,] %>%
    dplyr::filter(Treatment %in% input_filters$treatment_filter) %>%
    dplyr::filter(Timepoint %in% input_filters$timepoint_filter) %>%
    dplyr::filter(Location %in% input_filters$location_filter) %>%
    dplyr::filter(pam50 %in% input_filters$pam50_filter) %>%
    dplyr::filter(rorS_risk %in% input_filters$rorS_filter) %>%
    dplyr::filter(Mammaprint_risk %in% input_filters$Mammaprint_filter) %>%
    dplyr::filter(Menopause.status %in% input_filters$Meno_filter) %>%
    dplyr::filter(ER.status %in% input_filters$ER_filter)
  
  # Number of samples in the filtered dataset
  nsamples = nrow(dataset)
  
  model = ML[[category]][[subcategory]]
  
  # Predict and create a confusion matrix
  # Make predictions according to model chosen
  if(subcategory == "100X Bagging"){
    predictions_frame = adabag::predict.bagging(model, dataset)
    predictions = predictions_frame[["class"]]
    confusion_matrix = table(predictions, dataset$Response)
    model_accuracy = 1 - (confusion_matrix[1] + confusion_matrix[4])/
      sum(confusion_matrix)
    predicted_probabilities = predictions_frame[["prob"]]
    errs = err_metrics(confusion_matrix)
    colnames(errs) = c("Metrics", legend_entry)
  } else if(subcategory == "Boosting") {
    predictions_frame = predict(model, dataset)
    predictions = predictions_frame[["class"]]
    confusion_matrix = predictions_frame[["confusion"]]
    model_accuracy = 1 - (confusion_matrix[1] + confusion_matrix[4])/
      sum(confusion_matrix)
    predicted_probabilities = predictions_frame[["prob"]]
    errs = err_metrics(confusion_matrix)
    colnames(errs) = c("Metrics", legend_entry)
  } else if(category == "Support Vector Machines"){
    predictions = predict(model, dataset)
    confusion_matrix = table(predictions, dataset$Response)
    model_accuracy = (confusion_matrix[1] + confusion_matrix[4])/
      sum(confusion_matrix)
    errs = err_metrics(confusion_matrix)
    colnames(errs) = c("Metrics", legend_entry)
  } else {
    predictions = predict(model, dataset)
    confusion_matrix = table(predictions, dataset$Response)
    model_accuracy = (confusion_matrix[1] + confusion_matrix[4])/
      sum(confusion_matrix)
    predicted_probabilities = predict(model, dataset, type = "prob")
    errs = err_metrics(confusion_matrix)
    colnames(errs) = c("Metrics", legend_entry)
  }
  
  # ROC plot generation
  if(category == "Support Vector Machines"){
    print("ROC plots not available for Support Vector Machines")
    p = NULL
    auc_value = NULL
    model_roc = NULL
  } else {
    join = cbind(predictions, predicted_probabilities, dataset)
    colnames(join)[c(2,3)] = c("Responder", "Non_responder")
    join = join[order(join$Responder),]
    model_roc = roc(predictor = join$Responder, 
                    response = as.character(join$Response))
    auc_value = round(auc(model_roc), 3)
    p = plot(model_roc, 
             main = "ROC curve",
             col = color, lwd = 3, legacy.axes = TRUE, xlim = c(1,0), ylim = c(0,1), 
             asp = 1, cex = 4, xaxs = "i", yaxs = "i", width = 900, height = 900)
  }
  
  return(list(plot = p, conf = confusion_matrix, acc = model_accuracy, 
              Legends = legend_entry, auc_values = auc_value, nsamples = nsamples, 
              error_metrics = errs, model_roc = model_roc, 
              model_category = category, model_subcategory = subcategory))
}