# ROC curve function
plot_breast_cancer_ROC <- reactive({
  results <- list()
  
  if(counter$counter == 1){
    input_filters_1 = list(
      dataset_checkbox = input$ml_breast_dataset_checkbox,
      treatment_filter = input$breast_cancer_ml_treatment_filter,
      timepoint_filter = input$breast_cancer_ml_timepoint_filter,
      location_filter = input$breast_cancer_ml_location_filter,
      pam50_filter = input$breast_cancer_ml_pam50_filter,
      rorS_filter = input$breast_cancer_ml_rorS_filter,
      Mammaprint_filter = input$breast_cancer_ml_Mammaprint_filter,
      Meno_filter = input$breast_cancer_ml_Meno_filter,
      ER_filter = input$breast_cancer_ml_ER_filter
    )
    
    results[[1]] <- process_data(input$breast_cancer_ml_model_category, 
                                 input$breast_cancer_ml_model_subcategory, 
                                 input_filters_1, 
                                 input$breast_cancer_ml_legend_entry_1, 
                                 ML, 
                                 full_ml_set, 
                                 color = "#2A5674")
    
    if(input$breast_cancer_ml_model_category == "Support Vector Machines"){
      print("ROC plots not available for Support Vector Machines")
    }
    
  } else if(counter$counter == 2) {
    # Process the first dataset
    input_filters_1 = list(
      dataset_checkbox = input$ml_breast_dataset_checkbox,
      treatment_filter = input$breast_cancer_ml_treatment_filter,
      timepoint_filter = input$breast_cancer_ml_timepoint_filter,
      location_filter = input$breast_cancer_ml_location_filter,
      pam50_filter = input$breast_cancer_ml_pam50_filter,
      rorS_filter = input$breast_cancer_ml_rorS_filter,
      Mammaprint_filter = input$breast_cancer_ml_Mammaprint_filter,
      Meno_filter = input$breast_cancer_ml_Meno_filter,
      ER_filter = input$breast_cancer_ml_ER_filter
    )
    results[[1]] <- process_data(input$breast_cancer_ml_model_category, 
                                 input$breast_cancer_ml_model_subcategory, 
                                 input_filters_1, 
                                 input$breast_cancer_ml_legend_entry_1, 
                                 ML, 
                                 full_ml_set, 
                                 color = "#2A5674")
    
    # Process the second dataset
    input_filters_2 = list(
      dataset_checkbox = input$ml_breast_dataset_checkbox_2,
      treatment_filter = input$breast_cancer_ml_treatment_filter_2,
      timepoint_filter = input$breast_cancer_ml_timepoint_filter_2,
      location_filter = input$breast_cancer_ml_location_filter_2,
      pam50_filter = input$breast_cancer_ml_pam50_filter_2,
      rorS_filter = input$breast_cancer_ml_rorS_filter_2,
      Mammaprint_filter = input$breast_cancer_ml_Mammaprint_filter_2,
      Meno_filter = input$breast_cancer_ml_Meno_filter_2,
      ER_filter = input$breast_cancer_ml_ER_filter_2
    )
    results[[2]] <- process_data(input$breast_cancer_ml_model_category_2, 
                                 input$breast_cancer_ml_model_subcategory_2, 
                                 input_filters_2, 
                                 input$breast_cancer_ml_legend_entry_2, 
                                 ML, 
                                 full_ml_set, 
                                 color = "#E34F6F")
    
    if(input$breast_cancer_ml_model_category_2 == "Support Vector Machines"){
      print("ROC plots not available for Support Vector Machines")
    }
    
  } else if(counter$counter == 3) {
    # Process the first dataset
    input_filters_1 = list(
      dataset_checkbox = input$ml_breast_dataset_checkbox,
      treatment_filter = input$breast_cancer_ml_treatment_filter,
      timepoint_filter = input$breast_cancer_ml_timepoint_filter,
      location_filter = input$breast_cancer_ml_location_filter,
      pam50_filter = input$breast_cancer_ml_pam50_filter,
      rorS_filter = input$breast_cancer_ml_rorS_filter,
      Mammaprint_filter = input$breast_cancer_ml_Mammaprint_filter,
      Meno_filter = input$breast_cancer_ml_Meno_filter,
      ER_filter = input$breast_cancer_ml_ER_filter
    )
    results[[1]] <- process_data(input$breast_cancer_ml_model_category, 
                                 input$breast_cancer_ml_model_subcategory, 
                                 input_filters_1, 
                                 input$breast_cancer_ml_legend_entry_1, 
                                 ML, 
                                 full_ml_set,
                                 color = "#2A5674")
    
    # Process the second dataset
    input_filters_2 = list(
      dataset_checkbox = input$ml_breast_dataset_checkbox_2,
      treatment_filter = input$breast_cancer_ml_treatment_filter_2,
      timepoint_filter = input$breast_cancer_ml_timepoint_filter_2,
      location_filter = input$breast_cancer_ml_location_filter_2,
      pam50_filter = input$breast_cancer_ml_pam50_filter_2,
      rorS_filter = input$breast_cancer_ml_rorS_filter_2,
      Mammaprint_filter = input$breast_cancer_ml_Mammaprint_filter_2,
      Meno_filter = input$breast_cancer_ml_Meno_filter_2,
      ER_filter = input$breast_cancer_ml_ER_filter_2
    )
    results[[2]] <- process_data(input$breast_cancer_ml_model_category_2, 
                                 input$breast_cancer_ml_model_subcategory_2, 
                                 input_filters_2, 
                                 input$breast_cancer_ml_legend_entry_2, 
                                 ML, 
                                 full_ml_set,
                                 color = "#E34F6F")
    
    # Process the third dataset
    input_filters_3 = list(
      dataset_checkbox = input$ml_breast_dataset_checkbox_3,
      treatment_filter = input$breast_cancer_ml_treatment_filter_3,
      timepoint_filter = input$breast_cancer_ml_timepoint_filter_3,
      location_filter = input$breast_cancer_ml_location_filter_3,
      pam50_filter = input$breast_cancer_ml_pam50_filter_3,
      rorS_filter = input$breast_cancer_ml_rorS_filter_3,
      Mammaprint_filter = input$breast_cancer_ml_Mammaprint_filter_3,
      Meno_filter = input$breast_cancer_ml_Meno_filter_3,
      ER_filter = input$breast_cancer_ml_ER_filter_3
    )
    results[[3]] <- process_data(input$breast_cancer_ml_model_category_3, 
                                 input$breast_cancer_ml_model_subcategory_3, 
                                 input_filters_3, 
                                 input$breast_cancer_ml_legend_entry_3, 
                                 ML, 
                                 full_ml_set,
                                 color = "#6C2167")
    
    if(input$breast_cancer_ml_model_category_3 == "Support Vector Machines"){
      print("ROC plots not available for Support Vector Machines")
    }
  }
  
  return(results)
})