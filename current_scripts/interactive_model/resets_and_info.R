# Reset button
observeEvent(input$reset_new_prediction_breast_cancer, {
  # Delete data
  imported_data(NULL)
  
  # Reset elements
  shinyjs::reset("breast_cancer_new_prediction_file_input")
  shinyjs::reset("breast_cancer_new_prediction_type")
  shinyjs::reset("breast_cancer_new_prediction_treatment")
  shinyjs::reset("breast_cancer_new_prediction_pam50_annotation")
  shinyjs::reset("breast_cancer_new_prediction_scmod1_annotation")
  shinyjs::reset("breast_cancer_new_prediction_timepoint_annotation")
  shinyjs::reset("breast_cancer_new_prediction_ic10_annotation")
  shinyjs::reset("breast_cancer_new_prediction_mammaprint_annotation")
  shinyjs::reset("breast_cancer_new_prediction_rors_annotation")
  shinyjs::reset("breast_cancer_new_prediction_roc")
  shinyjs::reset("breast_cancer_new_prediction_timepoint_filter")
  shinyjs::reset("breast_cancer_new_prediction_pam50_filter")
  shinyjs::reset("breast_cancer_new_prediction_rorS_filter")
  shinyjs::reset("breast_cancer_new_prediction_Mammaprint_filter")
  shinyjs::reset("breast_cancer_new_prediction_ic10_filter")
  
  shinyjs::disable("breast_cancer_new_prediction_treatment")
})

# Pop-up info message, triggered when the user presses the Info button
output$new_prediction_breast_info_text <- renderText({
  paste0("<br> &#8226 Choose to either generate a random sample, import a sample with gene expression
           measurements only, import a pre-annotated gene expression sample or import a fully annotated dataset.",
         "<br> &#8226 You can either prespecify treatment in your imported file or choose it here.",
         "<br> &#8226 Column names of the genes should be NCBI Entrez ID's 
           (either as they are or prefixed with 'X_' - both work well).",
         "<br> &#8226 You can also edit pam50, iC10, timepoint, Mammaprint and rorS risk values for unique samples.",
         "<br> &#8226 You can subset datasets using filters for pam50, iC10, timepoint, Mammaprint and rorS risk values.",
         "<br> &#8226 <b>Make sure to choose 'Yes' in the 'Produce ROC plot' buttons if you want a ROC plot.</b>",
         "<br> &#8226 ROC plots can be generated if a dataset is imported with a Response column with levels
           'Responder', 'Non_responder'.")
})