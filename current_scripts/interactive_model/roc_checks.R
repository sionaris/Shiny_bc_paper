# ROC plot checks ###
# Hide ROCplot option if not dataset
roc_possibility = reactiveVal()
observeEvent(import_data_type(), {
  if (import_data_type() == "Import pre-annotated dataset") {
    shinyjs::enable("breast_cancer_new_prediction_roc")
    roc_possibility(TRUE)
  } else {
    shinyjs::disable("breast_cancer_new_prediction_roc")
    roc_possibility(FALSE)
  }
})

roc_handle = reactive({ input$breast_cancer_new_prediction_roc })
test_passed = reactiveVal()
# Check there is a response column with values "Responder", "Non_responder" in the input
observeEvent(roc_handle(), {
  if (roc_handle() == "Yes" && roc_possibility())  {
    # Check if there is a response column
    if (!"Response" %in% colnames(imported_data())) {
      test_passed(FALSE)
      showModal(modalDialog(
        title = "Warning",
        "The uploaded file should contain a column named 'Response' with levels
          'Responder', 'Non_responder'.",
        easyClose = TRUE
      ))
    }
    
    # If that succeeded, check if it has the desired levels
    if (all(unique(imported_data()[, "Response"]) %in% c("Non_responder", "Responder")) &&
        length(unique(imported_data()[, "Response"])) <= 2) {
      test_passed(TRUE)
    } else {
      test_passed(FALSE)
      showModal(modalDialog(
        title = "Warning",
        "The uploaded file should contain a column named 'Response' with levels
          'Responder', 'Non_responder'.",
        easyClose = TRUE
      ))
    }
  }
})