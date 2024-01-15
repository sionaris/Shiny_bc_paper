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