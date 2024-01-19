# Filters  
filtered_data = reactive({
  reactive_filt_data <- NULL
  
  # Only run in the case of datasets
  if (import_data_type() == "Import pre-annotated dataset") {
    reactive_filt_data = imported_data()
    
    # Create a separate data object to apply filters dynamically
    temp_data <- reactive_filt_data
    
    # Filters
    newpred_timepoint_filter = input$breast_cancer_new_prediction_timepoint_filter
    newpred_pam50_filter = input$breast_cancer_new_prediction_pam50_filter
    newpred_rorS_filter = input$breast_cancer_new_prediction_rorS_filter
    newpred_mammaprint_filter = input$breast_cancer_new_prediction_Mammaprint_filter
    newpred_iC10_filter = input$breast_cancer_new_prediction_ic10_filter
    newpred_scmod1_filter = input$breast_cancer_new_prediction_scmod1_filter
    
    # Filter timepoint
    if (newpred_timepoint_filter == "All") {
      # No filtering applied
      temp_data = temp_data
    } else if (newpred_timepoint_filter == "Pre-treatment") {
      temp_data = temp_data %>% filter(T2 == 0)
    } else if (newpred_timepoint_filter == "On-treatment") {
      temp_data = temp_data %>% filter(T2 == 1)
    }
    
    # pam50
    if (newpred_pam50_filter == "All") {
      # No filtering applied
      temp_data = temp_data
    } else if (newpred_pam50_filter == "Luminal A") {
      temp_data = temp_data %>% dplyr::filter(LumA == 1)
    } else if (newpred_pam50_filter == "Luminal B") {
      temp_data = temp_data %>% dplyr::filter(LumB == 1)
    } else if (newpred_pam50_filter == "Normal-like") {
      temp_data = temp_data %>% dplyr::filter(Normal == 1)
    } else if (newpred_pam50_filter == "HER2+") {
      temp_data = temp_data %>% dplyr::filter(HER2 == 1)
    } else if (newpred_pam50_filter == "Basal-like") {
      temp_data = temp_data[temp_data$LumA==0 &
                              temp_data$LumB==0 &
                              temp_data$Normal==0 &
                              temp_data$HER2==0,]
    }
    
    # scmod1
    if (newpred_scmod1_filter == "All") {
      # No filtering applied
      temp_data = temp_data
    } else if (newpred_scmod1_filter == "HER2+") {
      temp_data = temp_data %>% dplyr::filter(HER2_scmod1 == 1)
    } else if (newpred_scmod1_filter == "ER+/HER2- high proliferation") {
      temp_data = temp_data %>% dplyr::filter(ER_hp == 1)
    } else if (newpred_scmod1_filter == "ER+/HER2- low proliferation") {
      temp_data = temp_data %>% dplyr::filter(ER_lp == 1)
    } else if (newpred_scmod1_filter == "ER-/HER2-") {
      temp_data = temp_data[temp_data$HER2_scmod1==0 &
                              temp_data$ER_hp==0 &
                              temp_data$ER_lp==0,]
    }
    
    # rorS
    if (newpred_rorS_filter == "All") {
      # No filtering applied
      temp_data = temp_data
    } else if (newpred_rorS_filter == "High") {
      temp_data = temp_data %>% dplyr::filter(rorS_risk_high == 1)
    } else if (newpred_rorS_filter == "Intermediate") {
      temp_data = temp_data %>% dplyr::filter(rorS_risk_interm == 1)
    } else if (newpred_rorS_filter == "Low") {
      temp_data = temp_data[temp_data$rorS_risk_high==0 &
                              temp_data$rorS_risk_interm==0,]
    }
    
    # Mammaprint
    if (newpred_mammaprint_filter == "All") {
      # No filtering applied
      temp_data = temp_data
    } else if (newpred_mammaprint_filter == "At risk") {
      temp_data = temp_data %>% dplyr::filter(Mammaprint_risk_yes == 1)
    } else if (newpred_mammaprint_filter == "No risk") {
      temp_data = temp_data %>% dplyr::filter(Mammaprint_risk_yes == 0)
    }
    
    # iC10
    if (newpred_iC10_filter == "All") {
      # No filtering applied
      temp_data = temp_data
    } else if (newpred_iC10_filter == "iC1") {
      temp_data = temp_data[
        temp_data$IC2 == 0 &
          temp_data$IC3 == 0 &
          temp_data$IC4 == 0 &
          temp_data$IC5 == 0 &
          temp_data$IC6 == 0 &
          temp_data$IC7 == 0 &
          temp_data$IC8 == 0 &
          temp_data$IC9 == 0 &
          temp_data$IC10 == 0, ]
    } else if (newpred_iC10_filter == "iC2") {
      temp_data = temp_data %>% dplyr::filter(IC2 == 1)
    } else if (newpred_iC10_filter == "iC3") {
      temp_data = temp_data %>% dplyr::filter(IC3 == 1)
    } else if (newpred_iC10_filter == "iC4") {
      temp_data = temp_data %>% dplyr::filter(IC4 == 1)
    } else if (newpred_iC10_filter == "iC5") {
      temp_data = temp_data %>% dplyr::filter(IC5 == 1)
    } else if (newpred_iC10_filter == "iC6") {
      temp_data = temp_data %>% dplyr::filter(IC6 == 1)
    } else if (newpred_iC10_filter == "iC7") {
      temp_data = temp_data %>% dplyr::filter(IC7 == 1)
    } else if (newpred_iC10_filter == "iC8") {
      temp_data = temp_data %>% dplyr::filter(IC8 == 1)
    } else if (newpred_iC10_filter == "iC9") {
      temp_data = temp_data %>% dplyr::filter(IC9 == 1)
    } else if (newpred_iC10_filter == "iC10") {
      temp_data = temp_data %>% dplyr::filter(IC10 == 1)
    }
    
    # Warning if no data left
    if (nrow(temp_data) == 0) {
      showModal(modalDialog(
        title = "Warning",
        "No samples in the dataset satisfy all selected filters.",
        easyClose = TRUE
      ))
    }
    
    reactive_filt_data <- temp_data
  }
  
  return(reactive_filt_data)
})