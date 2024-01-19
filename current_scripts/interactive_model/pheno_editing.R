# pam50
pam50_subtype_edit = reactive({ input$breast_cancer_new_prediction_pam50_annotation })
observeEvent(pam50_subtype_edit(), {
  # Create intermediate variable
  current_data = imported_data()

  # Create the columns in the unique sample (genes only) case
  if (import_data_type() == "Import unique sample (genes only)") {
    current_data$HER2 = NA
    current_data$LumB = NA
    current_data$LumA = NA
    current_data$Normal = NA
  }
  
  if (!is.null(imported_data()) && pam50_subtype_edit() != "Preset" &&
                                    pam50_subtype_edit() != "None selected") {
    # Proceed with editing
    if (pam50_subtype_edit() == "Random") {
      pam50s = c(`Luminal A` = c(0, 0, 1, 0), 
                 `Luminal B` = c(0, 1, 0, 0), 
                 `Normal-like` = c(0, 0, 0, 1), 
                 `Basal-like`= c(1, 0, 0, 0), 
                 `HER2+` = c(0, 0, 0, 0))
      current_data[, c("HER2", "LumB", "LumA", "Normal")] = pam50s[[sample(seq(1, 5), 1)]]
    } else if (pam50_subtype_edit() == "Luminal A") {
      current_data[, c("HER2", "LumB", "LumA", "Normal")] = c(0, 0, 1, 0)
    } else if (pam50_subtype_edit() == "Luminal B") {
      current_data[, c("HER2", "LumB", "LumA", "Normal")] = c(0, 1, 0, 0)
    } else if (pam50_subtype_edit() == "Normal-like") {
      current_data[, c("HER2", "LumB", "LumA", "Normal")] = c(0, 0, 0, 1)
    } else if (pam50_subtype_edit() == "HER2+") {
      current_data[, c("HER2", "LumB", "LumA", "Normal")] = c(1, 0, 0, 0)
    } else if (pam50_subtype_edit() == "Basal-like") {
      current_data[, c("HER2", "LumB", "LumA", "Normal")] = c(0, 0, 0, 0)
    }
    # Update imported_data()
    current_data[, c("HER2", "LumB", "LumA", "Normal")] <- 
      lapply(current_data[, c("HER2", "LumB", "LumA", "Normal")], function(x) 
        factor(x, levels = c(0, 1), labels = c(0, 1))
      )
    imported_data(current_data)
  } else if (!is.null(imported_data()) &&
             import_data_type() != "Import unique sample (genes only)"
             && pam50_subtype_edit() == "Preset") {
    # Update imported_data()
    current_data[, c("HER2", "LumB", "LumA", "Normal")] <- 
      lapply(current_data[, c("HER2", "LumB", "LumA", "Normal")], function(x) 
        factor(x, levels = c(0, 1), labels = c(0, 1))
      )
    imported_data(current_data)
  } else if (!is.null(imported_data()) && pam50_subtype_edit() == "Preset" &&
             import_data_type() == "Import unique sample (genes only)") {
    showModal(
      modalDialog(
        title = "Warning", 
        "No annotation from the top right panel can have the value 'None selected' or 'Preset' when importing a 'genes only' sample.", easyClose = TRUE 
      )
    )
    return() # exit code for this event
  } 
})

# scmod1
scmod1_subtype_edit = reactive({ input$breast_cancer_new_prediction_scmod1_annotation })
observeEvent(scmod1_subtype_edit(), {
  # Create intermediate variable
  current_data = imported_data()
  
  # Create the columns in the unique sample (genes only) case
  if (import_data_type() == "Import unique sample (genes only)") {
    current_data$ER_hp = NA
    current_data$ER_lp = NA
    current_data$HER2_scmod1 = NA
  }
  
  if (!is.null(imported_data()) && scmod1_subtype_edit() != "Preset" &&
      scmod1_subtype_edit() != "None selected") {
    
    # Proceed with editing
    if (scmod1_subtype_edit() == "Random") {
      scmod1s = c(`ER-/HER2-` = c(0, 0, 0), 
                  `ER+/HER2- high proliferation` = c(1, 0, 0),
                  `ER+/HER2- low proliferation` = c(0, 1, 0), 
                  `HER2+` = c(0, 0, 1))
      current_data[, c("ER_hp", "ER_lp", "HER2_scmod1")] = scmod1s[[sample(seq(1, 4), 1)]]
    } else if (scmod1_subtype_edit() == "ER-/HER2-") {
      current_data[, c("ER_hp", "ER_lp", "HER2_scmod1")] = c(0, 0, 0)
    } else if (scmod1_subtype_edit() == "ER+/HER2- high proliferation") {
      current_data[, c("ER_hp", "ER_lp", "HER2_scmod1")] = c(1, 0, 0)
    } else if (scmod1_subtype_edit() == "ER+/HER2- low proliferation") {
      current_data[, c("ER_hp", "ER_lp", "HER2_scmod1")] = c(0, 1, 0)
    } else if (scmod1_subtype_edit() == "HER2+") {
      current_data[, c("ER_hp", "ER_lp", "HER2_scmod1")] = c(0, 0, 1)
    }
    # Update imported_data()
    current_data[, c("ER_hp", "ER_lp", "HER2_scmod1")] <- 
      lapply(current_data[, c("ER_hp", "ER_lp", "HER2_scmod1")], function(x) 
        factor(x, levels = c(0, 1), labels = c(0, 1))
      )
    imported_data(current_data)
  } else if (!is.null(imported_data()) && scmod1_subtype_edit() == "Preset" &&
             import_data_type() != "Import unique sample (genes only)") {
    # Update imported_data()
    current_data[, c("ER_hp", "ER_lp", "HER2_scmod1")] <- 
      lapply(current_data[, c("ER_hp", "ER_lp", "HER2_scmod1")], function(x) 
        factor(x, levels = c(0, 1), labels = c(0, 1))
      )
    imported_data(current_data)
  } else if (!is.null(imported_data()) && scmod1_subtype_edit() == "Preset" &&
             import_data_type() == "Import unique sample (genes only)") {
    showModal(
      modalDialog(
        title = "Warning", 
        "No annotation from the top right panel can have the value 'None selected' or 'Preset' when importing a 'genes only' sample.", easyClose = TRUE 
      )
    )
    return() # exit code for this event
  }
})

# Timepoint
timepoint_edit = reactive({ input$breast_cancer_new_prediction_timepoint_annotation })
observeEvent(timepoint_edit(), {
  # Create intermediate variable
  current_data = imported_data()
  
  # Create the columns in the unique sample (genes only) case
  if (import_data_type() == "Import unique sample (genes only)") {
    current_data$T2 = NA
  }
  
  if (!is.null(imported_data()) && timepoint_edit() != "Preset" &&
      timepoint_edit() != "None selected") {
    
    # Proceed with editing
    if (timepoint_edit() == "Random") {
      timepoints = c(`Pre-treatment` = 0, `On-treatment` = 1)
      current_data[, "T2"] = timepoints[[sample(seq(1, 2), 1)]]
    } else if (timepoint_edit() == "Pre-treatment") {
      current_data[, "T2"] = 0
    } else {
      current_data[, "T2"] = 1
    }
    # Update imported_data()
    current_data[, "T2"] <- 
      lapply(current_data[, "T2"], function(x) 
        factor(x, levels = c(0, 1), labels = c(0, 1))
      )
    imported_data(current_data)
  } else if (!is.null(imported_data()) && timepoint_edit() == "Preset" &&
             import_data_type() != "Import unique sample (genes only)") {
    # Update imported_data()
    current_data[, "T2"] <- 
      lapply(current_data[, "T2"], function(x) 
        factor(x, levels = c(0, 1), labels = c(0, 1))
      )
    imported_data(current_data)
  } else if (!is.null(imported_data()) && timepoint_edit() == "Preset" &&
             import_data_type() == "Import unique sample (genes only)") {
    showModal(
      modalDialog(
        title = "Warning", 
        "No annotation from the top right panel can have the value 'None selected' or 'Preset' when importing a 'genes only' sample.", easyClose = TRUE 
      )
    )
    return() # exit code for this event
  }
})

# iC10
iC10_edit = reactive({ input$breast_cancer_new_prediction_ic10_annotation })
observeEvent(iC10_edit(), {
  # Create intermediate variable
  current_data = imported_data()
  
  # Create the columns in the unique sample (genes only) case
  if (import_data_type() == "Import unique sample (genes only)") {
    current_data$IC2 = NA
    current_data$IC3 = NA
    current_data$IC4 = NA
    current_data$IC5 = NA
    current_data$IC6 = NA
    current_data$IC7 = NA
    current_data$IC8 = NA
    current_data$IC9 = NA
    current_data$IC10 = NA
  }
  
  if (!is.null(imported_data()) && iC10_edit() != "Preset" &&
      iC10_edit() != "None selected") {
    
    # Proceed with editing
    if (iC10_edit() == "Random") {
      iC10s = c(`iC1` = c(0, 0, 0, 0, 0, 0, 0, 0, 0), 
                `iC2` = c(1, 0, 0, 0, 0, 0, 0, 0, 0),
                `iC3` = c(0, 1, 0, 0, 0, 0, 0, 0, 0), 
                `iC4` = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
                `iC5` = c(0, 0, 0, 1, 0, 0, 0, 0, 0), 
                `iC6` = c(0, 0, 0, 0, 1, 0, 0, 0, 0), 
                `iC7` = c(0, 0, 0, 0, 0, 1, 0, 0, 0), 
                `iC8` = c(0, 0, 0, 0, 0, 0, 1, 0, 0), 
                `iC9` = c(0, 0, 0, 0, 0, 0, 0, 1, 0),
                `iC10` = c(0, 0, 0, 0, 0, 0, 0, 0, 1))
      current_data[, c("IC2", "IC3", "IC4",
                       "IC5", "IC6", "IC7", "IC8", "IC9",
                       "IC10")] = iC10s[[sample(seq(1, 10), 1)]]
    } else if (iC10_edit() == "iC1") {
      current_data[, c("IC2", "IC3", "IC4",
                       "IC5", "IC6", "IC7", "IC8", "IC9",
                       "IC10")] = c(0, 0, 0, 0, 0, 0, 0, 0, 0)
    } else if (iC10_edit() == "iC2") {
      current_data[, c("IC2", "IC3", "IC4",
                       "IC5", "IC6", "IC7", "IC8", "IC9",
                       "IC10")] = c(1, 0, 0, 0, 0, 0, 0, 0, 0)
    } else if (iC10_edit() == "iC3") {
      current_data[, c("IC2", "IC3", "IC4",
                       "IC5", "IC6", "IC7", "IC8", "IC9",
                       "IC10")] = c(0, 1, 0, 0, 0, 0, 0, 0, 0)
    } else if (iC10_edit() == "iC4") {
      current_data[, c("IC2", "IC3", "IC4",
                       "IC5", "IC6", "IC7", "IC8", "IC9",
                       "IC10")] = c(0, 0, 1, 0, 0, 0, 0, 0, 0)
    } else if (iC10_edit() == "iC5") {
      current_data[, c("IC2", "IC3", "IC4",
                       "IC5", "IC6", "IC7", "IC8", "IC9",
                       "IC10")] = c(0, 0, 0, 1, 0, 0, 0, 0, 0)
    } else if (iC10_edit() == "iC6") {
      current_data[, c("IC2", "IC3", "IC4",
                       "IC5", "IC6", "IC7", "IC8", "IC9",
                       "IC10")] = c(0, 0, 0, 0, 1, 0, 0, 0, 0)
    } else if (iC10_edit() == "iC7") {
      current_data[, c("IC2", "IC3", "IC4",
                       "IC5", "IC6", "IC7", "IC8", "IC9",
                       "IC10")] = c(0, 0, 0, 0, 0, 1, 0, 0, 0)
    } else if (iC10_edit() == "iC8") {
      current_data[, c("IC2", "IC3", "IC4",
                       "IC5", "IC6", "IC7", "IC8", "IC9",
                       "IC10")] = c(0, 0, 0, 0, 0, 0, 1, 0, 0)
    } else if (iC10_edit() == "iC9") {
      current_data[, c("IC2", "IC3", "IC4",
                       "IC5", "IC6", "IC7", "IC8", "IC9",
                       "IC10")] = c(0, 0, 0, 0, 0, 0, 0, 1, 0)
    } else if (iC10_edit() == "iC10") {
      current_data[, c("IC2", "IC3", "IC4",
                       "IC5", "IC6", "IC7", "IC8", "IC9",
                       "IC10")] = c(0, 0, 0, 0, 0, 0, 0, 0, 1)
    }
    # Update imported_data()
    current_data[, c("IC2", "IC3", "IC4",
                     "IC5", "IC6", "IC7", "IC8", "IC9",
                     "IC10")] <- 
      lapply(current_data[, c("IC2", "IC3", "IC4",
                              "IC5", "IC6", "IC7", "IC8", "IC9",
                              "IC10")], function(x) 
                                factor(x, levels = c(0, 1), labels = c(0, 1))
      )
    imported_data(current_data)
  } else if (!is.null(imported_data()) && iC10_edit() == "Preset" &&
             import_data_type() != "Import unique sample (genes only)") {
    # Update imported_data()
    current_data[, c("IC2", "IC3", "IC4",
                     "IC5", "IC6", "IC7", "IC8", "IC9",
                     "IC10")] <- 
      lapply(current_data[, c("IC2", "IC3", "IC4",
                              "IC5", "IC6", "IC7", "IC8", "IC9",
                              "IC10")], function(x) 
                                factor(x, levels = c(0, 1), labels = c(0, 1))
      )
    imported_data(current_data)
  } else if (!is.null(imported_data()) && iC10_edit() == "Preset" &&
             import_data_type() == "Import unique sample (genes only)") {
    showModal(
      modalDialog(
        title = "Warning", 
        "No annotation from the top right panel can have the value 'None selected' or 'Preset' when importing a 'genes only' sample.", easyClose = TRUE 
      )
    )
    return() # exit code for this event
  }
})

# Mammaprint
mammaprint_edit = reactive({ input$breast_cancer_new_prediction_mammaprint_annotation })
observeEvent(mammaprint_edit(), {
  # Create intermediate variable
  current_data = imported_data()
  
  # Create the columns in the unique sample (genes only) case
  if (import_data_type() == "Import unique sample (genes only)") {
    current_data$Mammaprint_risk_yes = NA
  }
  
  if (!is.null(imported_data()) && mammaprint_edit() != "Preset" &&
      mammaprint_edit() != "None selected") {
   
    # Proceed with editing
    if (mammaprint_edit() == "Random") {
      mammaprints = c(`At risk` = 1, `No risk` = 0)
      current_data[, "Mammaprint_risk_yes"] = mammaprints[[sample(seq(1, 2), 1)]]
    } else if (mammaprint_edit() == "At risk") {
      current_data[, "Mammaprint_risk_yes"] = 1
    } else {
      current_data[, "Mammaprint_risk_yes"] = 0
    }
    # Update imported_data()
    current_data[, "Mammaprint_risk_yes"] <- 
      lapply(current_data[, "Mammaprint_risk_yes"], function(x) 
        factor(x, levels = c(0, 1), labels = c(0, 1))
      )
    imported_data(current_data)
  } else if (!is.null(imported_data()) && mammaprint_edit() == "Preset" &&
             import_data_type() != "Import unique sample (genes only)") {
    # Update imported_data()
    current_data[, "Mammaprint_risk_yes"] <- 
      lapply(current_data[, "Mammaprint_risk_yes"], function(x) 
        factor(x, levels = c(0, 1), labels = c(0, 1))
      )
    imported_data(current_data)
  } else if (!is.null(imported_data()) && mammaprint_edit() == "Preset" &&
             import_data_type() == "Import unique sample (genes only)") {
    showModal(
      modalDialog(
        title = "Warning", 
        "No annotation from the top right panel can have the value 'None selected' or 'Preset' when importing a 'genes only' sample.", easyClose = TRUE 
      )
    )
    return() # exit code for this event
  }
})

# rorS
rorS_edit = reactive({ input$breast_cancer_new_prediction_rors_annotation })
observeEvent(rorS_edit(), {
  # Create intermediate variable
  current_data = imported_data()
  
  # Create the columns in the unique sample (genes only) case
  if (import_data_type() == "Import unique sample (genes only)") {
    current_data$rorS_risk_interm = NA
    current_data$rorS_risk_high = NA
  }
  
  if (!is.null(imported_data()) && rorS_edit() != "Preset" &&
      rorS_edit() != "None selected") {
    
    # Proceed with editing
    if (rorS_edit() == "Random") {
      rorSs = c(High= c(0, 1), Intermediate = c(1, 0), Low = c(0, 0))
      current_data[, c("rorS_risk_interm", "rorS_risk_high")] = rorSs[[sample(seq(1, 3), 1)]]
    } else if (rorS_edit() == "High") {
      current_data[, c("rorS_risk_interm", "rorS_risk_high")] = c(0, 1)
    } else if (rorS_edit() == "Intermediate") {
      current_data[, c("rorS_risk_interm", "rorS_risk_high")] = c(1, 0)
    } else if (rorS_edit() == "Low") {
      current_data[, c("rorS_risk_interm", "rorS_risk_high")] = c(0, 0)
    }
    # Update imported_data()
    current_data[, c("rorS_risk_interm", "rorS_risk_high")] <- 
      lapply(current_data[, c("rorS_risk_interm", "rorS_risk_high")], function(x) 
        factor(x, levels = c(0, 1), labels = c(0, 1))
      )
    imported_data(current_data)
  } else if (!is.null(imported_data()) && rorS_edit() == "Preset" &&
             import_data_type() != "Import unique sample (genes only)") {
    # Update imported_data()
    current_data[, c("rorS_risk_interm", "rorS_risk_high")] <- 
      lapply(current_data[, c("rorS_risk_interm", "rorS_risk_high")], function(x) 
        factor(x, levels = c(0, 1), labels = c(0, 1))
      )
    imported_data(current_data)
  } else if (!is.null(imported_data()) && rorS_edit() == "Preset" &&
             import_data_type() == "Import unique sample (genes only)") {
    showModal(
      modalDialog(
        title = "Warning", 
        "No annotation from the top right panel can have the value 'None selected' or 'Preset' when importing a 'genes only' sample.", easyClose = TRUE 
      )
    )
    return() # exit code for this event
  }
})