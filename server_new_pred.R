# Breast Cancer: Unique sample predictor #####
# Additional tab for importing single sample or new dataset (appropriately formatted)
# and make predictions

# Import data type
import_data_type = reactive({
  input$breast_cancer_new_prediction_type
})

# Desired treatment
desired_treatment = reactive({ input$breast_cancer_new_prediction_treatment })

# File input
file_input = reactive({ input$breast_cancer_new_prediction_file_input })

# Housekeeping
all_correct_columns = colnames(full_ml_set)[3:186]
all_correct_columns = all_correct_columns[all_correct_columns != "Endo"]
gene_columns_with_X = colnames(full_ml_set)[which(grepl("X_", colnames(full_ml_set)))]
gene_columns_without_X = gene_columns_with_X
str_sub(gene_columns_without_X, 1, 2) = ""
pheno_columns = setdiff(all_correct_columns, gene_columns_with_X)
all_correct_columns_without_X = c(gene_columns_without_X, pheno_columns)

# Reactive value import
imported_data <- reactiveVal()

# Hide File input and Import buttons if Random generation is chosen
observeEvent(import_data_type(), {
  if (grepl("Random", import_data_type())) {
    shinyjs::hide("breast_cancer_new_prediction_file_input")
    shinyjs::hide("import_new_prediction_breast_cancer")
    
    # Generate a 166-slot-long vector of normally distributed values
    gene_vector = rnorm(166, mean = 0, sd = 1)
    
    # pam50
    pam50_zeros_vector = rep(0, 4)
    single_one_pam50_vector = pam50_zeros_vector[sample(1:4, 1)] = 1
    random_pam50_vector = ifelse(runif(1) < 0.5, pam50_zeros_vector, 
                                 single_one_pam50_vector)
    
    # timepoint
    random_timepoint_vector = sample(c(0, 1), 1)
    
    # treatment
    random_treatment_vector = sample(c(0, 1), 1)
    
    # iC10
    iC10_zeros_vector = rep(0, 9)
    single_one_iC10_vector = iC10_zeros_vector[sample(1:9, 1)] = 1
    random_iC10_vector = ifelse(runif(1) < 0.5, iC10_zeros_vector, 
                                single_one_iC10_vector)
    
    # Mammaprint
    random_mammaprint_vector = sample(c(0, 1), 1)
    
    # rorS
    rorS_zeros_vector = rep(0, 2)
    single_one_rorS_vector = rorS_zeros_vector[sample(1:2, 1)] = 1
    random_rorS_vector = ifelse(runif(1) < 0.5, rorS_zeros_vector, 
                                single_one_rorS_vector)
    
    full_random_vector = c(genes_vector, random_pam50_vector, random_timepoint_vector,
                           random_treatment_vector, random_iC10_vector,
                           random_mammaprint_vector, random_rorS_vector)
    names(full_random_vector) = colnames(full_ml_set)[3:186]
    
    # Make reactive
    imported_data(full_random_vector)
    
    # Verification message
    showModal(
      modalDialog(
        title = "Success!", "Random file generated successfully. You may now edit
          annotation (pam50, iC10, timepoint, treatment, Mammaprint or rorS risk) if
          you wish, before generating a prediction.",
        easyClose = TRUE 
      )
    )
    
  } else {
    shinyjs::show("breast_cancer_new_prediction_file_input")
    shinyjs::show("import_new_prediction_breast_cancer")
  }
})

# Hide filters if not dataset
observeEvent(import_data_type(), {
  if (import_data_type() == "Import pre-annotated dataset") {
    shinyjs::show("breast_cancer_new_prediction_timepoint_filter")
    shinyjs::show("breast_cancer_new_prediction_pam50_filter")
    shinyjs::show("breast_cancer_new_prediction_rorS_filter")
    shinyjs::show("breast_cancer_new_prediction_Mammaprint_filter")
    shinyjs::show("breast_cancer_new_prediction_ic10_filter")
  } else {
    shinyjs::hide("breast_cancer_new_prediction_timepoint_filter")
    shinyjs::hide("breast_cancer_new_prediction_pam50_filter")
    shinyjs::hide("breast_cancer_new_prediction_rorS_filter")
    shinyjs::hide("breast_cancer_new_prediction_Mammaprint_filter")
    shinyjs::hide("breast_cancer_new_prediction_ic10_filter")
  }
})

# If import is chosen: Load the file
observeEvent(input$import_new_prediction_breast_cancer, {
  req(file_input())
  
  # Read the uploaded file
  if (grepl(".csv$", file_input$datapath)) {
    input_data = readr::read_csv(file_input$datapath, show_col_types = FALSE)
  } else if (grepl(".txt$", file_input$datapath)) {
    input_data = data.table::fread(file_input$datapath)
  } else if (grepl(".xlsx$", file_input$datapath)) {
    input_data = openxlsx::read.xlsx(file_input$datapth)
  } else if (grepl(".tsv$", file_input$datapath)) {
    input_data = readr::read_tsv(file_input$datapath, show_col_types = FALSE)
  }
  
  full_colnames_input_data = colnames(input_data)
  # Check if file meets all requirements
  if (is.null(input_data)) {
    showModal(
      modalDialog(
        title = "Warning", "The imported file is empty.", easyClose = TRUE 
      )
    )
  } else {
    # Assert if gene names are with X_ prefix or not and count gene columns
    if (grepl("X_", colnames(input_data))) {
      X_handle = "on"
      gene_colnames_input_data = intersect(colnames(input_data), gene_columns_with_X)
      X_columns_imported_number = length(gene_colnames_input_data)
    } else {
      X_handle = "off"
      gene_colnames_input_data = intersect(intersect(colnames(input_data), gene_columns_without_X))
      nonX_gene_columns_imported_number = length(gene_colnames_input_data)
    }
    
    # Check if they are 166 and correct
    if (X_handle == "on" && X_columns_imported_number < 166) {
      showModal(
        modalDialog(
          title = "Warning", 
          paste0("There are missing markers in the data. 166 variables required.",
                 "\n", "Problem with:", setdiff(gene_columns_with_X, gene_colnames_input_data)), easyClose = TRUE 
        )
      )
    } else if (X_handle == "off" && nonX_gene_columns_imported_number < 166) {
      showModal(
        modalDialog(
          title = "Warning", 
          paste0("There are missing markers in the data. 166 variables required.",
                 "\n", "Problem with:", setdiff(gene_columns_without_X, gene_colnames_input_data)), easyClose = TRUE 
        )
      )
    }
  }
  
  # Check the files only contain one row (unique samples)
  if (import_data_type == "Import unique sample (genes only)" && nrow(input_data) > 1) {
    showModal(
      modalDialog(
        title = "Warning", 
        "There are more than one rows in this sample. Only one-row files permitted.", easyClose = TRUE 
      )
    )
  }
  
  if (import_data_type == "Import unique sample (pre-annotated)" && nrow(input_data) > 1) {
    showModal(
      modalDialog(
        title = "Warning", 
        "There are more than one rows in this sample. Only one-row files permitted.", easyClose = TRUE 
      )
    )
  }
  
  # Check the rows are numeric in genes only - unique samples
  if (import_data_type == "Import unique sample (genes only)") {
    first_row <- input_data[1, ]
    if (any(!sapply(first_row, is.numeric))) {
      # Attempt to convert non-numeric values to numeric
      converted_row <- suppressWarnings(as.numeric(first_row))
      if (any(is.na(converted_row))) {
        showModal(modalDialog(
          title = "Warning",
          "The uploaded file contains non-numeric values that could not be converted to numeric.",
          easyClose = TRUE
        ))
      } else {
        input_data[1, ] <- converted_row
      }
    }
  } else if (import_data_type == "Import unique sample (pre-annotated)" && X_handle == "on") {
    first_row <- input_data[1, gene_columns_with_X]
    if (any(!sapply(first_row, is.numeric))) {
      # Attempt to convert non-numeric values to numeric
      converted_row <- suppressWarnings(as.numeric(first_row))
      if (any(is.na(converted_row))) {
        showModal(modalDialog(
          title = "Warning",
          "The uploaded file contains non-numeric values in the gene variables
              that could not be converted to numeric.",
          easyClose = TRUE
        ))
      } else {
        input_data[1, gene_columns_with_X] <- converted_row
      }
    }
  } else if (import_data_type == "Import unique sample (pre-annotated)" && X_handle == "off") {
    first_row <- input_data[1, gene_columns_without_X]
    if (any(!sapply(first_row, is.numeric))) {
      # Attempt to convert non-numeric values to numeric
      converted_row <- suppressWarnings(as.numeric(first_row))
      if (any(is.na(converted_row))) {
        showModal(modalDialog(
          title = "Warning",
          "The uploaded file contains non-numeric values in the gene variables
              that could not be converted to numeric.",
          easyClose = TRUE
        ))
      } else {
        input_data[1, gene_columns_without_X] <- converted_row
      }
    }
  } else if (import_data_type == "Import pre-annotated dataset" && X_handle == "on") {
    if (any(!sapply(input_data[, gene_columns_with_X], is.numeric))) {
      # Attempt to convert non-numeric values to numeric
      converted_data <- suppressWarnings(as.data.frame(lapply(input_data[, gene_columns_with_X], as.numeric)))
      if (any(is.na(converted_data))) {
        showModal(modalDialog(
          title = "Warning",
          "The uploaded file contains non-numeric values that could not be converted to numeric.",
          easyClose = TRUE
        ))
      } else {
        input_data[, gene_columns_with_X] <- converted_data
      }
    }
  } else if (import_data_type == "Import pre-annotated dataset" && X_handle == "off") {
    if (any(!sapply(input_data[, gene_columns_without_X], is.numeric))) {
      # Attempt to convert non-numeric values to numeric
      converted_data <- suppressWarnings(as.data.frame(lapply(input_data[, gene_columns_without_X], as.numeric)))
      if (any(is.na(converted_data))) {
        showModal(modalDialog(
          title = "Warning",
          "The uploaded file contains non-numeric values that could not be converted to numeric.",
          easyClose = TRUE
        ))
      } else {
        input_data[, gene_columns_without_X] <- converted_data
      }
    }
  }
  
  # If genes and annotation is chosen check all columns needed are present
  if (import_data_type %in% c("Import unique sample (pre-annotated)",
                              "Import pre-annotated dataset")) {
    if (length(intersect(colnames(input_data), all_correct_columns)) < 183) {
      showModal(
        modalDialog(
          title = "Warning", 
          paste0("There are missing columns in the data.",
                 "\n", "Problem with:", setdiff(all_correct_columns, colnames(input_data))), easyClose = TRUE 
        )
      )
    }
  }
  
  # Make sure all other columns have 0 or 1 and are converted to factors
  if (import_data_type == "Import unique sample (pre-annotated)") {
    if (!all(input_data[1, pheno_columns] %in% c(0, 1))) {
      showModal(
        modalDialog(
          title = "Warning", 
          "All values in the phenotypic column should be either 0 or 1.", easyClose = TRUE 
        )
      )
    }
  } else if (import_data_type == "Import pre-annotated dataset") {
    if (!all(apply(input_data[, pheno_columns], 2, function(col) all(col %in% c(0, 1))))) {
      showModal(
        modalDialog(
          title = "Warning", 
          "All values in the phenotypic column should be either 0 or 1.", easyClose = TRUE 
        )
      )
    }
  }
  
  # Convert pheno columns to factors
  if (import_data_type == "Import unique sample (pre-annotated)") {
    first_row <- input_data[1, pheno_columns]
    if (any(!sapply(first_row, is.factor))) {
      # Attempt to convert to factors
      converted_row <- suppressWarnings(factor(first_row, levels = c(0, 1), labels = c(0, 1)))
      if (any(is.na(converted_row))) {
        showModal(modalDialog(
          title = "Warning",
          "The uploaded file contains values that could not be converted to factors.",
          easyClose = TRUE
        ))
      } else {
        input_data[1, pheno_columns] <- converted_row
      }
    }
  } else if (import_data_type == "Import pre-annotated dataset") {
    if (any(!sapply(input_data[, pheno_columns], is.factor))) {
      # Attempt to convert non-numeric values to numeric
      converted_data <- suppressWarnings(as.data.frame(lapply(input_data[, pheno_columns], 
                                                              factor(input_data[, pheno_columns],
                                                                     levels = c(0, 1), labels = c(0, 1)))))
      if (any(is.na(converted_data))) {
        showModal(modalDialog(
          title = "Warning",
          "The uploaded file contains values that could not be converted to factors.",
          easyClose = TRUE
        ))
      } else {
        input_data[, pheno_columns] <- converted_data
      }
    }
  }
  
  # Convert to reactive
  imported_data(input_data)
})

# Check if treatment is provided in the imported data
observeEvent(desired_treatment(), {
  if (desired_treatment() == "Included in input") {
    if (!grepl("Endo", colnames(imported_data()))) {
      showModal(modalDialog(
        title = "Warning",
        "The uploaded file should contain a column named 'Endo' with a value 
          0 (Chemotherapy) or 1 (Endocrine treatment).",
        easyClose = TRUE
      ))
    }
  } else if (desired_treatment() == "Chemotherapy") {
    imported_data()[, "Endo"] = 0
  } else {
    imported_data()[, "Endo"] = 1
  }
})

# Sample pheno editing:
# pam50
pam50_subtype_edit = reactive({ input$breast_cancer_new_prediction_pam50_annotation })
observeEvent(pam50_subtype_edit(), {
  if (!is.null(imported_data()) && pam50_subtype_edit() != "Preset") {
    if (pam50_subtype_edit() == "Random") {
      pam50s = c("Luminal A", "Luminal B", "Normal-like", "Basal-like", "HER2+")
      new_subtype = pam50s[sample(seq(1, 5), 1)]
      pam50_subtype_edit(new_subtype)  # Update the reactive expression
    }
    
    # Edit data
    if (pam50_subtype_edit() == "Luminal A") {
      imported_data()[, c("HER2", "LumB", "LumA", "Normal")] = c(0, 0, 1, 0)
    } else if (pam50_subtype_edit() == "Luminal B") {
      imported_data()[, c("HER2", "LumB", "LumA", "Normal")] = c(0, 1, 0, 0)
    } else if (pam50_subtype_edit() == "Normal-like") {
      imported_data()[, c("HER2", "LumB", "LumA", "Normal")] = c(0, 0, 0, 1)
    } else if (pam50_subtype_edit() == "HER2+") {
      imported_data()[, c("HER2", "LumB", "LumA", "Normal")] = c(1, 0, 0, 0)
    } else if (pam50_subtype_edit() == "Basal-like") {
      imported_data()[, c("HER2", "LumB", "LumA", "Normal")] = c(0, 0, 0, 0)
    }
  }
})

# Timepoint
timepoint_edit = reactive({ input$breast_cancer_new_prediction_timepoint_annotation })
observeEvent(timepoint_edit(), {
  if (!is.null(imported_data()) && timepoint_edit() != "Preset") {
    if (timepoint_edit() == "Random") {
      timepoints = c("Pre-treatment", "On-treatment")
      new_timepoint = timepoints[sample(seq(1, 2), 1)]
      timepoint_edit(new_timepoint)
    }
    
    # Edit data
    if (timepoint_edit() == "Pre-treatment") {
      imported_data()[, "T2"] = 0
    } else {
      imported_data()[, "T2"] = 1
    }
  }
})

# iC10
iC10_edit = reactive({ input$breast_cancer_new_prediction_ic10_annotation })
observeEvent(iC10_edit(), {
  if (!is.null(imported_data()) && iC10_edit() != "Preset") {
    if (iC10_edit == "Random") {
      iC10s = c("iC1", "iC2", "iC3", "iC4",
                "iC5", "iC6", "iC7", "iC8", "iC9",
                "iC10")
      new_iC10 = iC10s[sample(seq(1, 10), 1)]
      iC10_edit(new_iC10)
    }
    
    # Edit data
    if (iC10_edit() == "iC1") {
      imported_data()[, c("IC2", "IC3", "IC4",
                          "IC5", "IC6", "IC7", "IC8", "IC9",
                          "IC10")] = c(0, 0, 0, 0, 0, 0, 0, 0, 0)
    } else if (iC10_edit() == "iC2") {
      imported_data()[, c("IC2", "IC3", "IC4",
                          "IC5", "IC6", "IC7", "IC8", "IC9",
                          "IC10")] = c(1, 0, 0, 0, 0, 0, 0, 0, 0)
    } else if (iC10_edit() == "iC3") {
      imported_data()[, c("IC2", "IC3", "IC4",
                          "IC5", "IC6", "IC7", "IC8", "IC9",
                          "IC10")] = c(0, 1, 0, 0, 0, 0, 0, 0, 0)
    } else if (iC10_edit() == "iC4") {
      imported_data()[, c("IC2", "IC3", "IC4",
                          "IC5", "IC6", "IC7", "IC8", "IC9",
                          "IC10")] = c(0, 0, 1, 0, 0, 0, 0, 0, 0)
    } else if (iC10_edit() == "iC5") {
      imported_data()[, c("IC2", "IC3", "IC4",
                          "IC5", "IC6", "IC7", "IC8", "IC9",
                          "IC10")] = c(0, 0, 0, 1, 0, 0, 0, 0, 0)
    } else if (iC10_edit() == "iC6") {
      imported_data()[, c("IC2", "IC3", "IC4",
                          "IC5", "IC6", "IC7", "IC8", "IC9",
                          "IC10")] = c(0, 0, 0, 0, 1, 0, 0, 0, 0)
    } else if (iC10_edit() == "iC7") {
      imported_data()[, c("IC2", "IC3", "IC4",
                          "IC5", "IC6", "IC7", "IC8", "IC9",
                          "IC10")] = c(0, 0, 0, 0, 0, 1, 0, 0, 0)
    } else if (iC10_edit() == "iC8") {
      imported_data()[, c("IC2", "IC3", "IC4",
                          "IC5", "IC6", "IC7", "IC8", "IC9",
                          "IC10")] = c(0, 0, 0, 0, 0, 0, 1, 0, 0)
    } else if (iC10_edit() == "iC9") {
      imported_data()[, c("IC2", "IC3", "IC4",
                          "IC5", "IC6", "IC7", "IC8", "IC9",
                          "IC10")] = c(0, 0, 0, 0, 0, 0, 0, 1, 0)
    } else if (iC10_edit() == "iC10") {
      imported_data()[, c("IC2", "IC3", "IC4",
                          "IC5", "IC6", "IC7", "IC8", "IC9",
                          "IC10")] = c(0, 0, 0, 0, 0, 0, 0, 0, 1)
    }
  }
})

# Mammaprint
mammaprint_edit = reactive({ input$breast_cancer_new_prediction_mammaprint_annotation })
observeEvent(mammaprint_edit(), {
  if (!is.null(imported_data()) && mammaprint_edit() != "Preset") {
    if (mammaprint_edit() == "Random") {
      mammaprints = c("At risk", "No risk")
      new_mammaprint = mammaprints[sample(seq(1, 2), 1)]
      mammaprint_edit(new_mammaprint)
    }
    
    # Edit data
    if (mammaprint_edit() == "At risk") {
      imported_data()[, "Mammaprint_risk_yes"] = 1
    } else {
      imported_data()[, "Mammaprint_risk_yes"] = 0
    }
  }
})

# rorS
rorS_edit = reactive({ input$breast_cancer_new_prediction_rors_annotation })
observeEvent(rorS_edit(), {
  if (!is.null(imported_data()) && rorS_edit() != "Preset") {
    if (rorS_edit == "Random") {
      rorSs = c("High", "Intermediate", "Low")
      new_rorS = rorSs[sample(seq(1, 3), 1)]
      rorS_edit(new_rorS)
    }
    
    # Edit data
    if (rorS_edit() == "High") {
      imported_data()[, c("rorS_risk_interm", "rorS_risk_high")] = c(0, 1)
    } else if (rorS_edit() == "Intermediate") {
      imported_data()[, c("rorS_risk_interm", "rorS_risk_high")] = c(1, 0)
    } else if (rorS_edit() == "Low") {
      imported_data()[, c("rorS_risk_interm", "rorS_risk_high")] = c(0, 0)
    }
  }
})

# ROC plot checks ###
# Hide ROCplot option if not dataset
observeEvent(import_data_type(), {
  if (import_data_type() == "Import pre-annotated dataset") {
    shinyjs::show("breast_cancer_new_prediction_roc")
    roc_possibility = TRUE
  } else {
    shinyjs::hide("breast_cancer_new_prediction_roc")
    roc_possibility = FALSE
  }
})

roc_handle = reactive({ input$breast_cancer_new_prediction_roc })
# Check there is a response column with values "Responder", "Non_responder" in the input
observeEvent(roc_handle(), {
  if (roc_handle() == "Yes" && roc_possibility)  {
    # Check if there is a response column
    if (!"Response" %in% colnames(imported_data())) {
      test_passed = FALSE
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
      test_passed = TRUE
    } else {
      test_passed = FALSE
      showModal(modalDialog(
        title = "Warning",
        "The uploaded file should contain a column named 'Response' with levels
          'Responder', 'Non_responder'.",
        easyClose = TRUE
      ))
    }
  }
})

# Filters  
filtered_data = reactive({
  # Only run in the case of datasets
  if (import_data_type() == "Import pre-annotated dataset") {
    reactive_filt_data = imported_data()
    
    # Filters
    newpred_timepoint_filter = input$breast_cancer_new_prediction_timepoint_filter
    newpred_pam50_filter = input$breast_cancer_new_prediction_pam50_filter
    newpred_rorS_filter = input$breast_cancer_new_prediction_rorS_filter
    newpred_mammaprint_filter = input$breast_cancer_new_prediction_Mammaprint_filter
    newpred_iC10_filter = input$breast_cancer_new_prediction_ic10_filter
    
    # Filter timepoint
    if (newpred_timepoint_filter == "All") {
      # No filtering applied
    } else if (newpred_timepoint_filter == "Pre-treatment") {
      reactive_filt_data = reactive_filt_data %>% filter(T2 == 0)
    } else if (newpred_timepoint_filter == "On-treatment") {
      reactive_filt_data = reactive_filt_data %>% filter(T2 == 1)
    }
    
    # pam50
    if (newpred_pam50_filter == "All") {
      # No filtering applied
    } else if (newpred_pam50_filter == "Luminal A") {
      reactive_filt_data = reactive_filt_data %>% dplyr::filter(LumA == 1)
    } else if (newpred_pam50_filter == "Luminal B") {
      reactive_filt_data = reactive_filt_data %>% dplyr::filter(LumB == 1)
    } else if (newpred_pam50_filter == "Normal-like") {
      reactive_filt_data = reactive_filt_data %>% dplyr::filter(Normal == 1)
    } else if (newpred_pam50_filter == "HER2+") {
      reactive_filt_data = reactive_filt_data %>% dplyr::filter(HER2 == 1)
    } else if (newpred_pam50_filter == "Basal-like") {
      reactive_filt_data = reactive_filt_data[reactive_filt_data$LumA==0 &
                                                reactive_filt_data$LumB==0 &
                                                reactive_filt_data$Normal==0 &
                                                reactive_filt_data$HER2==0,]
    }
    
    # rorS
    if (newpred_rorS_filter == "All") {
      # No filtering applied
    } else if (newpred_rorS_filter == "High") {
      reactive_filt_data = reactive_filt_data %>% dplyr::filter(rorS_risk_high == 1)
    } else if (newpred_rorS_filter == "Intermediate") {
      reactive_filt_data = reactive_filt_data %>% dplyr::filter(rorS_risk_interm == 1)
    } else if (newpred_rorS_filter == "Low") {
      reactive_filt_data = reactive_filt_data[reactive_filt_data$rorS_risk_high==0 &
                                                reactive_filt_data$rorS_risk_interm==0,]
    }
    
    # Mammaprint
    if (newpred_mammaprint_filter == "All") {
      # No filtering applied
    } else if (newpred_mammaprint_filter == "At risk") {
      reactive_filt_data = reactive_filt_data %>% dplyr::filter(Mammaprint_risk_yes == 1)
    } else if (newpred_mammaprint_filter == "No risk") {
      reactive_filt_data = reactive_filt_data %>% dplyr::filter(Mammaprint_risk_yes == 0)
    }
    
    # iC10
    if (newpred_iC10_filter == "All") {
      # No filtering applied
    } else if (newpred_iC10_filter == "iC1") {
      reactive_filt_data = reactive_filt_data[
        reactive_filt_data$IC2 == 0 &
          reactive_filt_data$IC3 == 0 &
          reactive_filt_data$IC4 == 0 &
          reactive_filt_data$IC5 == 0 &
          reactive_filt_data$IC6 == 0 &
          reactive_filt_data$IC7 == 0 &
          reactive_filt_data$IC8 == 0 &
          reactive_filt_data$IC9 == 0 &
          reactive_filt_data$IC10 == 0, ]
    } else if (newpred_iC10_filter == "iC2") {
      reactive_filt_data = reactive_filt_data %>% dplyr::filter(IC2 == 1)
    } else if (newpred_iC10_filter == "iC3") {
      reactive_filt_data = reactive_filt_data %>% dplyr::filter(IC3 == 1)
    } else if (newpred_iC10_filter == "iC4") {
      reactive_filt_data = reactive_filt_data %>% dplyr::filter(IC4 == 1)
    } else if (newpred_iC10_filter == "iC5") {
      reactive_filt_data = reactive_filt_data %>% dplyr::filter(IC5 == 1)
    } else if (newpred_iC10_filter == "iC6") {
      reactive_filt_data = reactive_filt_data %>% dplyr::filter(IC6 == 1)
    } else if (newpred_iC10_filter == "iC7") {
      reactive_filt_data = reactive_filt_data %>% dplyr::filter(IC7 == 1)
    } else if (newpred_iC10_filter == "iC8") {
      reactive_filt_data = reactive_filt_data %>% dplyr::filter(IC8 == 1)
    } else if (newpred_iC10_filter == "iC9") {
      reactive_filt_data = reactive_filt_data %>% dplyr::filter(IC9 == 1)
    } else if (newpred_iC10_filter == "iC10") {
      reactive_filt_data = reactive_filt_data %>% dplyr::filter(IC10 == 1)
    }
    
    # Warning if no data left
    if (nrow(reactive_filt_data) == 0) {
      showModal(modalDialog(
        title = "Warning",
        "No samples in the dataset satisfy all selected filters.",
        easyClose = TRUE
      ))
    }
  }
  
  reactive_filt_data
})

# Make the prediction (+/- produce ROC plot) ###
predict_new_data <- reactive({
  # Unique sample predictions
  if (import_data_type() %in% c("Random sample",
                                "Import unique sample (genes only)",
                                "Import unique sample (pre-annotated)")) {
    # Make prediction
    prediction = predict(ML$`Decision Trees`$`C5.0 - ROC`, 
                         imported_data(), 
                         type = "prob")
    resp_prob = paste0(round(100*as.numeric(prediction["Responder"]), 2), "%")
    non_resp_prob = paste0(round(100*as.numeric(prediction["Non_responder"]), 2), "%")
    
    # Downloadable content
    newdata = cbind(imported_data(), cbind(resp_prob, non_resp_prob))
    names(newdata[, c(ncol(newdata)-1, ncol(newdata))]) = c("Response chance %", "No response chance %")
    
    chosen_treatment = ifelse(imported_data()[, "Endo"] == 1, "endocrine treatment",
                              "chemotherapy")
    showModal(modalDialog(
      title = "Result",
      paste0("The patient has a ", resp_prob, " chance of responding to the chosen
              treatment type (", chosen_treatment, ")."),
      downloadButton('download_new_prediction_results', 'Download results'),
      easyClose = TRUE
    ))
    
    return(list(predictions = newdata, plot = NULL, auc = NULL))
    
  } else if (import_data_type() == "Import unique sample (pre-annotated)") {
    
    # Make prediction
    prediction = predict(ML$`Decision Trees`$`C5.0 - ROC`, 
                         reactive_filt_data(), 
                         type = "prob")
    resp_prob = paste0(round(100*as.numeric(prediction[, "Responder"]), 2), "%")
    non_resp_prob = paste0(round(100*as.numeric(prediction[, "Non_responder"]), 2), "%")
    
    # Downloadable content
    newdata = cbind(reactive_filt_data(), cbind(resp_prob, non_resp_prob))
    names(newdata[, c(ncol(newdata)-1, ncol(newdata))]) = c("Response chance %", "No response chance %")
    
    if (test_passed) {
      join = newdata
      colnames(join)[c(ncol(newdata)-1, ncol(newdata))] = c("Responder", "Non_responder")
      join = join[order(join$Responder),]
      model_roc = roc(predictor = join$Responder, 
                      response = as.character(join$Response))
      auc_value = round(auc(model_roc), 3)
      p = plot(model_roc, 
               main = "ROC curve",
               col = "#2A5674", lwd = 3, legacy.axes = TRUE, xlim = c(1,0), ylim = c(0,1), 
               asp = 0.92, cex = 4, xaxs = "i", yaxs = "i", width = 900, height = 900)
      
      return(list(predictions = newdata, plot = p, auc = auc_value))
    } else {
      showModal(modalDialog(
        title = "Result",
        "No ROC plot could be generated.",
        downloadButton('download_new_prediction_results', 'Download results'),
        easyClose = TRUE
      ))
      return(list(predictions = newdata, plot = NULL, auc = NULL))
    }
  }
})

# Plot ROC curve
output$newpred_ROC_plot <- renderPlot({
  input$newpred_bc_roc
  isolate({
    p = predict_new_data()$plot
    auc_val = predict_new_data()$auc
    
    p
    # Add legend
    legend(0.5, 0.25, legend=paste0("AUC = ", auc_val),
           col="#2A5674", lty=1, cex=0.8)
  })
})

# Downloadable output
output$download_new_prediction_results <- downloadHandler(
  filename = "my_response_prediction.csv",
  content = function(file) {
    data <- predict_new_data()$predictions
    write.csv(newdata, file, row.names = FALSE)
  }
)

# Reset button
observeEvent(input$reset_new_prediction_breast_cancer, {
  imported_data = NULL
  shinyjs::reset("breast_cancer_new_prediction_type")
  shinyjs::reset("breast_cancer_new_prediction_treatment")
  shinyjs::reset("breast_cancer_new_prediction_pam50_annotation")
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
         "<br> &#8226 ROC plots can be generated if a dataset is imported with a Response column with levels
           'Responder', 'Non_responder'.")
})