# Import data type
import_data_type = reactive({ input$breast_cancer_new_prediction_type })

# Disable treatment selection by default
shinyjs::disable("breast_cancer_new_prediction_treatment")

# File input
file_input = reactive({ input$breast_cancer_new_prediction_file_input })

# Housekeeping
all_correct_columns = colnames(full_ml_set)[3:189]
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
    shinyjs::hide("breast_cancer_new_prediction_prespecified_treatment")
    
    # Generate a 166-slot-long vector of normally distributed values
    genes_vector = rnorm(166, mean = 0, sd = 1)
    
    # pam50
    random_pam50_vector = list(c(0, 0, 0, 0),
                               c(0, 0, 0, 1),
                               c(0, 0, 1, 0),
                               c(0, 1, 0, 0),
                               c(1, 0, 0, 0))[[sample(1:5, 1)]]
    
    # timepoint
    random_timepoint_vector = sample(c(0, 1), 1)
    
    # treatment
    random_treatment_vector = sample(c(0, 1), 1)
    
    # iC10
    random_iC10_vector = list(c(0, 0, 0, 0, 0, 0, 0, 0, 0),
                              c(0, 0, 0, 0, 0, 0, 0, 0, 1),
                              c(0, 0, 0, 0, 0, 0, 0, 1, 0),
                              c(0, 0, 0, 0, 0, 0, 1, 0, 0),
                              c(0, 0, 0, 0, 0, 1, 0, 0, 0),
                              c(0, 0, 0, 0, 1, 0, 0, 0, 0),
                              c(0, 0, 0, 1, 0, 0, 0, 0, 0),
                              c(0, 0, 1, 0, 0, 0, 0, 0, 0),
                              c(0, 1, 0, 0, 0, 0, 0, 0, 0),
                              c(1, 0, 0, 0, 0, 0, 0, 0, 0))[[sample(1:10, 1)]]
    
    # Mammaprint
    random_mammaprint_vector = sample(c(0, 1), 1)
    
    # rorS
    random_rorS_vector = list(c(0, 0),
                              c(0, 1),
                              c(1, 0))[[sample(1:3, 1)]]
    
    # scmod1
    random_scmod1_vector = list(c(0, 0, 0),
                                c(0, 0, 1),
                                c(0, 1, 0),
                                c(1, 0, 0))[[sample(1:4, 1)]]
    
    full_random_vector = c(genes_vector, random_pam50_vector, random_timepoint_vector,
                           random_treatment_vector, random_iC10_vector,
                           random_mammaprint_vector, random_rorS_vector,
                           random_scmod1_vector)
    
    # Make reactive
    full_random_vector = as.data.frame(t(full_random_vector))
    colnames(full_random_vector) = colnames(full_ml_set)[3:189]
    imported_data(full_random_vector)
    
    # Verification message
    showModal(
      modalDialog(
        title = "Success!", "Random file generated successfully. You may now edit
          annotation (pam50, scmod1, iC10, timepoint, treatment, Mammaprint, rorS risk) if
          you wish, before generating a prediction.",
        easyClose = TRUE 
      )
    )
    
  } else {
    shinyjs::show("breast_cancer_new_prediction_file_input")
    shinyjs::show("import_new_prediction_breast_cancer")
    shinyjs::show("breast_cancer_new_prediction_prespecified_treatment")
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
    shinyjs::show("breast_cancer_new_prediction_scmod1_filter")
    shinyjs::hide("breast_cancer_new_prediction_pam50_annotation")
    shinyjs::hide("breast_cancer_new_prediction_timepoint_annotation")
    shinyjs::hide("breast_cancer_new_prediction_ic10_annotation")
    shinyjs::hide("breast_cancer_new_prediction_mammaprint_annotation")
    shinyjs::hide("breast_cancer_new_prediction_rors_annotation")
    shinyjs::hide("breast_cancer_new_prediction_scmod1_annotation")
  } else {
    shinyjs::hide("breast_cancer_new_prediction_timepoint_filter")
    shinyjs::hide("breast_cancer_new_prediction_pam50_filter")
    shinyjs::hide("breast_cancer_new_prediction_rorS_filter")
    shinyjs::hide("breast_cancer_new_prediction_Mammaprint_filter")
    shinyjs::hide("breast_cancer_new_prediction_ic10_filter")
    shinyjs::hide("breast_cancer_new_prediction_scmod1_filter")
    shinyjs::show("breast_cancer_new_prediction_pam50_annotation")
    shinyjs::show("breast_cancer_new_prediction_timepoint_annotation")
    shinyjs::show("breast_cancer_new_prediction_ic10_annotation")
    shinyjs::show("breast_cancer_new_prediction_mammaprint_annotation")
    shinyjs::show("breast_cancer_new_prediction_rors_annotation")
    shinyjs::show("breast_cancer_new_prediction_scmod1_annotation")
  }
})

# If import is chosen: Load the file
observeEvent(input$breast_cancer_new_prediction_file_input, {
  req(file_input())
  
  # Read the uploaded file
  if (grepl(".csv$", file_input()$datapath)) {
    input_data = readr::read_csv(file_input()$datapath, show_col_types = FALSE,
                                 col_names = TRUE)
  } else if (grepl(".txt$", file_input()$datapath)) {
    input_data = data.table::fread(file_input()$datapath, header = TRUE)
  } else if (grepl(".xlsx$", file_input()$datapath)) {
    input_data = openxlsx::read.xlsx(file_input()$datapath, colNames = TRUE)
  } else if (grepl(".tsv$", file_input()$datapath)) {
    input_data = readr::read_tsv(file_input()$datapath, show_col_types = FALSE,
                                 col_names = TRUE)
  }
  
  input_data = as.data.frame(input_data)
  print("Data was loaded successfully")
  # Check if file meets all requirements
  if (is.null(input_data)) {
    showModal(
      modalDialog(
        title = "Warning", "The imported file is empty.", easyClose = TRUE 
      )
    )
    return() # exit code for this event
  } else {
    # Assert if gene names are with X_ prefix or not and count gene columns
    if (length(which(grepl("X_", colnames(input_data))) > 1)) {
      X_handle = "on"
      gene_colnames_input_data = intersect(colnames(input_data), gene_columns_with_X)
      X_columns_imported_number = length(gene_colnames_input_data)
    } else {
      X_handle = "off"
      gene_colnames_input_data = intersect(colnames(input_data), gene_columns_without_X)
      nonX_gene_columns_imported_number = length(gene_colnames_input_data)
    }
    
    # Check if they are 166 and correct
    if (X_handle == "on" && X_columns_imported_number < 166) {
      showModal(
        modalDialog(
          title = "Warning", 
          paste0("There are missing markers in the data (assuming format 'X_Entrez'). 166 variables required.",
                 "\n", "Problem with:", 
                 paste(setdiff(gene_columns_with_X, gene_colnames_input_data), collapse = ", ")), easyClose = TRUE 
        )
      )
      return() # exit code for this event
    } else if (X_handle == "off" && nonX_gene_columns_imported_number < 166) {
      showModal(
        modalDialog(
          title = "Warning", 
          paste0("There are missing markers in the data (assuming format: 'Entrez'). 166 variables required.",
                 "\n", "Problem with:", paste(setdiff(gene_columns_without_X, gene_colnames_input_data), collapse = ", ")),
          easyClose = TRUE 
        )
      )
      return() # exit code for this event
    }
  }
  print("Data was checked for gene columns")
  
  # Check the files only contain one row (unique samples)
  if (import_data_type() == "Import unique sample (genes only)" && nrow(input_data) > 1) {
    showModal(
      modalDialog(
        title = "Warning", 
        "There are more than one rows in this sample. Only one-row files permitted.", easyClose = TRUE 
      )
    )
    return() # exit code for this event
  }
  
  if (import_data_type() == "Import unique sample (pre-annotated)" && nrow(input_data) > 1) {
    showModal(
      modalDialog(
        title = "Warning", 
        "There are more than one rows in this sample. Only one-row files permitted.", easyClose = TRUE 
      )
    )
    return() # exit code for this event
  }
  
  if (import_data_type() == "Import pre-annotated dataset" && nrow(input_data) == 1) {
    showModal(
      modalDialog(
        title = "Warning", 
        "There is only one row in this dataset. 
          Please switch import type to a unique sample category.", easyClose = TRUE 
      )
    )
    return() # exit code for this event
  }
  
  print("File was checked for multiple rows")
  # Check the rows are numeric in genes only - unique samples
  if (import_data_type() == "Import unique sample (genes only)") {
    first_row <- input_data
    if (any(!sapply(first_row, is.numeric))) {
      # Attempt to convert non-numeric values to numeric
      converted_row <- suppressWarnings(sapply(first_row, as.numeric))
      if (any(is.na(converted_row))) {
        showModal(modalDialog(
          title = "Warning",
          "The uploaded file contains non-numeric values that could not be converted to numeric.",
          easyClose = TRUE
        ))
        return() # exit code for this event
      } else {
        input_data <- converted_row
      }
    }
  } else if (import_data_type() == "Import unique sample (pre-annotated)" && X_handle == "on") {
    first_row <- input_data[gene_columns_with_X]
    if (any(!sapply(first_row, is.numeric))) {
      # Attempt to convert non-numeric values to numeric
      converted_row <- suppressWarnings(sapply(first_row, as.numeric))
      if (any(is.na(converted_row))) {
        showModal(modalDialog(
          title = "Warning",
          "The uploaded file contains non-numeric values in the gene variables
              that could not be converted to numeric.",
          easyClose = TRUE
        ))
        return() # exit code for this event
      } else {
        input_data[gene_columns_with_X] <- converted_row
      }
    }
  } else if (import_data_type() == "Import unique sample (pre-annotated)" && X_handle == "off") {
    first_row <- input_data[gene_columns_without_X]
    if (any(!sapply(first_row, is.numeric))) {
      # Attempt to convert non-numeric values to numeric
      converted_row <- suppressWarnings(sapply(first_row, as.numeric))
      if (any(is.na(converted_row))) {
        showModal(modalDialog(
          title = "Warning",
          "The uploaded file contains non-numeric values in the gene variables
              that could not be converted to numeric.",
          easyClose = TRUE
        ))
        return() # exit code for this event
      } else {
        input_data[gene_columns_without_X] <- converted_row
      }
    }
  } else if (import_data_type() == "Import pre-annotated dataset" && X_handle == "on") {
    if (any(!sapply(input_data[, gene_columns_with_X], is.numeric))) {
      # Attempt to convert non-numeric values to numeric
      converted_data <- suppressWarnings(as.data.frame(lapply(input_data[, gene_columns_with_X], as.numeric)))
      if (any(is.na(converted_data))) {
        showModal(modalDialog(
          title = "Warning",
          "The uploaded file contains non-numeric values that could not be converted to numeric.",
          easyClose = TRUE
        ))
        return() # exit code for this event
      } else {
        input_data[, gene_columns_with_X] <- converted_data
      }
    }
  } else if (import_data_type() == "Import pre-annotated dataset" && X_handle == "off") {
    if (any(!sapply(input_data[, gene_columns_without_X], is.numeric))) {
      # Attempt to convert non-numeric values to numeric
      converted_data <- suppressWarnings(as.data.frame(lapply(input_data[, gene_columns_without_X], as.numeric)))
      if (any(is.na(converted_data))) {
        showModal(modalDialog(
          title = "Warning",
          "The uploaded file contains non-numeric values that could not be converted to numeric.",
          easyClose = TRUE
        ))
        return() # exit code for this event
      } else {
        input_data[, gene_columns_without_X] <- converted_data
      }
    }
  }
  
  print("Numeric column checks complete")
  # If genes and annotation is chosen check all columns needed are present
  if (import_data_type() %in% c("Import unique sample (pre-annotated)",
                                "Import pre-annotated dataset") && X_handle == "on") {
    if (length(intersect(colnames(input_data), all_correct_columns)) < 183) {
      showModal(
        modalDialog(
          title = "Warning", 
          paste0("There are missing columns in the data.",
                 "\n", "Problem with:", 
                 paste(setdiff(all_correct_columns, colnames(input_data)), collapse = ", ")), 
          easyClose = TRUE 
        )
      )
      return() # exit code for this event
    }
  } else if (import_data_type() %in% c("Import unique sample (pre-annotated)",
                                       "Import pre-annotated dataset") && X_handle == "off") {
    if (length(intersect(colnames(input_data), all_correct_columns_without_X)) < 183) {
      showModal(
        modalDialog(
          title = "Warning", 
          paste0("There are missing columns in the data.",
                 "\n", "Problem with:", 
                 paste(setdiff(all_correct_columns_without_X, colnames(input_data)), collapse = ", ")), 
          easyClose = TRUE 
        )
      )
      return() # exit code for this event
    }
  }
  
  print("Missing genes checked successfully")
  # Make sure all other columns have 0 or 1 and are converted to factors
  if (import_data_type() == "Import unique sample (pre-annotated)") {
    if (!all(input_data[pheno_columns] %in% c(0, 1))) {
      showModal(
        modalDialog(
          title = "Warning", 
          "All values in the phenotypic column should be either 0 or 1.", easyClose = TRUE 
        )
      )
      return() # exit code for this event
    }
  } else if (import_data_type() == "Import pre-annotated dataset") {
    if (!all(apply(input_data[, pheno_columns], 2, function(col) all(col %in% c(0, 1))))) {
      showModal(
        modalDialog(
          title = "Warning", 
          "All values in the phenotypic column should be either 0 or 1.", easyClose = TRUE 
        )
      )
      return() # exit code for this event
    }
  }
  
  print("Factor checks successful")
  # Convert pheno columns to factors
  if (import_data_type() == "Import unique sample (pre-annotated)") {
    first_row <- input_data[pheno_columns]
    if (any(!sapply(first_row, is.factor))) {
      # Attempt to convert to factors
      converted_row <- suppressWarnings(factor(first_row, levels = c(0, 1), labels = c(0, 1)))
      if (any(is.na(converted_row))) {
        showModal(modalDialog(
          title = "Warning",
          "The uploaded file contains values that could not be converted to factors.",
          easyClose = TRUE
        ))
        return() # exit code for this event
      } else {
        input_data[pheno_columns] <- converted_row
      }
    }
  } else if (import_data_type() == "Import pre-annotated dataset") {
    if (any(!sapply(input_data[, pheno_columns], is.factor))) {
      # Attempt to convert non-numeric values to numeric
      converted_data <- suppressWarnings(
        as.data.frame(lapply(
          input_data[, pheno_columns], 
          function(x) factor(x, levels = c(0, 1), labels = c(0, 1)))))
      if (any(is.na(converted_data))) {
        showModal(modalDialog(
          title = "Warning",
          "The uploaded file contains values that could not be converted to factors.",
          easyClose = TRUE
        ))
        return() # exit code for this event
      } else {
        input_data[, pheno_columns] <- converted_data
      }
    }
  }
  
  print("Factor conversions successful")
  
  # Convert to reactive
  imported_data(as.data.frame(input_data))
  
  # If all those checks passed and X_handle == "off", convert the Entrez ID's
  # to X_Entrez format
  if (X_handle == "off") {
    current_data = imported_data()
    cols_to_rename <- gene_columns_without_X
    new_colnames <- gene_columns_with_X
    colnames(current_data)[which(names(current_data) %in% cols_to_rename)] <- new_colnames
    imported_data(current_data)
  }
  
  # Treatment
  treatment_toggle = reactive({ input$breast_cancer_new_prediction_prespecified_treatment })
  observeEvent(treatment_toggle(), {
    if (!is.null(imported_data()) && treatment_toggle() == "No") {
      shinyjs::enable("breast_cancer_new_prediction_treatment")
      desired_treatment = reactive({ input$breast_cancer_new_prediction_treatment })
      
      if (desired_treatment() == "Chemotherapy" && "Endo" %in% colnames(imported_data())) {
        current_data <- imported_data()
        current_data[["Endo"]] <- 0
        current_data$Endo = factor(current_data$Endo, levels = c(0, 1), labels = c(0, 1))
        imported_data(current_data)
      } else if (desired_treatment() == "Chemotherapy" && !"Endo" %in% colnames(imported_data())) {
        current_data <- imported_data()
        current_data$Endo = 0
        current_data$Endo = factor(current_data$Endo, levels = c(0, 1), labels = c(0, 1))
        imported_data(current_data)
      } else if (desired_treatment() == "Endocrine treatment" && "Endo" %in% colnames(imported_data())) {
        current_data <- imported_data()
        current_data[["Endo"]] <- 1
        current_data$Endo = factor(current_data$Endo, levels = c(0, 1), labels = c(0, 1))
        imported_data(current_data)
      } else if (desired_treatment() == "Endocrine treatment" && !"Endo" %in% colnames(imported_data())) {
        current_data <- imported_data()
        current_data$Endo = 1
        current_data$Endo = factor(current_data$Endo, levels = c(0, 1), labels = c(0, 1))
        imported_data(current_data)
      }
    } else if(!is.null(imported_data()) && treatment_toggle() == "Yes")  {
      shinyjs::disable("breast_cancer_new_prediction_treatment")
      current_data = imported_data()
      current_data$Endo = factor(current_data$Endo, levels = c(0, 1), labels = c(0, 1))
      imported_data(current_data)
    }
  })
  print("We passed treatment")
})
print("We passed the first check")