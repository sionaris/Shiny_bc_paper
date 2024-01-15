# Server #####
server <- function(input, output, session) {
  ### IMPORTANT ###
  # Wrap everything that's adjusted based on input values in a reactive() expression
  # Then use action Buttons and isolate in the output section, so that
  # the server is not constantly adjusting output for every small input change
  
  # Breast Cancer: histogram #####
  source("current_scripts/exploratory_analysis/histogram.R", local = TRUE)
  
  # Breast Cancer: bar chart #####
  source("current_scripts/exploratory_analysis/bar_chart.R", local = TRUE)
  
  # Breast cancer: sunbursts #####
  source("current_scripts/sunburst/sunburst_setup.R", local = TRUE) # general setup
  source("current_scripts/sunburst/analytical_sunburst.R", local = TRUE) # sunburst for trainval samples
  source("current_scripts/sunburst/consensus_clustering_sunburst.R", local = TRUE) # sunburts for M3C results
  
  # Breast Cancer: volcano plot #####
  source("current_scripts/volcano/volcano_plot.R", local = TRUE)
  
  # Breast Cancer: Machine Learning #####
  source("current_scripts/machine_learning/general_setup.R", local = TRUE)
  source("current_scripts/machine_learning/data_preprocessing.R", local = TRUE)
  source("current_scripts/machine_learning/roc_curve_function.R", local = TRUE)
  source("current_scripts/machine_learning/roc_observers.R", local = TRUE)
  
  # Breast Cancer: Custom DGEA #####
  source("current_scripts/custom_dgea/contrast_observers.R", local = TRUE)
  source("current_scripts/custom_dgea/dgea_main_function.R", local = TRUE)
  source("current_scripts/custom_dgea/dgea_output.R", local = TRUE)
  source("current_scripts/custom_dgea/additional_observers_and_info.R", local = TRUE)
  
  # Breast Cancer: Unique sample predictor #####
  # General setup
  source("current_scripts/interactive_model/general_setup_checks_and_housekeeping.R", local = TRUE)

  # Sample pheno editing:
  source("current_scripts/interactive_model/pheno_editing.R", local = TRUE)
  
  # ROC checks
  source("current_scripts/interactive_model/roc_checks.R", local = TRUE)
  
  # Apply default/selected filters
  source("current_scripts/interactive_model/filters.R", local = TRUE)
  
  # Make prediction and plot (if possible)
  source("current_scripts/interactive_model/predict_and_plot.R", local = TRUE)
  
  # Observers and info
  source("current_scripts/interactive_model/resets_and_info.R", local = TRUE)
}