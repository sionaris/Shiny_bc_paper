# Plot sunburst function (consensus set)
plot_consensus_sunburst <- reactive({
  
  # Define the subset of studies selected by the user
  subset_breast_consensus_sunburst <- breast_cancer_consensus_set$Dataset %in%
    input$sunburst_consensus_dataset_checkbox
  
  # If the user selected 4 variables
  if(input$sunburst_var1_select_breast_consensus != "None" &&
     input$sunburst_var2_select_breast_consensus != "None" &&
     input$sunburst_var3_select_breast_consensus != "None" &&
     input$sunburst_var4_select_breast_consensus != "None"){
    
    Pheno_sunburst = breast_cancer_consensus_set[subset_breast_consensus_sunburst,] %>% 
      dplyr::select(!!sym(input$sunburst_var1_select_breast_consensus),
                    !!sym(input$sunburst_var2_select_breast_consensus),
                    !!sym(input$sunburst_var3_select_breast_consensus),
                    !!sym(input$sunburst_var4_select_breast_consensus)) %>%
      group_by(!!sym(input$sunburst_var1_select_breast_consensus),
               !!sym(input$sunburst_var2_select_breast_consensus),
               !!sym(input$sunburst_var3_select_breast_consensus),
               !!sym(input$sunburst_var4_select_breast_consensus)) %>%
      summarise(Counts = n()) %>%
      as.data.frame()
    
    
  } else if(input$sunburst_var1_select_breast_consensus != "None" &&
            input$sunburst_var2_select_breast_consensus != "None" &&
            input$sunburst_var3_select_breast_consensus != "None" &&
            input$sunburst_var4_select_breast_consensus == "None") {
    
    # If the user selected 3 variables 
    Pheno_sunburst = breast_cancer_consensus_set[subset_breast_consensus_sunburst,] %>%
      dplyr::select(!!sym(input$sunburst_var1_select_breast_consensus),
                    !!sym(input$sunburst_var2_select_breast_consensus),
                    !!sym(input$sunburst_var3_select_breast_consensus)) %>%
      group_by(!!sym(input$sunburst_var1_select_breast_consensus),
               !!sym(input$sunburst_var2_select_breast_consensus),
               !!sym(input$sunburst_var3_select_breast_consensus)) %>%
      summarise(Counts = n()) %>%
      as.data.frame()
    
    
  } else if(input$sunburst_var1_select_breast_consensus != "None" &&
            input$sunburst_var2_select_breast_consensus != "None" &&
            input$sunburst_var3_select_breast_consensus == "None" &&
            input$sunburst_var4_select_breast_consensus == "None") {
    
    # If the user selected 2 variables
    Pheno_sunburst = breast_cancer_consensus_set[subset_breast_consensus_sunburst,] %>%
      dplyr::select(!!sym(input$sunburst_var1_select_breast_consensus),
                    !!sym(input$sunburst_var2_select_breast_consensus)) %>%
      group_by(!!sym(input$sunburst_var1_select_breast_consensus),
               !!sym(input$sunburst_var2_select_breast_consensus)) %>%
      summarise(Counts = n()) %>%
      as.data.frame()
    
  } else if(input$sunburst_var1_select_breast_consensus != "None" &&
            input$sunburst_var2_select_breast_consensus == "None" &&
            input$sunburst_var3_select_breast_consensus == "None" &&
            input$sunburst_var4_select_breast_consensus == "None") {
    
    # If the user selected 1 variable only
    Pheno_sunburst = breast_cancer_consensus_set[subset_breast_consensus_sunburst,] %>%
      dplyr::select(!!sym(input$sunburst_var1_select_breast_consensus)) %>%
      group_by(!!sym(input$sunburst_var1_select_breast_consensus)) %>%
      summarise(Counts = n()) %>%
      as.data.frame()
  }
  
  # Convert df to sunburst format
  Pheno_sunburst_new = as.sunburstDF(Pheno_sunburst, value_column = "Counts",
                                     add_root = FALSE) %>% 
    inner_join(coloring, by = "labels")
  
  # Plotly
  pie = plot_ly(height = 600) %>%
    add_trace(ids = Pheno_sunburst_new$ids, labels = Pheno_sunburst_new$labels, 
              parents = Pheno_sunburst_new$parents, values = Pheno_sunburst_new$values,
              type = 'sunburst', branchvalues = 'total',
              insidetextorientation = 'radial', maxdepth = ncol(Pheno_sunburst),
              marker = list(colors = Pheno_sunburst_new$colors),
              opacity = input$alpha_breast_sunburst_analytical) %>%
    layout(
      grid = list(columns = 1, rows = 1),
      margin = list(l = 0, r = 0, b = 0, t = 0)
    )
  pie
  
})

# Plot consensus set sunburst
output$consensus_sunburst_breast <- renderPlotly({
  input$draw_breast_sunburst_consensus
  isolate({plot_consensus_sunburst()})
})

# Select all button
observe({
  if(input$select_all_sunburst_consensus_breast > 0){
    if (input$select_all_sunburst_consensus_breast %% 2 == 0) {
      updateCheckboxGroupInput(session, "sunburst_consensus_dataset_checkbox",
                               choices = unique(breast_cancer_full_pheno$Dataset),
                               selected = unique(breast_cancer_full_pheno$Dataset),
                               inline = TRUE)
    }
    else
    {
      updateCheckboxGroupInput(session, "sunburst_consensus_dataset_checkbox",
                               choices = unique(breast_cancer_full_pheno$Dataset),
                               selected = c(),
                               inline = TRUE)
    }
  }
})

# Reset parameters after pressing the corresponding button
observeEvent(input$reset_input_breast_sunburst_consensus, {
  shinyjs::reset("sunburst_var1_select_breast_consensus")
  shinyjs::reset("sunburst_var2_select_breast_consensus")
  shinyjs::reset("sunburst_var3_select_breast_consensus")
  shinyjs::reset("sunburst_var4_select_breast_consensus")
})

# Pop-up info message, triggered when the user presses the Info button
output$consensus_sunburst_breast_info_text <- renderText({
  paste0("<br> &#8226 Select parameters for the sunburst plot.",
         "<br> &#8226 Default parameters are the ones used in the project.",
         "<br> &#8226 You can pick up to <b>four</b> variables for the sunburst.",
         "<br> &#8226 Colors are pre-selected due to the very large number of combinations and levels.")})