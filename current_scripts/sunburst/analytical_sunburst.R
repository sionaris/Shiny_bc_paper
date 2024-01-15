# Plot sunburst function (analytical set)
plot_analytical_sunburst <- reactive({
  
  # Define the subset of studies selected by the user
  subset_breast_analytical_sunburst <- breast_cancer_full_pheno$Dataset %in%
    input$sunburst_analytical_dataset_checkbox
  
  # If the user selected 4 variables
  if(input$sunburst_var1_select_breast != "None" &&
     input$sunburst_var2_select_breast != "None" &&
     input$sunburst_var3_select_breast != "None" &&
     input$sunburst_var4_select_breast != "None"){
    
    Pheno_sunburst = breast_cancer_full_pheno[subset_breast_analytical_sunburst,] %>% 
      dplyr::select(!!sym(input$sunburst_var1_select_breast),
                    !!sym(input$sunburst_var2_select_breast),
                    !!sym(input$sunburst_var3_select_breast),
                    !!sym(input$sunburst_var4_select_breast)) %>%
      group_by(!!sym(input$sunburst_var1_select_breast),
               !!sym(input$sunburst_var2_select_breast),
               !!sym(input$sunburst_var3_select_breast),
               !!sym(input$sunburst_var4_select_breast)) %>%
      summarise(Counts = n()) %>%
      as.data.frame()
    
    
  } else if(input$sunburst_var1_select_breast != "None" &&
            input$sunburst_var2_select_breast != "None" &&
            input$sunburst_var3_select_breast != "None" &&
            input$sunburst_var4_select_breast == "None") {
    
    # If the user selected 3 variables 
    Pheno_sunburst = breast_cancer_full_pheno[subset_breast_analytical_sunburst,] %>%
      dplyr::select(!!sym(input$sunburst_var1_select_breast),
                    !!sym(input$sunburst_var2_select_breast),
                    !!sym(input$sunburst_var3_select_breast)) %>%
      group_by(!!sym(input$sunburst_var1_select_breast),
               !!sym(input$sunburst_var2_select_breast),
               !!sym(input$sunburst_var3_select_breast)) %>%
      summarise(Counts = n()) %>%
      as.data.frame()
    
    
  } else if(input$sunburst_var1_select_breast != "None" &&
            input$sunburst_var2_select_breast != "None" &&
            input$sunburst_var3_select_breast == "None" &&
            input$sunburst_var4_select_breast == "None") {
    
    # If the user selected 2 variables
    Pheno_sunburst = breast_cancer_full_pheno[subset_breast_analytical_sunburst,] %>%
      dplyr::select(!!sym(input$sunburst_var1_select_breast),
                    !!sym(input$sunburst_var2_select_breast)) %>%
      group_by(!!sym(input$sunburst_var1_select_breast),
               !!sym(input$sunburst_var2_select_breast)) %>%
      summarise(Counts = n()) %>%
      as.data.frame()
    
  } else if(input$sunburst_var1_select_breast != "None" &&
            input$sunburst_var2_select_breast == "None" &&
            input$sunburst_var3_select_breast == "None" &&
            input$sunburst_var4_select_breast == "None") {
    
    # If the user selected 1 variable only
    Pheno_sunburst = breast_cancer_full_pheno[subset_breast_analytical_sunburst,] %>%
      dplyr::select(!!sym(input$sunburst_var1_select_breast)) %>%
      group_by(!!sym(input$sunburst_var1_select_breast)) %>%
      summarise(Counts = n()) %>%
      as.data.frame()
  }
  
  # Convert df to sunburst format
  Pheno_sunburst_new = as.sunburstDF(Pheno_sunburst, value_column = "Counts",
                                     add_root = FALSE) %>% 
    inner_join(coloring, by = "labels")
  
  # Plotly
  pie = plot_ly(height  = 600) %>%
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

# Plot analytical set sunburst
output$analytical_sunburst_breast <- renderPlotly({
  input$draw_breast_sunburst_analytical
  isolate({plot_analytical_sunburst()})
})

# Select all button
observe({
  if(input$select_all_sunburst_analytical_breast > 0){
    if (input$select_all_sunburst_analytical_breast %% 2 == 0) {
      updateCheckboxGroupInput(session, "sunburst_analytical_dataset_checkbox",
                               choices = unique(breast_cancer_full_pheno$Dataset),
                               selected = unique(breast_cancer_full_pheno$Dataset),
                               inline = TRUE)
    }
    else
    {
      updateCheckboxGroupInput(session, "sunburst_analytical_dataset_checkbox",
                               choices = unique(breast_cancer_full_pheno$Dataset),
                               selected = c(),
                               inline = TRUE)
    }
  }
})

# Reset parameters after pressing the corresponding button
observeEvent(input$reset_input_breast_sunburst_analytical, {
  shinyjs::reset("sunburst_var1_select_breast")
  shinyjs::reset("sunburst_var2_select_breast")
  shinyjs::reset("sunburst_var3_select_breast")
  shinyjs::reset("sunburst_var4_select_breast")
})

# Pop-up info message, triggered when the user presses the Info button
output$analytical_sunburst_breast_info_text <- renderText({
  paste0("<br> &#8226 Select parameters for the sunburst plot.",
         "<br> &#8226 Default parameters are the ones used in the project.",
         "<br> &#8226 You can pick up to <b>four</b> variables for the sunburst.",
         "<br> &#8226 Colors are pre-selected due to the very large number of combinations and levels.")})