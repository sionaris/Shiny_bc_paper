# Server #####
server <- function(input, output, session) {
  ### IMPORTANT ###
  # Wrap everything that's adjusted based on input values in a reactive() expression
  # Then use action Buttons and isolate in the output section, so that
  # the server is not constantly adjusting output for every small input change
  
  # Breast Cancer: histogram #####
  # Enable Select/De-select all button
  observe({
    if(input$select_all_hist_breast > 0){
      if (input$select_all_hist_breast %% 2 == 0) {
        updateCheckboxGroupInput(session, "hist_dataset_checkbox",
                                 choices = unique(breast_cancer_full_pheno$Dataset),
                                 selected = unique(breast_cancer_full_pheno$Dataset),
                                 inline = TRUE)
      }
      else
      {
        updateCheckboxGroupInput(session, "hist_dataset_checkbox",
                                 choices = unique(breast_cancer_full_pheno$Dataset),
                                 selected = c(),
                                 inline = TRUE)
      }
    }
  })
  
  prepare_histogram <- reactive({
    # Dynamic control of histogram types
    hist_types_breast = list(classic = "classic", probability = "probability",
                             percent = "percentage")
    y_axis_titles_breast = list(classic = "Counts", probability = "Probability",
                                percentage = "Percentage")
    subset_breast_hist = breast_cancer_full_pheno$Dataset %in%
      input$hist_dataset_checkbox
    
    # Plot
    plot_ly(data = breast_cancer_full_pheno[subset_breast_hist,], 
            x = breast_cancer_full_pheno[subset_breast_hist,input$histvar_select_breast],
            type = "histogram", 
            histnorm = names(hist_types_breast[which(hist_types_breast %in% input$hist_type_breast)]),
            marker = list(color = input$hist_fill_breast,
                          line = list(color = input$hist_color_breast, width = 0.5)),
            autobinx = FALSE, nbinsx = ~input$hist_breast_bins*2, height = 300) %>%
      layout(title = list(text = paste0("<b>Histogram of ", input$histvar_select_breast),
                          x = 0, y = 0.99, font = list(size  = 10)),
             annotations = list(x = -0.098, y = 1.065, xref = "paper", yref = "paper",
                                showarrow = F,
                                text = paste0("Studies: ",
                                              c(list(input$hist_dataset_checkbox))),
                                font = list(size = 8)),
             xaxis = list(title = list(text = paste0("<b>", input$histvar_select_breast)),
                          linecolor = '#000000',
                          linewidth = 2),
             yaxis = list(title = list(text = y_axis_titles_breast[input$hist_type_breast][[1]]),
                          linecolor = '#000000',
                          linewidth = 2))
  })
  
  
  # Plot the breast cancer histogram after the user pressed the "Draw!" button
  output$breast_histogram <- renderPlotly({
    input$draw_breast_hist 
    isolate({prepare_histogram()})
  })
  
  # Reset parameters after pressing the corresponding button
  observeEvent(input$reset_input_breast_hist, {
    shinyjs::reset("Histogram_tuning_breast") 
  })
  
  # Pop-up info message, triggered when the user presses the Info button
  output$hist_breast_info_text <- renderText({
    paste0("<br> &#8226 Select variables and parameters to plot in a histogram.",
           "<br> &#8226You can toggle the histogram variable, type, color, studies etc.", 
           "<br> &#8226Number of bins is only for continuous variables.")})
  
  
  # Breast Cancer: bar chart #####
  
  # Observe secondary variable
  observe({
    if(input$barchart_varclust_select_breast == "None"){
      disable("barchart_type_breast")
    } else {
      enable("barchart_type_breast")
    }
  })
  
  prepare_barchart <- reactive({
    
    # Define the subset of studies selected by the user
    subset_breast_barchart <- breast_cancer_full_pheno$Dataset %in%
      input$barchart_dataset_checkbox
    
    # Define the variables that will be plotted
    if(input$barchart_varclust_select_breast != "None"){
      cols = c(input$barchart_varmain_select_breast, input$barchart_varclust_select_breast)
    } else {
      cols = input$barchart_varmain_select_breast
    }
    
    # Prepare data for plotly bar chart
    data_breast_bar = as.data.frame(breast_cancer_full_pheno[subset_breast_barchart,
                                                             c(cols)])
    colnames(data_breast_bar) = cols
    
    if(length(cols) == 1){
      data_breast_bar = data_breast_bar %>% 
        count(., !!sym(input$barchart_varmain_select_breast))
      str_sub(colnames(data_breast_bar)[-1], start = 0, end = 2) = ""
    } else {
      data_breast_bar = data_breast_bar %>% 
        count(., !!sym(input$barchart_varmain_select_breast),
              !!sym(input$barchart_varclust_select_breast)) %>%
        reshape(., idvar = input$barchart_varmain_select_breast,
                timevar = input$barchart_varclust_select_breast, direction = "wide")
      str_sub(colnames(data_breast_bar)[-1], start = 0, end = 2) = ""
    }
    
    # Bar chart
    p = plot_ly(data = data_breast_bar, type = "bar", 
                x = data_breast_bar[,input$barchart_varmain_select_breast], 
                y = data_breast_bar[,2], 
                marker = list(color = input$barchart_fill_breast,
                              line = list(color = input$barchart_color_breast, width = 0.5)),
                height = 300, name = colnames(data_breast_bar)[2])
    
    
    # Add traces if the user chooses to have a second variable  
    if(input$barchart_varclust_select_breast != "None"){
      for (i in 3:ncol(data_breast_bar)){
        p <- p %>%
          add_trace(y = data_breast_bar[,i], name = colnames(data_breast_bar)[i],
                    marker = list(color = randomColor(),
                                  line = list(color = "black", width = 0.5)))
      }
      
      # Add layout
      p <- p %>%
        layout(title = list(text = paste0("<b>Bar chart of ", input$barchart_varmain_select_breast),
                            x = 0, y = 0.99, font = list(size  = 10)),
               annotations = list(x = -0.098, y = 1.065, xref = "paper", yref = "paper",
                                  showarrow = F,
                                  text = paste0("Studies: ",
                                                c(list(input$hist_dataset_checkbox))),
                                  font = list(size = 8)),
               xaxis = list(title = list(text = paste0("<b>", input$barchart_varmain_select_breast)),
                            linecolor = '#000000',
                            linewidth = 2),
               yaxis = list(title = list(text = "Counts"),
                            linecolor = '#000000',
                            linewidth = 2),
               barmode = input$barchart_type_breast)
    } else {
      p <- p %>%
        layout(title = list(text = paste0("<b>Bar chart of ", input$barchart_varmain_select_breast),
                            x = 0, y = 0.99, font = list(size  = 10)),
               annotations = list(x = -0.098, y = 1.065, xref = "paper", yref = "paper",
                                  showarrow = F,
                                  text = paste0("Studies: ",
                                                c(list(input$hist_dataset_checkbox))),
                                  font = list(size = 8)),
               xaxis = list(title = list(text = paste0("<b>", input$barchart_varmain_select_breast)),
                            linecolor = '#000000',
                            linewidth = 2),
               yaxis = list(title = list(text = "Counts"),
                            linecolor = '#000000',
                            linewidth = 2))
    }
    
    p
  })
  
  # Plot the breast cancer bar chart after the user pressed the "Draw!" button
  output$breast_barchart <- renderPlotly({
    input$draw_breast_barchart
    isolate({prepare_barchart()})
  })
  
  # Select all button
  observe({
    if(input$select_all_barchart_breast > 0){
      if (input$select_all_barchart_breast %% 2 == 0) {
        updateCheckboxGroupInput(session, "barchart_dataset_checkbox",
                                 choices = unique(breast_cancer_full_pheno$Dataset),
                                 selected = unique(breast_cancer_full_pheno$Dataset),
                                 inline = TRUE)
      }
      else
      {
        updateCheckboxGroupInput(session, "barchart_dataset_checkbox",
                                 choices = unique(breast_cancer_full_pheno$Dataset),
                                 selected = c(),
                                 inline = TRUE)
      }
    }
  })
  
  # Reset parameters after pressing the corresponding button
  observeEvent(input$reset_input_breast_barchart, {
    shinyjs::reset("Barchart_tuning_breast") 
  })
  
  # Pop-up info message, triggered when the user presses the Info button
  output$barchart_breast_info_text <- renderText({
    paste0("<br> &#8226 Select variables and parameters to plot in a bar chart.",
           "<br> &#8226 You can only select <b>categorical variables</b>.",
           "<br> &#8226 You can also do clustered bar charts by selecting and additional variable.",
           "<br> &#8226 <b>Color can only be selected for single variable plots</b>.",
           "<br> When using two variables, level colors will be generated automatically.")})
  
  # Breast cancer: sunbursts #####
  
  # Define a wrapper function to create sunburst input in the appropriate format
  as.sunburstDF <- function(DF, value_column = NULL, add_root = FALSE){
    require(data.table)
    
    colNamesDF <- names(DF)
    
    if(is.data.table(DF)){
      DT <- copy(DF)
    } else {
      DT <- data.table(DF, stringsAsFactors = FALSE)
    }
    
    if(add_root){
      DT[, root := "Total"]  
    }
    
    colNamesDT <- names(DT)
    hierarchy_columns <- setdiff(colNamesDT, value_column)
    DT[, (hierarchy_columns) := lapply(.SD, as.factor), .SDcols = hierarchy_columns]
    
    if(is.null(value_column) && add_root){
      setcolorder(DT, c("root", colNamesDF))
    } else if(!is.null(value_column) && !add_root) {
      setnames(DT, value_column, "values", skip_absent=TRUE)
      setcolorder(DT, c(setdiff(colNamesDF, value_column), "values"))
    } else if(!is.null(value_column) && add_root) {
      setnames(DT, value_column, "values", skip_absent=TRUE)
      setcolorder(DT, c("root", setdiff(colNamesDF, value_column), "values"))
    }
    
    hierarchyList <- list()
    
    for(i in seq_along(hierarchy_columns)){
      current_columns <- colNamesDT[1:i]
      if(is.null(value_column)){
        currentDT <- unique(DT[, ..current_columns][, values := .N, by = current_columns], by = current_columns)
      } else {
        currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=current_columns, .SDcols = "values"]
      }
      setnames(currentDT, length(current_columns), "labels")
      hierarchyList[[i]] <- currentDT
    }
    
    hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
    
    parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", value_column))
    hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parent_columns]
    hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
    hierarchyDT[, c(parent_columns) := NULL]
    return(hierarchyDT)
  }
  
  # Set up color palette for the sunburst (un-customisable by the user)
  coloring = data.frame(stringsAsFactors = FALSE,
                        colors = tolower(gplots::col2hex(c("paleturquoise3", "sienna2", 
                                                           "paleturquoise1", "paleturquoise1", "paleturquoise1", 
                                                           "sienna1", "sienna1", "sienna1", "sienna1",
                                                           "sienna1", "sienna1", "sienna1", 
                                                           "paleturquoise1", "paleturquoise1", "sienna1", "paleturquoise1",
                                                           "deeppink4", "dodgerblue4", 
                                                           "red4", "violet", "darkblue", "skyblue", "lightgreen",
                                                           "goldenrod2", "firebrick2", 
                                                           "red4", "orange", "chartreuse4",
                                                           "red3", "green",
                                                           "grey", "goldenrod2",
                                                           "violet", "red4", "skyblue", "dodgerblue4",
                                                           "dodgerblue4", "chartreuse4", "violet", 
                                                           "orange", "dodgerblue3", # orange is used for ER/Meno mixed status
                                                           "dodgerblue4",
                                                           "orange", "skyblue"))),
                        labels = c("Chemotherapy", "Endocrine_treatment", 
                                   "C1", "C2", "C3", 
                                   "E1_1", "E1_2", "E1_3", "E2", 
                                   "E3", "E4_1", "E4_2",
                                   "XVC1", "XVC2", "XVE", "SJS",
                                   "Non_responder", "Responder", 
                                   "Basal", "Her2", "LumA", "LumB", "Normal",
                                   "T1", "T2",
                                   "High", "Intermediate", "Low",
                                   "Risk", "No risk",
                                   "Others", "Claudin",
                                   "HER2+", "ER-/HER2-", "ER+/HER2- High Prolif", "ER+/HER2- Low Prolif",
                                   "UK", "USA", "Korea",
                                   "Mixed", "PM", # Mixed is used for both ER and Meno status here
                                   "ER+",
                                   "Cluster 1", "Cluster 2"))
  
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
  
  # Breast Cancer: volcano plot #####
  prepare_volcano <- reactive({
    # Some useful functions for the volcano
    # Vertical line for plotly
    vline <- function(x = 0, color = "black") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = "dot")
      )
    }
    
    # Horizontal line for plotly
    hline <- function(y = 0, color = "black") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list(color = color, dash = "dot")
      )
    }  
    
    # Create custom variable to aid in color coding and legend labels later
    breast_cancer_DGEA$volc_p_status = NA
    breast_cancer_DGEA$volc_p_status[breast_cancer_DGEA$adj.P.Val>input$pval_breast &
                                       abs(breast_cancer_DGEA$logFC)<input$logFC_breast] = "Not Significant"
    breast_cancer_DGEA$volc_p_status[breast_cancer_DGEA$adj.P.Val<input$pval_breast &
                                       abs(breast_cancer_DGEA$logFC)<input$logFC_breast] = 
      paste0("|DE|<", input$logFC_breast," s.d. & p<", input$pval_breast)
    breast_cancer_DGEA$volc_p_status[breast_cancer_DGEA$adj.P.Val<input$pval_breast &
                                       breast_cancer_DGEA$logFC<input$logFC_breast*-1] = 
      paste0("DE<-", input$logFC_breast," s.d. & p<", input$pval_breast)
    breast_cancer_DGEA$volc_p_status[breast_cancer_DGEA$adj.P.Val<input$pval_breast &
                                       abs(breast_cancer_DGEA$logFC)>input$logFC_breast] = 
      paste0("DE>", input$logFC_breast," s.d. & p<", input$pval_breast)
    breast_cancer_DGEA$volc_p_status = factor(breast_cancer_DGEA$volc_p_status,
                                              levels = c("Not Significant", 
                                                         paste0("|DE|<", input$logFC_breast," s.d. & p<", input$pval_breast),
                                                         paste0("DE<-", input$logFC_breast," s.d. & p<", input$pval_breast), 
                                                         paste0("DE>", input$logFC_breast," s.d. & p<", input$pval_breast)),
                                              labels = c("Not Significant", 
                                                         paste0("|DE|<", input$logFC_breast," s.d. & p<", input$pval_breast),
                                                         paste0("DE<-", input$logFC_breast," s.d. & p<", input$pval_breast), 
                                                         paste0("DE>", input$logFC_breast," s.d. & p<", input$pval_breast)))
    
    # Create the palette for the scatter colors
    pal = c(input$scatter_color_breast_NS, 
            input$scatter_color_breast_p_only, 
            input$scatter_color_breast_downr, 
            input$scatter_color_breast_upr)
    pal = setNames(pal, levels(breast_cancer_DGEA$volc_p_status))
    
    # Main plotly function call
    plot_ly(data = breast_cancer_DGEA, 
            x = ~logFC, 
            y = ~-log10(breast_cancer_DGEA[,input$pval_selector_breast]),
            color = ~volc_p_status, 
            marker = list(size = 5), 
            type = "scatter",
            mode = "markers",
            colors = pal, opacity = input$alpha_breast_volcano,
            hoverinfo = "text",
            hovertext = paste("</br> Gene Symbol:", breast_cancer_DGEA$Gene.Symbol,
                              "</br> p-value:", breast_cancer_DGEA$P.Value,
                              "</br> adj.P.Val:", breast_cancer_DGEA$adj.P.Val,
                              "</br> Diff.Exp:", round(breast_cancer_DGEA$logFC,3)),
            width = 880, height = 730) %>%
      
      # Layout with titles and drawn lines
      layout(title = list(text = paste0("<b>", input$breast_volcano_title),
                          x = 0, y = 0.99),
             shapes = list(
               vline(input$logFC_breast), vline(-input$logFC_breast), # vertical lines
               hline(-log10(input$pval_breast))), # horizontal line
             xaxis = list(title = list(text = "<b>Differential Expression (units:sd)"),
                          range = c(-(max(abs(breast_cancer_DGEA$logFC))+0.1*max(abs(breast_cancer_DGEA$logFC))), 
                                    (max(abs(breast_cancer_DGEA$logFC))+0.1*max(abs(breast_cancer_DGEA$logFC)))),
                          linecolor = '#000000',
                          linewidth = 2),
             yaxis = list(title = list(text = "<b>-log10(p)"),
                          range = c(0, (max(-log10(breast_cancer_DGEA[,input$pval_selector_breast]))+1)),
                          linecolor = '#000000',
                          linewidth = 2)) %>%
      
      # Labels for gene symbols. Toggled when clicking on individual points
      add_annotations(visible = FALSE,
                      x = breast_cancer_DGEA$logFC, 
                      y = -log10(breast_cancer_DGEA[,input$pval_selector_breast]),
                      text = breast_cancer_DGEA$Gene.Symbol,
                      showarrow = TRUE,
                      arrowhead = 0,
                      clicktoshow = "onoff")
  })
  
  
  # Plot the breast cancer volcano after the user pressed the "Draw!" button
  output$breast_volcano <- renderPlotly({
    input$draw_breast_volcano 
    isolate({prepare_volcano()})
  })
  
  # Reset parameters after pressing the corresponding button
  observeEvent(input$reset_input_breast_volcano, {
    shinyjs::reset("Volcano_tuning_breast") 
  })
  
  # Pop-up info message, triggered when the user presses the Info button
  output$volcano_breast_info_text <- renderText({
    paste0("<br> &#8226 Select parameters for the volcano plot.",
           "<br> &#8226 Default parameters are the ones used in the project.",
           "<br> &#8226 You can overlay gene symbols by clicking on individual data points.",
           "<br> &#8226 The x-axis shows the numerical difference between the mean expression in group 1 (responders) and group 2 (non-responders) measured in standard deviations from the gene's overall mean expression.")})
  
  # Breast Cancer: Machine Learning #####
  # A function for dynamic conditional selectInput for the ML model subcategory
  
  update_subcategory_radios <- function(model_num) {
    observeEvent(input[[paste0("breast_cancer_ml_model_category_", model_num)]], {
      category_input <- input[[paste0("breast_cancer_ml_model_category_", model_num)]]
      subcategory_id <- paste0("breast_cancer_ml_model_subcategory_", model_num)
      
      if(category_input == "Logistic Regression"){
        updateRadioButtons(inputId = subcategory_id,
                           choices = c("Backward", "Lasso-regularised"),
                           selected = "Lasso-regularised")
      } else if(category_input == "Decision Trees"){
        updateRadioButtons(inputId = subcategory_id,
                           choices = sort(c(`C5.0 (tuned with Cohen's k)` = "C5.0 - k",
                                            `Bagging (100 iterations)` = "100X Bagging",
                                            `AdaBoost` = "Boosting",
                                            `Random Forest (tuned with Cohen's k)` = "RForest - k",
                                            `Random Forest (tuned with ROC)` = "RForest - ROC",
                                            `C5.0 (tuned with ROC)` = "C5.0 - ROC")),
                           selected = "C5.0 - ROC")
      } else if(category_input == "Support Vector Machines"){
        updateRadioButtons(inputId = subcategory_id,
                           choices = sort(c(`Linear kernel` = "Linear",
                                            `L2-regularised linear kernel` = "L2 Linear",
                                            `Radial Basis Function (RBF) kernel` = "RBF")),
                           selected = "RBF")
      }
    })
  }
  
  # Call the function for each model
  update_subcategory_radios(1)
  update_subcategory_radios(2)
  update_subcategory_radios(3)
  
  # Plot counter
  counter = reactiveValues(counter = 1)
  
  # Hide second model selection box
  observeEvent(input$multiple_rocs_breast, {
    if(input$multiple_rocs_breast %% 2 == 1){
      shinyjs::show("model_select_breast_2")
    } else {
      shinyjs::hide("model_select_breast_2")
    }
  })
  
  # If the user clicks apply then set the counter to 2
  observeEvent(input$apply_comparison_breast_2, {
    counter$counter = 2
  })
  
  # Hide third model and variable selection box
  observeEvent(input$multiple_rocs_breast_2, {
    if(input$multiple_rocs_breast_2 %% 2 == 1){
      shinyjs::show("model_select_breast_3")
    } else {
      shinyjs::hide("model_select_breast_3")
    }
  })
  
  # If the user clicks apply then set the counter to 2
  observeEvent(input$apply_comparison_breast_3, {
    counter$counter = 3
  })
  
  observeEvent(input$breast_cancer_ROC_plot, {
    counter$counter = 1
  })
  
  err_metrics = function(CM)
  {
    TN =CM[1,1]
    TP =CM[2,2]
    FP =CM[1,2]
    FN =CM[2,1]
    precision =(TP)/(TP+FP)
    recall_score =(FP)/(FP+TN)
    f1_score=2*((precision*recall_score)/(precision+recall_score))
    accuracy_model  =(TP+TN)/(TP+TN+FP+FN)
    False_positive_rate =(FP)/(FP+TN)
    False_negative_rate =(FN)/(FN+TP)
    err_df = as.data.frame(cbind(c("Accuracy", "Precision", "Recall", "FPR", "FNR", "F1"),
                                 c(accuracy_model, precision, recall_score, False_positive_rate,
                                   False_negative_rate, f1_score)))
    return(err_df)
  }
  
  process_data <- function(category, subcategory, input_filters, legend_entry, ML, full_ml_set, color) {
    
    # Variables for inputs
    ml_study_subset = full_ml_set$Dataset %in% input_filters$dataset_checkbox
    
    # Subset the data that the model will be assessed on
    dataset = full_ml_set[ml_study_subset,] %>%
      dplyr::filter(Treatment %in% input_filters$treatment_filter) %>%
      dplyr::filter(Timepoint %in% input_filters$timepoint_filter) %>%
      dplyr::filter(Location %in% input_filters$location_filter) %>%
      dplyr::filter(pam50 %in% input_filters$pam50_filter) %>%
      dplyr::filter(rorS_risk %in% input_filters$rorS_filter) %>%
      dplyr::filter(Mammaprint_risk %in% input_filters$Mammaprint_filter) %>%
      dplyr::filter(Menopause.status %in% input_filters$Meno_filter) %>%
      dplyr::filter(ER.status %in% input_filters$ER_filter)
    
    # Number of samples in the filtered dataset
    nsamples = nrow(dataset)
    
    model = ML[[category]][[subcategory]]
    
    # Predict and create a confusion matrix
    # Make predictions according to model chosen
    if(subcategory == "100X Bagging"){
      predictions_frame = adabag::predict.bagging(model, dataset)
      predictions = predictions_frame[["class"]]
      confusion_matrix = table(predictions, dataset$Response)
      model_accuracy = 1 - (confusion_matrix[1] + confusion_matrix[4])/
        sum(confusion_matrix)
      predicted_probabilities = predictions_frame[["prob"]]
      errs = err_metrics(confusion_matrix)
      colnames(errs) = c("Metrics", legend_entry)
    } else if(subcategory == "Boosting") {
      predictions_frame = predict(model, dataset)
      predictions = predictions_frame[["class"]]
      confusion_matrix = predictions_frame[["confusion"]]
      model_accuracy = 1 - (confusion_matrix[1] + confusion_matrix[4])/
        sum(confusion_matrix)
      predicted_probabilities = predictions_frame[["prob"]]
      errs = err_metrics(confusion_matrix)
      colnames(errs) = c("Metrics", legend_entry)
    } else {
      predictions = predict(model, dataset)
      confusion_matrix = table(predictions, dataset$Response)
      model_accuracy = (confusion_matrix[1] + confusion_matrix[4])/
        sum(confusion_matrix)
      predicted_probabilities = predict(model, dataset, type = "prob")
      errs = err_metrics(confusion_matrix)
      colnames(errs) = c("Metrics", legend_entry)
    }
    
    # ROC plot generation
    if(category == "Support Vector Machines"){
      print("ROC plots not available for Support Vector Machines")
      p = NULL
      auc_value = NULL
    } else {
      join = cbind(predictions, predicted_probabilities, dataset)
      colnames(join)[c(2,3)] = c("Responder", "Non_responder")
      join = join[order(join$Responder),]
      model_roc = roc(predictor = join$Responder, 
                      response = as.character(join$Response))
      auc_value = round(auc(model_roc), 3)
      p = plot(model_roc, 
               main = "ROC curve",
               col = color, lwd = 3, legacy.axes = TRUE, xlim = c(1,0), ylim = c(0,1), 
               asp = 0.92, cex = 4, xaxs = "i", yaxs = "i", width = 900, height = 900)
    }
    
    return(list(plot = p, conf = confusion_matrix, acc = model_accuracy, 
                Legends = legend_entry, auc_values = auc_value, nsamples = nsamples, 
                error_metrics = errs))
  }
  
  # ROC curve function
  plot_breast_cancer_ROC <- reactive({
    results <- list()
    
    if(counter$counter == 1){
      input_filters_1 = list(
        dataset_checkbox = input$ml_breast_dataset_checkbox,
        treatment_filter = input$breast_cancer_ml_treatment_filter,
        timepoint_filter = input$breast_cancer_ml_timepoint_filter,
        location_filter = input$breast_cancer_ml_location_filter,
        pam50_filter = input$breast_cancer_ml_pam50_filter,
        rorS_filter = input$breast_cancer_ml_rorS_filter,
        Mammaprint_filter = input$breast_cancer_ml_Mammaprint_filter,
        Meno_filter = input$breast_cancer_ml_Meno_filter,
        ER_filter = input$breast_cancer_ml_ER_filter
      )
      
      results[[1]] <- process_data(input$breast_cancer_ml_model_category, 
                                   input$breast_cancer_ml_model_subcategory, 
                                   input_filters_1, 
                                   input$breast_cancer_ml_legend_entry_1, 
                                   ML, 
                                   full_ml_set, 
                                   color = "#2A5674")
      
      if(input$breast_cancer_ml_model_category == "Support Vector Machines"){
        print("ROC plots not available for Support Vector Machines")
      }
      
    } else if(counter$counter == 2) {
      # Process the first dataset
      input_filters_1 = list(
        dataset_checkbox = input$ml_breast_dataset_checkbox,
        treatment_filter = input$breast_cancer_ml_treatment_filter,
        timepoint_filter = input$breast_cancer_ml_timepoint_filter,
        location_filter = input$breast_cancer_ml_location_filter,
        pam50_filter = input$breast_cancer_ml_pam50_filter,
        rorS_filter = input$breast_cancer_ml_rorS_filter,
        Mammaprint_filter = input$breast_cancer_ml_Mammaprint_filter,
        Meno_filter = input$breast_cancer_ml_Meno_filter,
        ER_filter = input$breast_cancer_ml_ER_filter
      )
      results[[1]] <- process_data(input$breast_cancer_ml_model_category, 
                                   input$breast_cancer_ml_model_subcategory, 
                                   input_filters_1, 
                                   input$breast_cancer_ml_legend_entry_1, 
                                   ML, 
                                   full_ml_set, 
                                   color = "#2A5674")
      
      # Process the second dataset
      input_filters_2 = list(
        dataset_checkbox = input$ml_breast_dataset_checkbox_2,
        treatment_filter = input$breast_cancer_ml_treatment_filter_2,
        timepoint_filter = input$breast_cancer_ml_timepoint_filter_2,
        location_filter = input$breast_cancer_ml_location_filter_2,
        pam50_filter = input$breast_cancer_ml_pam50_filter_2,
        rorS_filter = input$breast_cancer_ml_rorS_filter_2,
        Mammaprint_filter = input$breast_cancer_ml_Mammaprint_filter_2,
        Meno_filter = input$breast_cancer_ml_Meno_filter_2,
        ER_filter = input$breast_cancer_ml_ER_filter_2
      )
      results[[2]] <- process_data(input$breast_cancer_ml_model_category_2, 
                                   input$breast_cancer_ml_model_subcategory_2, 
                                   input_filters_2, 
                                   input$breast_cancer_ml_legend_entry_2, 
                                   ML, 
                                   full_ml_set, 
                                   color = "#E34F6F")
      
      if(input$breast_cancer_ml_model_category_2 == "Support Vector Machines"){
        print("ROC plots not available for Support Vector Machines")
      }
      
    } else if(counter$counter == 3) {
      # Process the first dataset
      input_filters_1 = list(
        dataset_checkbox = input$ml_breast_dataset_checkbox,
        treatment_filter = input$breast_cancer_ml_treatment_filter,
        timepoint_filter = input$breast_cancer_ml_timepoint_filter,
        location_filter = input$breast_cancer_ml_location_filter,
        pam50_filter = input$breast_cancer_ml_pam50_filter,
        rorS_filter = input$breast_cancer_ml_rorS_filter,
        Mammaprint_filter = input$breast_cancer_ml_Mammaprint_filter,
        Meno_filter = input$breast_cancer_ml_Meno_filter,
        ER_filter = input$breast_cancer_ml_ER_filter
      )
      results[[1]] <- process_data(input$breast_cancer_ml_model_category, 
                                   input$breast_cancer_ml_model_subcategory, 
                                   input_filters_1, 
                                   input$breast_cancer_ml_legend_entry_1, 
                                   ML, 
                                   full_ml_set,
                                   color = "#2A5674")
      
      # Process the second dataset
      input_filters_2 = list(
        dataset_checkbox = input$ml_breast_dataset_checkbox_2,
        treatment_filter = input$breast_cancer_ml_treatment_filter_2,
        timepoint_filter = input$breast_cancer_ml_timepoint_filter_2,
        location_filter = input$breast_cancer_ml_location_filter_2,
        pam50_filter = input$breast_cancer_ml_pam50_filter_2,
        rorS_filter = input$breast_cancer_ml_rorS_filter_2,
        Mammaprint_filter = input$breast_cancer_ml_Mammaprint_filter_2,
        Meno_filter = input$breast_cancer_ml_Meno_filter_2,
        ER_filter = input$breast_cancer_ml_ER_filter_2
      )
      results[[2]] <- process_data(input$breast_cancer_ml_model_category_2, 
                                   input$breast_cancer_ml_model_subcategory_2, 
                                   input_filters_2, 
                                   input$breast_cancer_ml_legend_entry_2, 
                                   ML, 
                                   full_ml_set,
                                   color = "#E34F6F")
      
      # Process the third dataset
      input_filters_3 = list(
        dataset_checkbox = input$ml_breast_dataset_checkbox_3,
        treatment_filter = input$breast_cancer_ml_treatment_filter_3,
        timepoint_filter = input$breast_cancer_ml_timepoint_filter_3,
        location_filter = input$breast_cancer_ml_location_filter_3,
        pam50_filter = input$breast_cancer_ml_pam50_filter_3,
        rorS_filter = input$breast_cancer_ml_rorS_filter_3,
        Mammaprint_filter = input$breast_cancer_ml_Mammaprint_filter_3,
        Meno_filter = input$breast_cancer_ml_Meno_filter_3,
        ER_filter = input$breast_cancer_ml_ER_filter_3
      )
      results[[3]] <- process_data(input$breast_cancer_ml_model_category_3, 
                                   input$breast_cancer_ml_model_subcategory_3, 
                                   input_filters_3, 
                                   input$breast_cancer_ml_legend_entry_3, 
                                   ML, 
                                   full_ml_set,
                                   color = "#6C2167")
      
      if(input$breast_cancer_ml_model_category_3 == "Support Vector Machines"){
        print("ROC plots not available for Support Vector Machines")
      }
    }
    
    return(results)
  })
    
  
  # Plot ROC curve
  output$breast_cancer_ROC_plot <- renderPlot({
    input$predict_ml_breast_cancer
    isolate({
      results = plot_breast_cancer_ROC()
      
      # Predefined colors
      predefined_colors = c("#2A5674", "#E34F6F", "#6C2167")
      
      # Initialize an empty plot
      plot(1, type="n", xlim=c(1,0), ylim=c(0,1), xlab="False Positive Rate", ylab="True Positive Rate", main="ROC curve")
      
      # Store legends
      legends = vector("character", length(results))
      
      # Iterate over results and add each ROC curve to the plot
      for(i in 1:length(results)) {
        res = results[[i]]
        if(!is.null(res$plot)) {
          # Store legend
          legends[i] = paste0(res$Legends, ", AUC=", res$auc_values, ", n=", res$nsamples)
          
          # Add ROC curve to the plot using predefined color
          lines(res$plot, col=predefined_colors[i], lwd=3)
        }
      }
      
      # Add legend
      legend(0.5, 0.25, legend=legends, col=predefined_colors[1:length(results)], lty=1, cex=0.8)
    })
  })
  
  
  # Print error metrics
  output$breast_cancer_error_table <- DT::renderDataTable({
    input$predict_ml_breast_cancer
    isolate({
      plot_breast_cancer_ROC()$error_metrics})
  })
  
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
  
  # Pop-up info message, triggered when the user presses the Info button
  output$ml_breast_info_text <- renderText({
    paste0("<br> &#8226 Support Vector Machines <b>do not produce ROC plots</b>, only error metrics.",
           "<br> &#8226 The final model we chose in our project is the <b>C5.0-ROC-optimised decision tree</b>.",
           "<br> &#8226 You can pick the model category and then subcategory you are interested in and then the study subset of interest.", 
           "<br> &#8226 You can also filter by multiple variables, for more specific model predictions. Excessive filtering may result in a NULL set.",
           "<br> &#8226 You can plot up to three ROC curves at the same plot, <b>but you have to click 'Apply' after you have specified your model and data subset and also provide an appropriate legend for each selection of yours</b>.",
           "<br> &#8226 After pressing 'Predict' and viewing your output, if you want to do multiple models again, you'll have to <b>press Apply</b> in <b>all</b> additional models before plotting.")})
  
  # Breast Cancer: Custom DGEA #####
  
  # Contrast variables control
  observeEvent(input$breast_custom_DGEA_adjustments, {
    updateSelectInput(inputId = "breast_cancer_custom_DGEA_contrast_selection",
                      choices = sort(input$breast_custom_DGEA_adjustments),
                      selected = sort(input$breast_custom_DGEA_adjustments)[1])
  })
  
  observeEvent(input$breast_cancer_custom_DGEA_contrast_selection, {
    updateSelectInput(inputId = "breast_cancer_custom_DGEA_contrast_level1_selection",
                      choices = sort(unique(Pheno_exprs[,input$breast_cancer_custom_DGEA_contrast_selection])),
                      selected = sort(unique(Pheno_exprs[,input$breast_cancer_custom_DGEA_contrast_selection]))[1])
    updateSelectInput(inputId = "breast_cancer_custom_DGEA_contrast_level2_selection",
                      choices = sort(unique(Pheno_exprs[,input$breast_cancer_custom_DGEA_contrast_selection])),
                      selected = sort(unique(Pheno_exprs[,input$breast_cancer_custom_DGEA_contrast_selection]))[2])
  })
  
  # DGEA function
  analyse <- reactive({
    
    # Filter samples based on user selection
    study_subset = Pheno_exprs$Dataset %in% 
      input$breast_custom_DGEA_dataset_checkbox
    
    dgea_set = Pheno_exprs[study_subset,] %>%
      dplyr::filter(Treatment %in% input$breast_custom_DGEA_treatment_filter) %>%
      dplyr::filter(Timepoint_coded %in% input$breast_custom_DGEA_timepoint_filter) %>%
      dplyr::filter(Location %in% input$breast_custom_DGEA_location_filter) %>%
      dplyr::filter(pam50 %in% input$breast_custom_DGEA_pam50_filter) %>%
      dplyr::filter(rorS_risk %in% input$breast_custom_DGEA_rorS_filter) %>%
      dplyr::filter(Mammaprint_risk %in% input$breast_custom_DGEA_Mammaprint_filter) %>%
      dplyr::filter(Menopause.status %in% input$breast_custom_DGEA_Meno_filter) %>%
      dplyr::filter(ER.status %in% input$breast_custom_DGEA_ER_filter)
    
    # Filter expression matrix
    matrix = z_exprs[, dgea_set$Sample.ID]
    
    # Design matrix
    adjustments = input$breast_custom_DGEA_adjustments
    contrast = input$breast_cancer_custom_DGEA_contrast_selection
    contrast_level_1 = input$breast_cancer_custom_DGEA_contrast_level1_selection
    contrast_level_2 = input$breast_cancer_custom_DGEA_contrast_level2_selection
    
    adjustments = c(contrast, sort(c(setdiff(adjustments, contrast), "Dataset")))
    
    design_vars = lapply(adjustments, function(x) {
      paste0("dgea_set$", x)
    }
    )
    design = model.matrix(as.formula(paste("~0 + ", 
                                           paste(design_vars, collapse = " + "))))
    
    rownames(design) = colnames(matrix)
    colnames2 = colnames(design)
    str_sub(colnames2, 0, 9) = ""
    colnames(design) = colnames2
    colnames(design)[c(1,2)] = sort(c(contrast_level_1, contrast_level_2))
    
    # Contrast matrix
    cm = makeContrasts(contrasts = 
                         paste0(contrast_level_1, " - " , contrast_level_2),
                       levels = design)
    
    # Limma                     
    fit = lmFit(matrix, design = design)
    fit2 = contrasts.fit(fit, contrasts = cm)
    fit2 = eBayes(fit2, robust = TRUE)
    results = summary(decideTests(fit2))
    topTable = as.data.frame(topTable(fit2, adjust = "BH", number = Inf))
    topTable$Gene.Symbol = as.character(rownames(topTable))
    
    # Volcano
    vline <- function(x = 0, color = "black") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = "dot")
      )
    }
    
    # Horizontal line for plotly
    hline <- function(y = 0, color = "black") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list(color = color, dash = "dot")
      )
    }  
    
    # Create custom variable to aid in color coding and legend labels later
    topTable$volc_p_status = NA
    topTable$volc_p_status[topTable$adj.P.Val>input$pval_breast_custom &
                             abs(topTable$logFC)<input$logFC_breast_custom] = "Not Significant"
    topTable$volc_p_status[topTable$adj.P.Val>input$pval_breast_custom &
                             abs(topTable$logFC)>input$logFC_breast_custom] = 
      paste0("|DE|<", input$logFC_breast_custom," s.d. & p<", input$pval_breast_custom)
    topTable$volc_p_status[topTable$adj.P.Val<input$pval_breast_custom &
                             topTable$logFC < abs(input$logFC_breast_custom)*-1] = 
      paste0("DE<-", input$logFC_breast_custom," s.d. & p<", input$pval_breast_custom)
    topTable$volc_p_status[topTable$adj.P.Val<input$pval_breast_custom &
                             topTable$logFC>input$logFC_breast_custom] = 
      paste0("DE>", input$logFC_breast_custom," s.d. & p<", input$pval_breast_custom)
    topTable$volc_p_status = factor(topTable$volc_p_status,
                                    levels = c("Not Significant", 
                                               paste0("|DE|<", input$logFC_breast_custom," s.d. & p<", input$pval_breast_custom),
                                               paste0("DE<-", input$logFC_breast_custom," s.d. & p<", input$pval_breast_custom), 
                                               paste0("DE>", input$logFC_breast_custom," s.d. & p<", input$pval_breast_custom)),
                                    labels = c("Not Significant", 
                                               paste0("|DE|<", input$logFC_breast_custom," s.d. & p<", input$pval_breast_custom),
                                               paste0("DE<-", input$logFC_breast_custom," s.d. & p<", input$pval_breast_custom), 
                                               paste0("DE>", input$logFC_breast_custom," s.d. & p<", input$pval_breast_custom)))
    
    # Create the palette for the scatter colors
    pal = c(input$scatter_color_breast_NS_custom, 
            input$scatter_color_breast_p_only_custom, 
            input$scatter_color_breast_downr_custom, 
            input$scatter_color_breast_upr_custom)
    pal = setNames(pal, levels(topTable$volc_p_status))
    
    # Main plotly function call
    p = plot_ly(data = topTable, 
                x = ~logFC, 
                y = ~-log10(topTable[,input$pval_selector_breast_custom]),
                color = ~volc_p_status, 
                marker = list(size = 5), 
                type = "scatter",
                mode = "markers",
                colors = pal, opacity = input$alpha_breast_volcano_custom,
                hoverinfo = "text",
                hovertext = paste("</br> Gene Symbol:", topTable$Gene.Symbol,
                                  "</br> p-value:", topTable$P.Value,
                                  "</br> adj.P.Val:", topTable$adj.P.Val,
                                  "</br> Diff.Exp:", round(topTable$logFC,3)),
                width = 750, height = 500) %>%
      
      # Layout with titles and drawn lines
      layout(title = list(text = paste0("<b>", input$breast_custom_DGEA_title_input),
                          x = 0, y = 0.99),
             shapes = list(
               vline(input$logFC_breast_custom), vline(-input$logFC_breast_custom), # vertical lines
               hline(-log10(input$pval_breast_custom))), # horizontal line
             xaxis = list(title = list(text = "<b>Differential Expression (units:sd)"),
                          range = c(-(max(abs(topTable$logFC))+0.1*max(abs(topTable$logFC))), 
                                    (max(abs(topTable$logFC))+0.1*max(abs(topTable$logFC)))),
                          linecolor = '#000000',
                          linewidth = 2),
             yaxis = list(title = list(text = "<b>-log10(p)"),
                          range = c(0, (max(-log10(topTable[,input$pval_selector_breast_custom]))+1)),
                          linecolor = '#000000',
                          linewidth = 2)) %>%
      
      # Labels for gene symbols. Toggled when clicking on individual points
      add_annotations(visible = FALSE,
                      x = topTable$logFC, 
                      y = -log10(topTable[,input$pval_selector_breast_custom]),
                      text = topTable$Gene.Symbol,
                      showarrow = TRUE,
                      arrowhead = 0,
                      clicktoshow = "onoff")
    
    return(list(topTable = topTable, volcano = p))
    
  })
  
  # Plot Volcano
  output$breast_cancer_volcano_custom <- renderPlotly({
    input$breast_cancer_DGEA_analyse_button
    isolate({
      res = analyse()
      res$volcano
    })
  })
  
  # Print topTable output
  output$breast_cancer_custom_toptable <- DT::renderDataTable({
    input$breast_cancer_DGEA_analyse_button
    isolate({
      analyse()$topTable})
  }, options = list(scrollX = TRUE))
  
  # Download table data
  output$download_data_custom_DGEA <- downloadHandler(
    filename = "topTable.csv",
    content = function(file) {
      data <- analyse()$topTable
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Select all button
  observe({
    if(input$select_all_breast_custom_DGEA > 0){
      if (input$select_all_breast_custom_DGEA %% 2 == 0) {
        updateCheckboxGroupInput(session, "breast_custom_DGEA_dataset_checkbox",
                                 choices = unique(Pheno_exprs$Dataset),
                                 selected = unique(Pheno_exprs$Dataset),
                                 inline = TRUE)
      }
      else
      {
        updateCheckboxGroupInput(session, "breast_custom_DGEA_dataset_checkbox",
                                 choices = unique(Pheno_exprs$Dataset),
                                 selected = c(),
                                 inline = TRUE)
      }
    }
  })
  
  # Reset button
  observeEvent(input$reset_input_breast_custom_DGEA, {
    shinyjs::reset("breast_custom_DGEA_treatment_filter")
    shinyjs::reset("breast_custom_DGEA_timepoint_filter")
    shinyjs::reset("breast_custom_DGEA_location_filter")
    shinyjs::reset("breast_custom_DGEA_pam50_filter")
    shinyjs::reset("breast_custom_DGEA_rorS_filter")
    shinyjs::reset("breast_custom_DGEA_Mammaprint_filter")
    shinyjs::reset("breast_custom_DGEA_Meno_filter")
    shinyjs::reset("breast_custom_DGEA_ER_filter")
    shinyjs::reset("breast_custom_DGEA_adjustments")
    shinyjs::reset("breast_custom_DGEA_adjustments")
    shinyjs::reset("breast_cancer_custom_DGEA_contrast_selection")
    shinyjs::reset("breast_cancer_custom_DGEA_contrast_level1_selection")
    shinyjs::reset("breast_cancer_custom_DGEA_contrast_level2_selection")
    shinyjs::reset("pval_selector_breast_custom")
    shinyjs::reset("logFC_breast_custom")
    shinyjs::reset("pval_breast_custom")
    shinyjs::reset("scatter_color_breast_NS_custom")
    shinyjs::reset("scatter_color_breast_DE_only_custom")
    shinyjs::reset("scatter_color_breast_p_only_custom")
    shinyjs::reset("scatter_color_breast_DE_p_custom")
    shinyjs::reset("alpha_breast_volcano_custom")
  })
  
  # Pop-up info message, triggered when the user presses the Info button
  output$custom_dgea_info_text <- renderText({
    paste0("<br> &#8226 This analysis is run on <b>training</b> and <b>validation</b> samples only.",
           "<br> &#8226 You can select how to filter the combined samples based on multiple variables.",
           "<br> &#8226 You must choose at least one variable for adjustment. Results are by default adjusted for dataset.",
           "<br> &#8226 Select level 1 and level 2 for the contrasts, i.e. the groups you want
                           to compare. Results should be interpreted as down-/up-regulated in level 1 compared to level 2.",
           "<br> &#8226 <b>You must refresh the page after each time you press the 'Analyse' button to produce new results</b>.")
  })
  
  # Breast Cancer: Unique sample predictor #####
  # Additional tab for importing single sample or new dataset (appropriately formatted)
  # and make predictions
  
  # Import data type
  import_data_type = reactive({ input$breast_cancer_new_prediction_type })
  
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
  
  print("All ok until line 1444")
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
                   "\n", "Problem with:", setdiff(gene_columns_with_X, gene_colnames_input_data)), easyClose = TRUE 
          )
        )
        return() # exit code for this event
      } else if (X_handle == "off" && nonX_gene_columns_imported_number < 166) {
        showModal(
          modalDialog(
            title = "Warning", 
            paste0("There are missing markers in the data (assuming format: 'Entrez'). 166 variables required.",
                   "\n", "Problem with:", setdiff(gene_columns_without_X, gene_colnames_input_data)), easyClose = TRUE 
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
                                  "Import pre-annotated dataset")) {
      if (length(intersect(colnames(input_data), all_correct_columns)) < 183) {
        showModal(
          modalDialog(
            title = "Warning", 
            paste0("There are missing columns in the data.",
                   "\n", "Problem with:", setdiff(all_correct_columns, colnames(input_data))), easyClose = TRUE 
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
    
    # Add treatment column
    if (!"Endo" %in% colnames(imported_data())) {
      
    }
    
    print("Maybe treatment failure?")
    if (desired_treatment() == "Included in input") {
      if (unique(grepl("Endo", colnames(imported_data()))) == FALSE) {
        showModal(modalDialog(
          title = "Warning",
          "'Included in input' is selected in the treatment radio buttons.
          The uploaded file should contain a column named 'Endo' with a value 
          0 (Chemotherapy) or 1 (Endocrine treatment).",
          easyClose = TRUE
        ))
        return() # exit code for this event
      }
    } else if (desired_treatment() == "Chemotherapy" && !"Endo" %in% colnames(imported_data())) {
      imported_data(imported_data() %>% dplyr::mutate(Endo = 0))
    } else if (desired_treatment() == "Chemotherapy" && "Endo" %in% colnames(imported_data())) {
      imported_data()[, "Endo"] = 0
    } else if (desired_treatment() == "Endocrine treatment" && !"Endo" %in% colnames(imported_data())) {
      imported_data(imported_data() %>% dplyr::mutate(Endo = 1))
    } else if (desired_treatment() == "Endocrine treatment" && "Endo" %in% colnames(imported_data())) {
      imported_data()[, "Endo"] = 1
    }
  })

}