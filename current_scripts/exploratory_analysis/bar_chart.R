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
                          x = 0, y = 0.99, font = list(size  = 14)),
             # annotations = list(x = -0.062, y = 1.1, xref = "paper", yref = "paper",
             #                    showarrow = F,
             #                    text = paste0("Studies: ",
             #                                 paste(input$hist_dataset_checkbox, collapse = ", ")),
             #                  font = list(size = 10)),
             margin = list(t = 50, b = 5),
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
                          x = 0, y = 0.99, font = list(size  = 14)),
             # annotations = list(x = -0.062, y = 1.1, xref = "paper", yref = "paper",
             #                    showarrow = F,
             #                    text = paste0("Studies: ",
             #                                  paste(input$hist_dataset_checkbox, collapse = ", ")),
             #                    font = list(size = 10)),
             margin = list(t = 50, b = 5),
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