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
          x = breast_cancer_full_pheno[subset_breast_hist, input$histvar_select_breast],
          type = "histogram", 
          histnorm = names(hist_types_breast[which(hist_types_breast %in% input$hist_type_breast)]),
          marker = list(color = input$hist_fill_breast,
                        line = list(color = input$hist_color_breast, width = 0.5)),
          autobinx = FALSE, nbinsx = ~input$hist_breast_bins*2, height = 300) %>%
    layout(title = list(text = paste0("<b>Histogram of ", input$histvar_select_breast),
                        x = 0, y = 0.99, font = list(size  = 14)),
           # annotations = list(x = -0.062, y = 1.1, xref = "paper", yref = "paper",
           #                  showarrow = F,
           #                   text = paste0("Studies: ",
           #                                  paste(input$hist_dataset_checkbox, collapse = ", ")),
           #                   font = list(size = 10)),
           margin = list(t = 50, b = 5),
           xaxis = list(title = list(text = paste0("<b>", input$histvar_select_breast)),
                        linecolor = '#000000',
                        linewidth = 2),
           yaxis = list(title = list(text = y_axis_titles_breast[input$hist_type_breast][[1]]),
                        linecolor = '#000000',
                        linewidth = 2)
    )
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

