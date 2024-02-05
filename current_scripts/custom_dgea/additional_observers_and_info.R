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
  HTML(paste0("<br> &#8226 This analysis is run on <b>training</b> and <b>validation</b> samples only.",
         "<br> &#8226 You can select how to filter the combined samples based on multiple variables.",
         "<br> &#8226 You must choose at least one variable for adjustment. Results are by default adjusted for dataset.",
         "<br> &#8226 Select level 1 and level 2 for the contrasts, i.e. the groups you want
                           to compare. Results should be interpreted as down-/up-regulated in level 1 compared to level 2.",
         "<br> &#8226 <b>You must refresh the page after each time you press the 'Analyse' button to produce new results</b>.",
         "<br> &#8226 Check out this GIF tutorial for a short demonstration:
           <ul>
           <li><a href='GIFs/custom_dgea_gif.gif' target='_blank'>Custom DGEA demo</a></li>
         </ul>"))
})