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