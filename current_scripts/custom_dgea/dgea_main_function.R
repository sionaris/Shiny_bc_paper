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
  topTable$volc_p_status[topTable$adj.P.Val>input$pval_breast_custom] = "Not Significant"
  topTable$volc_p_status[topTable$adj.P.Val<input$pval_breast_custom &
                           abs(topTable$logFC)<input$logFC_breast_custom] = 
    paste0("|DE|<", input$logFC_breast_custom," s.d. & p<", input$pval_breast_custom)
  topTable$volc_p_status[topTable$adj.P.Val<input$pval_breast_custom &
                           topTable$logFC < abs(input$logFC_breast_custom)*-1] = 
    paste0("DE<-", input$logFC_breast_custom," s.d. & p<", input$pval_breast_custom)
  topTable$volc_p_status[topTable$adj.P.Val<input$pval_breast_custom &
                           topTable$logFC>abs(input$logFC_breast_custom)] = 
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