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
  breast_cancer_DGEA$volc_p_status[breast_cancer_DGEA$adj.P.Val>input$pval_breast] = "Not Significant"
  breast_cancer_DGEA$volc_p_status[breast_cancer_DGEA$adj.P.Val<input$pval_breast &
                                     abs(breast_cancer_DGEA$logFC)<input$logFC_breast] = 
    paste0("|DE|<", input$logFC_breast," s.d. & p<", input$pval_breast)
  breast_cancer_DGEA$volc_p_status[breast_cancer_DGEA$adj.P.Val<input$pval_breast &
                                     breast_cancer_DGEA$logFC<abs(input$logFC_breast)*-1] = 
    paste0("DE<-", input$logFC_breast," s.d. & p<", input$pval_breast)
  breast_cancer_DGEA$volc_p_status[breast_cancer_DGEA$adj.P.Val<input$pval_breast &
                                     breast_cancer_DGEA$logFC>abs(input$logFC_breast)] = 
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

# Create a waiter
w <- Waiter$new(id = "breast_volcano", volcano_loading_screen, "black")

# Output
output$breast_volcano <- renderPlotly({
  w$show()
  input$draw_breast_volcano 
  isolate({prepare_volcano()})
})

# Reset parameters after pressing the corresponding button
observeEvent(input$reset_input_breast_volcano, {
  shinyjs::reset("Volcano_tuning_breast") 
})

# Pop-up info message, triggered when the user presses the Info button
output$volcano_breast_info_text <- renderText({
  HTML(paste0("<br> &#8226 Select parameters for the volcano plot.",
         "<br> &#8226 Default parameters are the ones used in the project.",
         "<br> &#8226 You can overlay gene symbols by clicking on individual data points.",
         "<br> &#8226 The x-axis shows the numerical difference between the mean expression in group 1 (responders) and group 2 (non-responders) measured in standard deviations from the gene's overall mean expression.",
         "<br> &#8226 Check out this GIF tutorial for a short demonstration:
           <ul>
           <li><a href='GIFs/volcano_gif.gif' target='_blank'>Volcano plot generation demo</a></li>
         </ul>"))})