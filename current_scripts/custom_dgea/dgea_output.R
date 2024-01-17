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

# Downloadable output
output$download_data_custom_DGEA <- downloadHandler(
  filename = function() {
    paste0("topTable_", Sys.Date(), ".xlsx")
  },
  content = function(con) {
    # Call the predict_new_data function once and store the result
    result <- analyse()
    toptable <- result$topTable
    
    # Check if the table is not null
    if (!is.null(toptable)) {
      # Directly write the table to the Excel file
      openxlsx::write.xlsx(toptable, con)
    }
  }
)