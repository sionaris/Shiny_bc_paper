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