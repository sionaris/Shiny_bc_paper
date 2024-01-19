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
                                 "XVC1", "XVC2", "XVE", "XVC3",
                                 "Non_responder", "Responder", 
                                 "Basal", "Her2", "LumA", "LumB", "Normal",
                                 "T1", "T2",
                                 "High", "Intermediate", "Low",
                                 "Risk", "No risk",
                                 "Others", "Claudin",
                                 "HER2+", "ER-/HER2-", "ER+/HER2- High Prolif", "ER+/HER2- Low Prolif",
                                 "UK", "USA", "Korea",
                                 "Mixed", "Post-meno", # Mixed is used for both ER and Meno status here
                                 "ER+",
                                 "Cluster 1", "Cluster 2"))

# Synchronize tabs
observeEvent(input$sunburst_tabs_breast, {
  # This will trigger whenever the selected tab in the first tabBox changes
  # Update the selected tab in the second tabBox
  updateTabsetPanel(session, "sunburst_tuning_breast", selected = input$sunburst_tabs_breast)
})

observeEvent(input$sunburst_tuning_breast, {
  # This will trigger whenever the selected tab in the second tabBox changes
  # Update the selected tab in the first tabBox
  updateTabsetPanel(session, "sunburst_tabs_breast", selected = input$sunburst_tuning_breast)
})
