# Make the prediction (+/- produce ROC plot) ###
predict_new_data <- reactive({
  # print(paste("Colnames of object:", paste(colnames(imported_data()), collapse = ", ")))
  current_data = imported_data()
  current_data[, c("HER2", "LumA", "LumB", "Normal", "T2", "Endo", "IC2", "IC3", "IC4",
                   "IC5", "IC6", "IC7", "IC8", "IC9",
                   "IC10", "Mammaprint_risk_yes", "rorS_risk_interm", "rorS_risk_high",
                   "ER_hp", "ER_lp", "HER2_scmod1")] <- 
    lapply(current_data[, c("HER2", "LumA", "LumB", "Normal", "T2", "Endo", "IC2", "IC3", "IC4",
                            "IC5", "IC6", "IC7", "IC8", "IC9",
                            "IC10", "Mammaprint_risk_yes", "rorS_risk_interm", "rorS_risk_high",
                            "ER_hp", "ER_lp", "HER2_scmod1")], function(x) 
                              factor(x, levels = c(0, 1), labels = c(0, 1))
    )
  imported_data(current_data)
  # Unique sample predictions
  if (import_data_type() %in% c("Random sample",
                                "Import unique sample (genes only)",
                                "Import unique sample (pre-annotated)")) {
    # Make prediction
    prediction = predict(ML$`Decision Trees`$`C5.0 - ROC`, 
                         imported_data(), 
                         type = "prob")
    resp_prob = paste0(round(100*as.numeric(prediction["Responder"]), 2), "%")
    # print(resp_prob)
    non_resp_prob = paste0(round(100*as.numeric(prediction["Non_responder"]), 2), "%")
    
    # Create a new data frame for the new variables
    new_vars <- data.frame(
      Responder = as.numeric(prediction[, "Responder"]),
      Non_responder = as.numeric(prediction[, "Non_responder"]),
      `Response chance %` = resp_prob,
      `No response chance %` = non_resp_prob
    )
    
    # Bind the new variables to your original data frame
    newdata <- cbind(imported_data(), new_vars)
    
    # Update the Response column
    newdata$Response <- factor(newdata$Response, levels = c("Responder", "Non_responder"),
                               labels = c("Responder", "Non_responder"))
    
    chosen_treatment = ifelse(imported_data()[, "Endo"] == 1, "endocrine treatment",
                              "chemotherapy")
    
    return(list(predictions = newdata, resp_prob = resp_prob, plot = NULL, auc = NULL, 
                chosen_treatment = chosen_treatment))
    
  } else if (import_data_type() == "Import pre-annotated dataset") {
    
    # Make prediction
    prediction = predict(ML$`Decision Trees`$`C5.0 - ROC`, 
                         filtered_data(), 
                         type = "prob")
    resp_prob = paste0(round(100*as.numeric(prediction[, "Responder"]), 2), "%")
    non_resp_prob = paste0(round(100*as.numeric(prediction[, "Non_responder"]), 2), "%")
    
    # Create a new data frame for the new variables
    new_vars <- data.frame(
      Responder = as.numeric(prediction[, "Responder"]),
      Non_responder = as.numeric(prediction[, "Non_responder"]),
      `Response chance %` = resp_prob,
      `No response chance %` = non_resp_prob
    )
    
    # Bind the new variables to your original data frame
    newdata <- cbind(filtered_data(), new_vars)
    
    # Update the Response column
    newdata$Response <- factor(newdata$Response, levels = c("Responder", "Non_responder"),
                               labels = c("Responder", "Non_responder"))
    
    if (test_passed()) {
      join = newdata
      join = join[order(join$Responder),]
      model_roc = roc(predictor = join$Responder, 
                      response = as.character(join$Response))
      auc_value = round(auc(model_roc), 3)
      p = plot(model_roc, 
               main = "ROC curve",
               col = "#2A5674", lwd = 3, legacy.axes = TRUE, xlim = c(1,0), ylim = c(0,1), 
               asp = 0.92, cex = 4, xaxs = "i", yaxs = "i", width = 900, height = 900)
      
      return(list(predictions = newdata, plot = p, auc = auc_value))
    } else {
      return(list(predictions = newdata, plot = NULL, auc = NULL))
    }
  }
})

# Create a waiter
w4 <- Waiter$new(id = "newpred_ROC_plot", ml_loading_screen_solar, "#030649")

# Plot ROC curve
output$newpred_ROC_plot <- renderPlot({
  w4$show()
  req(input$predict_new_prediction_breast_cancer)
  isolate({
    if (import_data_type() != "Import pre-annotated dataset") {
      return(NULL)  # Return NULL to not display any plot for this case
    } else {
      p = predict_new_data()$plot
      if (is.null(p)) {
        return(NULL)  # Return NULL to not display any plot when the plot is NULL
      } else {
        auc_val = predict_new_data()$auc
        plot(p)
        # Add legend
        legend("bottomright", legend=paste0("AUC = ", auc_val), col="#2A5674", lty=1, cex=0.8)
      }
    }
  })
})

# Text output
output$results_text <- renderText({
  w3$show()
  req(input$predict_new_prediction_breast_cancer)
  isolate({
    if (import_data_type() != "Import pre-annotated dataset") {
      paste0("The patient has a ", predict_new_data()$resp_prob, " chance of responding to the chosen treatment type (", predict_new_data()$chosen_treatment, ").")
    } else {
      p = predict_new_data()$plot
      if (is.null(p)) {
        "No ROC plot could be generated."
      } else {
        NULL  # No text information to display when the plot is not NULL
      }
    }
  })
})

# Downloadable output
output$download_new_prediction_results <- downloadHandler(
  filename = function() {
    paste("results_", Sys.Date(), ".zip", sep = "")
  },
  content = function(con) {
    tmpdir <- tempdir()
    
    # Call the predict_new_data function once and store the result
    prediction_result <- predict_new_data()
    p <- prediction_result$plot
    auc_val <- prediction_result$auc
    
    # Always save the new data as an excel file
    newdata <- prediction_result$predictions
    if (!is.null(newdata)) {
      xlsx_path <- file.path(tmpdir, "new_data.xlsx")
      openxlsx::write.xlsx(newdata, xlsx_path)
    }
    
    # If a plot should be included, save it as a png file
    if (!is.null(p) && !is.null(auc_val)) {
      png(file.path(tmpdir, "plot.png"))
      plot(p)
      legend("bottomright", legend = paste0("AUC = ", auc_val), col = "#2A5674", lty = 1, cex = 0.8)
      dev.off()
    }
    
    # Save the current working directory
    owd <- getwd()
    
    # Change the working directory to the temporary directory before creating the zip file
    setwd(tmpdir)
    
    # Specify the files to be included in the zip archive by their names
    file_paths <- c()
    if (!is.null(newdata)) {
      file_paths <- c(file_paths, "new_data.xlsx")
    }
    if (!is.null(p) && !is.null(auc_val)) {
      file_paths <- c(file_paths, "plot.png")
    }
    
    # Now let's print the file_paths to the console to debug what is being zipped
    print(file_paths)
    
    # Create a zip file containing all the files (note that we're using relative paths now)
    zip::zip(zipfile = con, files = file_paths)
    
    # Change back to the original working directory after creating the zip file
    setwd(owd)
  }
)