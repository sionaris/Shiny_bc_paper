# This code is for my first approach to build a Shiny app for my PhD project
library(shiny)
library(shinydashboard)
library(plotly)
library(openxlsx)
library(colourpicker)
library(shinyBS)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
library(dplyr)
library(rlang)
library(stringr)
library(randomcoloR)
library(data.table)
library(pROC)
library(limma)
library(later)

# Load the required data #####

# Breast Cancer ###
breast_cancer_study_chars = openxlsx::read.xlsx("Study_characteristics.xlsx", sheet = 1)
breast_cancer_study_chars$Dataset = c("C1", "C2", "C3", "E1_1", "E1_2", "E1_3",
                                      "E2", "E3", "E4_1", "E4_2", "XVC1", "XVC2",
                                      "XVE", "XVC3")
breast_cancer_Pheno = openxlsx::read.xlsx("Pheno.xlsx", sheet = 1)
breast_cancer_DGEA = openxlsx::read.xlsx("DGEA.xlsx")
breast_cancer_extval = openxlsx::read.xlsx("Ext_val.xlsx")
breast_cancer_full_pheno = rbind(breast_cancer_Pheno, breast_cancer_extval)
breast_cancer_full_pheno = breast_cancer_full_pheno %>% 
  inner_join(breast_cancer_study_chars %>%
               dplyr::select(Dataset, Publication, Year, Samples, Patients,
                             Location, ER.status, Menopause.status,
                             `Age.(years)`, Design = `Timepoints`),
             by = "Dataset")

# Read in consensus data
breast_cancer_consensus_set = readRDS("annotation_for_heatmap.rds")

# Enrich consensus data
breast_cancer_consensus_set = breast_cancer_consensus_set %>% 
  mutate(Sample.ID = rownames(.)) %>%
  inner_join(breast_cancer_full_pheno[,c("Sample.ID", 
                                         setdiff(colnames(breast_cancer_full_pheno), 
                                                 colnames(breast_cancer_consensus_set)))],
             by = "Sample.ID")

# Fix Mammaprint column
breast_cancer_full_pheno$new_Mamma = NA
breast_cancer_full_pheno$new_Mamma[breast_cancer_full_pheno$Mammaprint_risk == 1] = "Risk"
breast_cancer_full_pheno$new_Mamma[breast_cancer_full_pheno$Mammaprint_risk == 0] = "No risk"
breast_cancer_full_pheno = breast_cancer_full_pheno %>%
  dplyr::select(-Mammaprint_risk) %>%
  dplyr::rename(Mammaprint_risk = new_Mamma)

# Read in ML models
RegLogR = readRDS("Reg_LogR.rds")
BackLogR = readRDS("Back_LogR.rds")
LogR = list(`Lasso-regularised` = RegLogR,
            Backward = BackLogR)
DT = readRDS("DT.rds")
SVM = readRDS("SVM.rds")
ML = list(`Logistic Regression` = c("Lasso-regularised", "Backward"),
          `Decision Trees` = DT,
          `Support Vector Machines` = SVM)
ML[["Logistic Regression"]] = LogR

# Read in ready-for-predictions data
train = readRDS("train_set.rds") 
validation = readRDS("validation_set.rds")
test = readRDS("test_set.rds")
extval = readRDS("ext_val_set.rds")
exprs_samples = c(train$Sample.ID, validation$Sample.ID)

ml_set = rbind(train, validation, test, extval)
full_ml_set = ml_set %>%
  inner_join(breast_cancer_full_pheno[, c("Sample.ID", "Dataset", "Location",
                                          "ER.status", "Menopause.status", 
                                          "pam50", "Mammaprint_risk", "rorS_risk",
                                          "Treatment", "Timepoint_coded", "ClaudinLow")], 
             by = "Sample.ID") %>%
  dplyr::rename(Timepoint = Timepoint_coded, Meno = Menopause.status) %>%
  mutate(Menopause.status = gsub("PM", "Post-menopausal", Meno)) %>%
  dplyr::select(-Meno)

# Read in expression matrix of training and validation samples
z_exprs = readRDS("normalised_expression.rds")
Pheno_exprs = breast_cancer_full_pheno[breast_cancer_full_pheno$Sample.ID %in% 
                                         exprs_samples,]
z_exprs = z_exprs[breast_cancer_DGEA$EntrezGene.ID,]
rownames(z_exprs) = breast_cancer_DGEA$Gene.Symbol

train_samples = train$Sample.ID; validation_samples = validation$Sample.ID
test_samples = test$Sample.ID; extval_samples = extval$Sample.ID
trainval_samples = c(train_samples, validation_samples)

rm(breast_cancer_Pheno, breast_cancer_extval, breast_cancer_study_chars,
   RegLogR, BackLogR, DT, SVM, LogR, ml_set, train, validation, test, extval,
   exprs_samples); gc()

# PDAC ###


# Header #####
header <- dashboardHeader(title = "My PhD project")

# Body #####
body <- dashboardBody(tabItems(
  # Add tab items for breast cancer, PDAC and OSCC
  # Breast Cancer - general tab
  tabItem(tabName = "breast_cancer_project"),
  
  # Breast Cancer - exploratory analysis #####
  tabItem(tabName = "breast_cancer_exploratory",
          h2("Exploratory analysis plots and tables"),
          fluidRow(
            
            # Histogram plot ##
            box(id = "Histogram_breast", title = "Histogram", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, width = 7, height = 400,
                plotlyOutput("breast_histogram")),
            
            # Histogram tuning
            box(id = "Histogram_tuning_breast", title = "Histogram tuning", status = "warning",
                solidHeader = TRUE, collapsible = TRUE, width = 5, height = 400,
                shinyjs::useShinyjs(),
                
                # Select variable to plot
                div(id = "div_histvar_select_breast", 
                style="display:inline-block; width: 32%; margin-left: 7.5%;
                    margin-right: 5%",
                    selectInput("histvar_select_breast", "Select variable", 
                            choices = sort(c("pam50", "scmod1",
                                        "ClaudinLow", "IC10", 
                                        "Mammaprint_risk", "Mammaprint_score",
                                        "rorS_risk", "rorS_score", "Location",
                                        "Menopause.status",
                                        "ER.status", "Year", "Platform",
                                        "Platform_comp", "Treatment", "Response"
                                        )),
                            selected = "pam50")),
                
                # Select type of histogram (probability, percentage, classic)
                div(id = "div_hist_type_breast", 
                style="display:inline-block; width: 38%; margin-left: 5%;
                    margin-right: 5%",
                    selectInput("hist_type_breast", "Select type of histogram",
                            choices = list(classic = "classic", probability = "probability",
                                        percent = "percentage"), 
                            selected = "classic")),
                
                # div for checkbox input and select-all button
                div(id = "div_checkbox_selectall_group_breast_hist", 
                    style = "width: 100%;",
                # Select the datasets that you want to include in the plot
                div(id = "div_hist_dataset_checkbox",
                    style = "display: inline-block; width: 55%; margin-left: 7.5%;",
                    checkboxGroupInput("hist_dataset_checkbox", "Select studies",
                              selected = unique(breast_cancer_full_pheno$Dataset), 
                              choices = unique(breast_cancer_full_pheno$Dataset),
                              inline = TRUE)),
                
                # add "select all" button for the datasets to choose from
                div(id = "div_select_all_hist_breast",
                style = "display: inline-block; margin: 5% 5% 0% 5%; width: 25%; 
                position: absolute;",
                    actionButton("select_all_hist_breast", "Select/De-select All",
                                 style = "background-color: #F8ECBB; font-weight: bold;
                                 border-radius: 10px;"))),
                
                # Select colors for fill and outline of the bins
                div(id = "div_hist_fill_breast",
                style="display:inline-block; margin-top: 0%; margin-left: 7.5%; 
                    margin-right: 2.5%; width: 25%;",
                    colourpicker::colourInput("hist_fill_breast", "Bin fill color", 
                                          value = "#1194B1", allowTransparent = TRUE)),
                div(id = "div_hist_color_breast",
                    style="display:inline-block; margin-left: 2.5%; margin-right: 2.5%; width: 25%",
                    colourpicker::colourInput("hist_color_breast", "Bin outline color", 
                                          value = "#000000", allowTransparent = TRUE)),
                
                # Select number of bins
                div(id = "div_hist_breast_bins",
                    style="display:inline-block; margin-left: 2.5%; margin-right: 5%; width: 25%",
                    numericInput("hist_breast_bins", "Select # of bins",
                                 value = 10, min = 0, max = Inf, width = "150px")),
                
                # div for action buttons
                div(id = "div_action_buttons_hist_breast", style = "display: flex;
                width: 100%; align-content: center; justify-content: center",
                # Button to trigger plot generation/update
                div(id = "div_draw_breast_hist",
                    # style = "display: flex; margin-right: 3%; margin-top: 3%;",
                    tags$head(tags$link(rel = "stylesheet", type = "text/css",
                                        href = "rgb_button_css.css")),
                    actionButton(inputId = "draw_breast_hist", label = "Draw!") %>% 
                      tagAppendAttributes(class = 'rgb-button')),
                
                # Button to reset inputs to default
                div(id = "div_default_breast_hist",
                    # style = "display: flex; margin-right: 3%; margin-left: 3%; margin-top: 3%;",
                    tags$head(tags$link(rel = "stylesheet", type = "text/css",
                                        href = "default_button.css")),
                    actionButton(inputId = "reset_input_breast_hist", 
                             label = "Default parameters") %>%
                      tagAppendAttributes(class = 'default-button')),
                
                # Add an info button (pop up with shinyalert())
                div(id = "div_info_breast_hist",
                    # style = "display: inline-flex; margin-left: 3%; margin-top: 3%;",
                    tags$head(tags$link(rel = "stylesheet", type = "text/css",
                                        href = "info_button.css")),
                actionButton(inputId = "hist_breast_info",
                             label = "Info") %>%
                  tagAppendAttributes(class = 'info-button')))),
            
            bsModal("histogram_breast_info", "Information", "hist_breast_info",
                    fluidRow(
                      htmlOutput("hist_breast_info_text")
                    )),
            
            # Bar chart ##
            box(id = "Barchart_plot_breast", title = "Bar chart", status = "primary",
                solidHeader = TRUE, collapsible = TRUE, width = 7, height = 400,
                plotlyOutput("breast_barchart")),
            
            # Bar chart tuning
            box(id = "Barchart_plot_tuning_breast", title = "Bar chart tuning", status = "warning",
                solidHeader = TRUE, collapsible = TRUE, width = 5, height = 400,
                shinyjs::useShinyjs(),
                
                # Select main variable to plot
                div(id = "div_barchart_varmain_select_breast", 
                    style="display:inline-block; width: 27.5%; margin-left: 3%;
                    margin-right: 2.5%",
                    selectInput("barchart_varmain_select_breast", "Main variable", 
                                choices = sort(c("pam50", "scmod1",
                                                 "ClaudinLow", "IC10", 
                                                 "Mammaprint_risk",
                                                 "rorS_risk", "Location",
                                                 "Menopause.status",
                                                 "ER.status", "Year", "Platform",
                                                 "Platform_comp", "Treatment", "Response"
                                )),
                                selected = "pam50")),
                
                # Select clustering variable (optional)
                div(id = "div_barchart_varclust_select_breast", 
                    style="display:inline-block; width: 27.5%; margin-left: 2.5%;
                    margin-right: 2.5%",
                    selectInput("barchart_varclust_select_breast", "Second variable (optional)", 
                                choices = c("None", sort(c("pam50", "scmod1",
                                                           "ClaudinLow", "IC10", 
                                                           "Mammaprint_risk",
                                                           "rorS_risk", "Location",
                                                           "Menopause.status",
                                                           "ER.status", "Year", "Platform",
                                                           "Platform_comp", "Treatment", "Response"
                                ))),
                                selected = "None")),
                
                # Select type of bar chart (clustered, stacked) - if a second variable is selected
                div(id = "div_barchart_type_breast", 
                    style="display:inline-block; width: 27.5%; margin-left: 2.5%;
                    margin-right: 3%",
                    selectInput("barchart_type_breast", "Select type of bar chart",
                                choices = c("group", "stack"), 
                                selected = "group")),
                
                # div for checkbox input and select-all button
                div(id = "div_checkbox_selectall_group_breast_barchart", 
                    style = "width: 100%;",
                    # Select the datasets that you want to include in the plot
                    div(id = "div_barchart_dataset_checkbox",
                        style = "display: inline-block; width: 55%; margin-left: 3%;",
                        checkboxGroupInput("barchart_dataset_checkbox", "Select studies",
                                           selected = unique(breast_cancer_full_pheno$Dataset), 
                                           choices = unique(breast_cancer_full_pheno$Dataset),
                                           inline = TRUE)),
                    
                    # add "select all" button for the datasets to choose from
                    div(id = "div_select_all_barchart_breast",
                        style = "display: inline-block; margin: 5% 3% 0% 7%; width: 25%; 
                position: absolute;",
                        actionButton("select_all_barchart_breast", "Select/De-select All",
                                     style = "background-color: #F8ECBB; font-weight: bold;
                                 border-radius: 10px;"))),
                
                # div for color and fill selections for barchart
                div(id = "div_fill_color_group_barachart",
                    style = "display: flex; justify-content: center; align-content:center;",
                # Select colors for fill and outline of the bins
                div(id = "div_barchart_fill_breast",
                    style="display:inline-block; margin-top: 0%; margin-left: 3%; 
                    margin-right: 7.5%; width: 35%;",
                    colourpicker::colourInput("barchart_fill_breast", "Bin fill color", 
                                              value = "#9419B2", allowTransparent = TRUE)),
                div(id = "div_barchart_color_breast",
                    style="display:inline-block; margin-left: 7.5%; margin-right: 3%; width: 35%",
                    colourpicker::colourInput("barchart_color_breast", "Bin outline color", 
                                              value = "#000000", allowTransparent = TRUE))),
                
                # div for action buttons
                div(id = "div_action_buttons_hist_breast", style = "display: flex;
                width: 100%; align-content: center; justify-content: center",
                    # Button to trigger plot generation/update
                    div(id = "div_draw_breast_hist",
                        # style = "display: flex; margin-right: 3%; margin-top: 3%;",
                        tags$head(tags$link(rel = "stylesheet", type = "text/css",
                                            href = "rgb_button_css.css")),
                        actionButton(inputId = "draw_breast_barchart", label = "Draw!") %>% 
                          tagAppendAttributes(class = 'rgb-button')),
                    
                    # Button to reset inputs to default
                    div(id = "div_default_breast_hist",
                        # style = "display: flex; margin-right: 3%; margin-left: 3%; margin-top: 3%;",
                        tags$head(tags$link(rel = "stylesheet", type = "text/css",
                                            href = "default_button.css")),
                        actionButton(inputId = "reset_input_breast_barchart", 
                                     label = "Default parameters") %>%
                          tagAppendAttributes(class = 'default-button')),
                    
                    # Add an info button (pop up with shinyalert())
                    div(id = "div_info_breast_hist",
                        # style = "display: inline-flex; margin-left: 3%; margin-top: 3%;",
                        tags$head(tags$link(rel = "stylesheet", type = "text/css",
                                            href = "info_button.css")),
                        actionButton(inputId = "barchart_breast_info",
                                     label = "Info") %>%
                          tagAppendAttributes(class = 'info-button')))),
            
            bsModal("barchart_breast_information", "Information", "barchart_breast_info",
                    fluidRow(
                      htmlOutput("barchart_breast_info_text")
                    ))
          )),
  
  # Breast Cancer - sunburst tab #####
  tabItem(tabName = "breast_cancer_sunburst",
    h2("Sunburst plots: analytical dataset and consensus subtypes"),
    fluidRow(
      # tabBox of plotting sunbursts
      tabBox(
        title = "Sunburst plots", id = "sunburst_tabs_breast", height = 700,
        width = 7, selected = "Analytical data sunburst plot",
        
        # Panel for analytical set
        tabPanel("Analytical data sunburst plot", title = "Analytical dataset",
                 plotlyOutput("analytical_sunburst_breast")),
        
        # Panel for consensus set
        tabPanel("Consensus data sunburst plot", title = "Consensus subtypes",
                 plotlyOutput("consensus_sunburst_breast"))
      ),
      
      # tabBox of tuning sunbursts
      tabBox(
        title = "Sunburst tuning", id = "sunburst_tuning_breast", height = 700,
        width = 5, selected = "Analytical tuning",
        
        # Panel for analytical set
        tabPanel("Analytical tuning", title = "Analytical dataset",
                 shinyjs::useShinyjs(),
                 
                 # Select variable 1 for analytical set
                 selectInput("sunburst_var1_select_breast", "Select root variable", 
                             choices = sort(c("pam50", "scmod1",
                                              "ClaudinLow", Timepoint = "Timepoint_coded", 
                                              "Mammaprint_risk",
                                              "rorS_risk", "Location",
                                              "Menopause.status", "Dataset", "None",
                                              "ER.status", "Treatment", "Response"
                             )),
                             selected = "Treatment"),
                 
                 # Select variable 2 for analytical set
                 selectInput("sunburst_var2_select_breast", "Select second variable", 
                             choices = sort(c("pam50", "scmod1",
                                              "ClaudinLow", Timepoint = "Timepoint_coded", 
                                              "Mammaprint_risk",
                                              "rorS_risk", "Location",
                                              "Menopause.status", "Dataset", "None",
                                              "ER.status", "Treatment", "Response"
                             )),
                             selected = "Dataset"),
                 
                 # Select variable 3 for analytical set
                 selectInput("sunburst_var3_select_breast", "Select third variable", 
                             choices = sort(c("pam50", "scmod1",
                                              "ClaudinLow", Timepoint = "Timepoint_coded", 
                                              "Mammaprint_risk",
                                              "rorS_risk", "Location",
                                              "Menopause.status", "Dataset", "None",
                                              "ER.status", "Treatment", "Response"
                             )),
                             selected = "Response"),
                 
                 # Select variable 4 for analytical set
                 selectInput("sunburst_var4_select_breast", "Select fourth variable", 
                             choices = sort(c("pam50", "scmod1",
                                              "ClaudinLow", Timepoint = "Timepoint_coded", 
                                              "Mammaprint_risk",
                                              "rorS_risk", "Location",
                                              "Menopause.status", "Dataset", "None",
                                              "ER.status", "Treatment", "Response"
                             )),
                             selected = "pam50"),
                 
                 # add "select all" button for the datasets to choose from
                 actionButton("select_all_sunburst_analytical_breast", "Select/De-select All"),
                 
                 # Select the datasets that you want to include in the plot
                 checkboxGroupInput("sunburst_analytical_dataset_checkbox", "Select studies",
                                    selected = unique(breast_cancer_full_pheno$Dataset), 
                                    choices = unique(breast_cancer_full_pheno$Dataset),
                                    inline = TRUE),
                 
                 # Select opacity of data points
                 sliderInput("alpha_breast_sunburst_analytical", "Opacity (alpha)",
                             min = 0, max = 1, value = 0.9),
                 
                 # Button to trigger plot generation/update
                 actionButton(inputId = "draw_breast_sunburst_analytical", 
                              label = "Draw!"),
                 
                 # Button to reset inputs to default
                 actionButton(inputId = "reset_input_breast_sunburst_analytical", 
                              label = "Default parameters"),
                 
                 # Add an info button (pop up with shinyalert())
                 actionButton(inputId = "sunburst_analytical_breast_info",
                              label = "Info"),
                 
                 bsModal("analytical_sunburst_breast_info", "Information", "sunburst_analytical_breast_info",
                         fluidRow(
                           htmlOutput("analytical_sunburst_breast_info_text")
                         ))),
      
        # Panel for consensus set
        tabPanel(id = "Consensus tuning", title = "Consensus subtypes",
                 shinyjs::useShinyjs(),
                 
                 # Select variable 1 for consensus set
                 selectInput("sunburst_var1_select_breast_consensus", 
                             "Select root variable", 
                             choices = sort(c("pam50", "scmod1",
                                              "ClaudinLow", "Timepoint", 
                                              "Mammaprint_risk",
                                              "rorS_risk", "Location",
                                              "Menopause.status", "Dataset", "None",
                                              "ER.status", "Treatment", "Response",
                                              "Cluster"
                             )),
                             selected = "Cluster"),
                 
                 # Select variable 2 for consensus set
                 selectInput("sunburst_var2_select_breast_consensus", 
                             "Select second variable", 
                             choices = sort(c("pam50", "scmod1",
                                              "ClaudinLow", "Timepoint", 
                                              "Mammaprint_risk",
                                              "rorS_risk", "Location",
                                              "Menopause.status", "Dataset", "None",
                                              "ER.status", "Treatment", "Response",
                                              "Cluster"
                             )),
                             selected = "Dataset"),
                 
                 # Select variable 3 for consensus set
                 selectInput("sunburst_var3_select_breast_consensus", 
                             "Select third variable", 
                             choices = sort(c("pam50", "scmod1",
                                              "ClaudinLow", "Timepoint", 
                                              "Mammaprint_risk",
                                              "rorS_risk", "Location",
                                              "Menopause.status", "Dataset", "None",
                                              "ER.status", "Treatment", "Response",
                                              "Cluster"
                             )),
                             selected = "Response"),
                 
                 # Select variable 4 for consensus set
                 selectInput("sunburst_var4_select_breast_consensus", 
                             "Select fourth variable", 
                             choices = sort(c("pam50", "scmod1",
                                              "ClaudinLow", "Timepoint", 
                                              "Mammaprint_risk",
                                              "rorS_risk", "Location",
                                              "Menopause.status", "Dataset", "None",
                                              "ER.status", "Treatment", "Response",
                                              "Cluster"
                             )),
                             selected = "Mammaprint_risk"),
                 
                 # add "select all" button for the datasets to choose from
                 actionButton("select_all_sunburst_consensus_breast", "Select/De-select All"),
                 
                 # Select the datasets that you want to include in the plot
                 checkboxGroupInput("sunburst_consensus_dataset_checkbox", "Select studies",
                                    selected = unique(breast_cancer_consensus_set$Dataset), 
                                    choices = unique(breast_cancer_consensus_set$Dataset),
                                    inline = TRUE),
                 
                 # Select opacity of data points
                 sliderInput("alpha_breast_sunburst_consensus", "Opacity (alpha)",
                             min = 0, max = 1, value = 0.9),
                 
                 # Button to trigger plot generation/update
                 actionButton(inputId = "draw_breast_sunburst_consensus", 
                              label = "Draw!"),
                 
                 # Button to reset inputs to default
                 actionButton(inputId = "reset_input_breast_sunburst_consensus", 
                              label = "Default parameters"),
                 
                 # Add an info button (pop up with shinyalert())
                 actionButton(inputId = "sunburst_consensus_breast_info",
                              label = "Info"),
                 bsModal("consensus_sunburst_breast_info", "Information", "sunburst_consensus_breast_info",
                         fluidRow(
                           htmlOutput("consensus_sunburst_breast_info_text")
                         )))
        
        
      )
    )
  ),
  
  # Breast Cancer - volcano #####
  tabItem(tabName = "breast_cancer_subItem_volcano",
          h2("Neoadjuvant Treatment (NAT) in Breast Cancer"),
          fluidRow(
            # Volcano plot
            box(id = "Volcano_plot_breast", title = "Volcano plot", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width = 9, height = 800,
                plotlyOutput("breast_volcano")),
            
            # Interactive switches and tools for plotting the volcano
            box(id = "Volcano_tuning_breast", title = "Volcano tuning", status = "warning", solidHeader = TRUE,
                collapsible = TRUE, width = 3, height = 800, 
                shinyjs::useShinyjs(), 
                
                # Select p-value for y-axis (corrected/uncorrected)
                selectInput("pval_selector_breast", "p-value to plot", 
                            choices = c("P.Value", "adj.P.Val"),
                            selected = "adj.P.Val"),
                
                # Select x-coordinate for vertical logFC lines
                numericInput("logFC_breast", 
                "DET: Diff. Exp. Threshold - vertical lines", value = 0.25, min = 0,
                max = Inf),
                
                # Select y-coordinate for p-value threshold
                numericInput("pval_breast", 
                             "PVT (adj.P.Val threshold) - horizontal line", value = 0.05, min = 0,
                             max = Inf),
                
                # Select colors for scatter points
                colourpicker::colourInput("scatter_color_breast_NS", "Color for p>PVT (n.s. results)", 
                          value = "#BEBEBE"),
                colourpicker::colourInput("scatter_color_breast_DE_only", "Color for DE>DET (p>PVT)", 
                      value = "#FFC0CB"),
                colourpicker::colourInput("scatter_color_breast_p_only", "Color for p<PVT (DE<DET)", 
                          value = "#551A8B"),
                colourpicker::colourInput("scatter_color_breast_DE_p", "Color for DE>DET & p<PVT", 
                          value = "#8B0000"),
                
                # Select opacity of data points
                sliderInput("alpha_breast_volcano", "Opacity (alpha)",
                            min = 0, max = 1, value = 0.7),
                
                # Write down a representative title for the plot
                textInput("breast_volcano_title", "Set a title for your plot",
                          value = c("Volcano plot: NAT responders vs. non-responders")),
                
                # Button to trigger plot generation/update
                actionButton(inputId = "draw_breast_volcano", label = "Draw!"),
                
                # Button to reset inputs to default
                actionButton(inputId = "reset_input_breast_volcano", 
                             label = "Default parameters"),
                
                # Add an info button (pop up with shinyalert())
                actionButton(inputId = "volcano_breast_info",
                             label = "Info"),
                
                bsModal("preset_volcano_breast_info", "Information", "volcano_breast_info",
                        fluidRow(
                          htmlOutput("volcano_breast_info_text")
                        ))),
            
            # Hovering info in tuning parameters
            bsTooltip("pval_selector_breast", 
                      "Select p-value for plotting on y-axis (-log10(p-value))", 
                      placement = "bottom", trigger = "hover", options = NULL),
            bsTooltip("logFC_breast", 
                      "x-coordinate for symmetrical vertical lines (illustrative purposes)", 
                      placement = "bottom", trigger = "hover", options = NULL),
            bsTooltip("pval_breast", 
                      "y-coordinate for horizontal p-value threshold (default:0.05)", 
                      placement = "bottom", trigger = "hover", options = NULL)
          )),
  
  # Breast Cancer - Machine Learning (Response prediction models) #####
  tabItem(tabName = "breast_cancer_subItem_ml",
          h2("Machine Learning"),
          fluidRow(
            
            # Interactive model and data selection panel
            box(id = "ML_control_breast", title = "Select model and data", 
                status = "primary", solidHeader = TRUE,
                width = 12, height = 270, shinyjs::useShinyjs(), 
                
                splitLayout(cellWidths = c("200px", "250px", "350px", "150px",
                                           "100px", "80px"),
                            style = "border:1px;padding:10px;white-space:normal;",
                            # Select ML model category
                            radioButtons(inputId = "breast_cancer_ml_model_category",
                                         label = "Select model category",
                                         choices = names(ML), selected = "Decision Trees",
                                         width = "150px"),
                            
                            # Updatable radioButtons for model subcategory
                            radioButtons("breast_cancer_ml_model_subcategory",
                                         "Select model subcategory",
                                         choices = sort(names(ML[["Decision Trees"]])),
                                         selected = "C5.0 - ROC", width = "250px"),
                            
                            # Select the datasets that you want to include in the plot
                            checkboxGroupInput("ml_breast_dataset_checkbox", "Select studies",
                                               selected = unique(full_ml_set$Dataset), 
                                               choices = unique(full_ml_set$Dataset),
                                               inline = TRUE, width = "300px"),
                            
                            # add "select all" button for the datasets to choose from
                            actionButton("select_all_ml_model1_breast", "Select/De-select All")
                )
            ),
          # Filtering the subset of the data based on variables for more specific prediction
            
          # A wide box with checkboxes
          box(id = "ML_breast_cancer_variable_filtering", title = "Variable filtering", 
              status = "primary", solidHeader = TRUE,
              width = 12, height = 300, shinyjs::useShinyjs(), 
              
              splitLayout(cellWidths = c("180px", "100px", "90px", "90px",
                                         "130px", "130px", "150px", "120px", "200px",
                                         "80px", "80px", "150px", "80px"),
                          style = "border:1px;padding:10px;white-space:normal;",
                          
                          # Filter for treatment
                          checkboxGroupInput("breast_cancer_ml_treatment_filter", "Treatment",
                                             selected = unique(full_ml_set$Treatment),
                                             choices = unique(full_ml_set$Treatment),
                                             inline = FALSE, width = "200px"),
                          
                          # Filter for timepoint
                          checkboxGroupInput("breast_cancer_ml_timepoint_filter", "Timepoint",
                                             selected = unique(full_ml_set$Timepoint),
                                             choices = unique(full_ml_set$Timepoint),
                                             inline = FALSE, width = "180px"),
                          
                          # Filter for location
                          checkboxGroupInput("breast_cancer_ml_location_filter", "Location",
                                             selected = unique(full_ml_set$Location),
                                             choices = unique(full_ml_set$Location),
                                             inline = FALSE, width = "150px"),
                          
                          # Filter for pam50
                          checkboxGroupInput("breast_cancer_ml_pam50_filter", "pam50",
                                             selected = unique(full_ml_set$pam50),
                                             choices = unique(full_ml_set$pam50),
                                             inline = FALSE, width = "150px"),
                          
                          # Filter for rorS risk
                          checkboxGroupInput("breast_cancer_ml_rorS_filter", "Risk of recurrence",
                                             selected = unique(full_ml_set$rorS_risk),
                                             choices = unique(full_ml_set$rorS_risk),
                                             inline = FALSE, width = "200px"),
                          
                          # Filter for Mammaprint risk
                          checkboxGroupInput("breast_cancer_ml_Mammaprint_filter", "Mammaprint risk",
                                             selected = unique(full_ml_set$Mammaprint_risk),
                                             choices = unique(full_ml_set$Mammaprint_risk),
                                             inline = FALSE, width = "200px"),
                          
                          # Filter for Menopause status
                          checkboxGroupInput("breast_cancer_ml_Meno_filter", "Menopause",
                                             selected = unique(full_ml_set$Menopause.status),
                                             choices = unique(full_ml_set$Menopause.status),
                                             inline = FALSE, width = "200px"),
                          
                          # Filter for ER status
                          checkboxGroupInput("breast_cancer_ml_ER_filter", "ER status",
                                             selected = unique(full_ml_set$ER.status),
                                             choices = unique(full_ml_set$ER.status),
                                             inline = FALSE, width = "100px"),
                          
                          # legend title input
                          textInput(inputId = "breast_cancer_ml_legend_entry_1",
                                    "Name for legend", value = "Model 1"),
                          
                          # add "Predict" - button that triggers the prediction model on the selected data
                          actionButton("predict_ml_breast_cancer", "Predict!",
                                       style = "color: #FFFFFF; background-color: #1986B2; border-color: #2e6da4"),
                          
                          # Button to reset inputs to default
                          actionButton(inputId = "reset_input_breast_ml_model1", 
                                       label = "Reset",
                                       style = "color: #FFFFFF; background-color: #000000; border-color: #2e6da4"),
                          
                          # add "Comparison" button
                          actionButton("multiple_rocs_breast", "Add comparison"),
                          
                          # add "Info" button
                          actionButton("info_ml_breast_cancer", "Info"),
                          
                          bsModal("ml_breast_info", "Information", "info_ml_breast_cancer",
                                  fluidRow(
                                    htmlOutput("ml_breast_info_text")
                                  ))
                          
              )
          ),
            # Model selection box for comparison #2
            hidden(div(id = "model_select_breast_2",
                       box(id = "ML_control_breast_2", title = "Select model and data #2", 
                           status = "primary", solidHeader = TRUE,
                           width = 12, height = 500, shinyjs::useShinyjs(), 
                           
                           splitLayout(cellWidths = c("200px", "250px", "350px", "350px",
                                                      "180px", "100px", "90px", "90px",
                                                      "130px", "130px", "150px", "120px", "200px", 
                                                      "80px", "80px", "200px"),
                                       style = "border:1px;padding:10px;white-space:normal;",
                                       # Select ML model category
                                       radioButtons(inputId = "breast_cancer_ml_model_category_2",
                                                    label = "Select model category",
                                                    choices = names(ML), selected = "Decision Trees",
                                                    width = "150px"),
                                       
                                       # Updatable radioButtons for model subcategory
                                       radioButtons("breast_cancer_ml_model_subcategory_2",
                                                    "Select model subcategory",
                                                    choices = sort(names(ML[["Decision Trees"]])),
                                                    selected = "C5.0 - ROC", width = "250px"),
                                       
                                       # Select the datasets that you want to include in the plot
                                       checkboxGroupInput("ml_breast_dataset_checkbox_2", "Select studies",
                                                          selected = unique(full_ml_set$Dataset), 
                                                          choices = unique(full_ml_set$Dataset),
                                                          inline = TRUE, width = "300px"),
                                       
                                       # add "select all" button for the datasets to choose from
                                       actionButton("select_all_ml_model2_breast", "Select/De-select All"),
                                       
                                       # Filter for treatment
                                       checkboxGroupInput("breast_cancer_ml_treatment_filter_2", "Treatment",
                                                          selected = unique(full_ml_set$Treatment),
                                                          choices = unique(full_ml_set$Treatment),
                                                          inline = FALSE, width = "200px"),
                                       
                                       # Filter for timepoint
                                       checkboxGroupInput("breast_cancer_ml_timepoint_filter_2", "Timepoint",
                                                          selected = unique(full_ml_set$Timepoint),
                                                          choices = unique(full_ml_set$Timepoint),
                                                          inline = FALSE, width = "180px"),
                                       
                                       # Filter for location
                                       checkboxGroupInput("breast_cancer_ml_location_filter_2", "Location",
                                                          selected = unique(full_ml_set$Location),
                                                          choices = unique(full_ml_set$Location),
                                                          inline = FALSE, width = "150px"),
                                       
                                       # Filter for pam50
                                       checkboxGroupInput("breast_cancer_ml_pam50_filter_2", "pam50",
                                                          selected = unique(full_ml_set$pam50),
                                                          choices = unique(full_ml_set$pam50),
                                                          inline = FALSE, width = "150px"),
                                       
                                       # Filter for rorS risk
                                       checkboxGroupInput("breast_cancer_ml_rorS_filter_2", "Risk of recurrence",
                                                          selected = unique(full_ml_set$rorS_risk),
                                                          choices = unique(full_ml_set$rorS_risk),
                                                          inline = FALSE, width = "200px"),
                                       
                                       # Filter for Mammaprint risk
                                       checkboxGroupInput("breast_cancer_ml_Mammaprint_filter_2", "Mammaprint risk",
                                                          selected = unique(full_ml_set$Mammaprint_risk),
                                                          choices = unique(full_ml_set$Mammaprint_risk),
                                                          inline = FALSE, width = "200px"),
                                       
                                       # Filter for Menopause status
                                       checkboxGroupInput("breast_cancer_ml_Meno_filter_2", "Menopause",
                                                          selected = unique(full_ml_set$Menopause.status),
                                                          choices = unique(full_ml_set$Menopause.status),
                                                          inline = FALSE, width = "200px"),
                                       
                                       # Filter for ER status
                                       checkboxGroupInput("breast_cancer_ml_ER_filter_2", "ER status",
                                                          selected = unique(full_ml_set$ER.status),
                                                          choices = unique(full_ml_set$ER.status),
                                                          inline = FALSE, width = "100px"),
                                       
                                       # legend title input
                                       textInput(inputId = "breast_cancer_ml_legend_entry_2",
                                                 "Name for legend", value = "Model 2"),
                                       
                                       # add "Apply" trigger button
                                       actionButton("apply_comparison_breast_2", "Apply",
                                                    style = "color: #FFFFFF; background-color: #7A0799; border-color: #2e6da4"),
                                       
                                       # Button to reset inputs to default
                                       actionButton(inputId = "reset_input_breast_ml_model2", 
                                                    label = "Reset",
                                                    style = "color: #FFFFFF; background-color: #000000; border-color: #2e6da4"),
                                       
                                       # add "Comparison" button
                                       actionButton("multiple_rocs_breast_2", "Add comparison")
                                       
                           )
                       ))),
          
          # Model selection box for comparison #3
          hidden(div(id = "model_select_breast_3",
                     box(id = "ML_control_breast_3", title = "Select model and data #3", 
                         status = "primary", solidHeader = TRUE,
                         width = 12, height = 500, shinyjs::useShinyjs(), 
                         
                         splitLayout(cellWidths = c("200px", "250px", "350px", "350px",
                                                    "180px", "100px", "90px", "90px",
                                                    "130px", "130px", "150px", "120px", "200px", 
                                                    "80px", "80px"),
                                     style = "border:1px;padding:10px;white-space:normal;",
                                     # Select ML model category
                                     radioButtons(inputId = "breast_cancer_ml_model_category_3",
                                                  label = "Select model category",
                                                  choices = names(ML), selected = "Decision Trees",
                                                  width = "150px"),
                                     
                                     # Updatable radioButtons for model subcategory
                                     radioButtons("breast_cancer_ml_model_subcategory_3",
                                                  "Select model subcategory",
                                                  choices = sort(names(ML[["Decision Trees"]])),
                                                  selected = "C5.0 - ROC", width = "250px"),
                                     
                                     # Select the datasets that you want to include in the plot
                                     checkboxGroupInput("ml_breast_dataset_checkbox_3", "Select studies",
                                                        selected = unique(full_ml_set$Dataset), 
                                                        choices = unique(full_ml_set$Dataset),
                                                        inline = TRUE, width = "300px"),
                                     
                                     # add "select all" button for the datasets to choose from
                                     actionButton("select_all_ml_model3_breast", "Select/De-select All"),
                                     
                                     # Filter for treatment
                                     checkboxGroupInput("breast_cancer_ml_treatment_filter_3", "Treatment",
                                                        selected = unique(full_ml_set$Treatment),
                                                        choices = unique(full_ml_set$Treatment),
                                                        inline = FALSE, width = "200px"),
                                     
                                     # Filter for timepoint
                                     checkboxGroupInput("breast_cancer_ml_timepoint_filter_3", "Timepoint",
                                                        selected = unique(full_ml_set$Timepoint),
                                                        choices = unique(full_ml_set$Timepoint),
                                                        inline = FALSE, width = "180px"),
                                     
                                     # Filter for location
                                     checkboxGroupInput("breast_cancer_ml_location_filter_3", "Location",
                                                        selected = unique(full_ml_set$Location),
                                                        choices = unique(full_ml_set$Location),
                                                        inline = FALSE, width = "150px"),
                                     
                                     # Filter for pam50
                                     checkboxGroupInput("breast_cancer_ml_pam50_filter_3", "pam50",
                                                        selected = unique(full_ml_set$pam50),
                                                        choices = unique(full_ml_set$pam50),
                                                        inline = FALSE, width = "150px"),
                                     
                                     # Filter for rorS risk
                                     checkboxGroupInput("breast_cancer_ml_rorS_filter_3", "Risk of recurrence",
                                                        selected = unique(full_ml_set$rorS_risk),
                                                        choices = unique(full_ml_set$rorS_risk),
                                                        inline = FALSE, width = "200px"),
                                     
                                     # Filter for Mammaprint risk
                                     checkboxGroupInput("breast_cancer_ml_Mammaprint_filter_3", "Mammaprint risk",
                                                        selected = unique(full_ml_set$Mammaprint_risk),
                                                        choices = unique(full_ml_set$Mammaprint_risk),
                                                        inline = FALSE, width = "200px"),
                                     
                                     # Filter for Menopause status
                                     checkboxGroupInput("breast_cancer_ml_Meno_filter_3", "Menopause",
                                                        selected = unique(full_ml_set$Menopause.status),
                                                        choices = unique(full_ml_set$Menopause.status),
                                                        inline = FALSE, width = "200px"),
                                     
                                     # Filter for ER status
                                     checkboxGroupInput("breast_cancer_ml_ER_filter_3", "ER status",
                                                        selected = unique(full_ml_set$ER.status),
                                                        choices = unique(full_ml_set$ER.status),
                                                        inline = FALSE, width = "100px"),
                                     
                                     # legend title input
                                     textInput(inputId = "breast_cancer_ml_legend_entry_3",
                                               "Name for legend", value = "Model 3"),
                                     
                                     # add "Apply" trigger button
                                     actionButton("apply_comparison_breast_3", "Apply",
                                                  style = "color: #FFFFFF; background-color: #7A0799; border-color: #2e6da4"),
                                     
                                     # Button to reset inputs to default
                                     actionButton(inputId = "reset_input_breast_ml_model3", 
                                                  label = "Reset",
                                                  style = "color: #FFFFFF; background-color: #000000; border-color: #2e6da4")
                                     
                         )
                     )))
          ),
          # ROC plot, confusion matrix and error metrics section
          bsModal("ML_output_breast", "Output", "predict_ml_breast_cancer",
                  size = "large", fluidRow(
            column(
              width = 12,
              
              # A box to plot the ROC curve
              box(
                title = "Model performance", id = "breast_cancer_roc_box",
                status = "warning", width = 12, height = 600, solidHeader = TRUE, align = "center",
                plotOutput("breast_cancer_ROC_plot", width = 500, height = 500)
              )
            ),
            
            column(
              width = 12,
              
              # Error metrics
              box(
                title = "Error Metrics", width = 12, solidHeader = TRUE,
                status = "warning", id = "breast_cancer_error_metrics",
                DT::dataTableOutput("breast_cancer_error_table")
              )
            )
          ))
  ),
  
  # Breast Cancer - Custom DGEA #####
  tabItem(tabName = "breast_cancer_subItem_DGEA",
          h2("Customized Differential Gene Expression Analysis"),
          fluidRow(
            
            box(id = "Custom_DGEA_control_panel", title = "Define DGEA subset", 
                status = "primary", solidHeader = TRUE,
                width = 12, height = 600, shinyjs::useShinyjs(), 
                
                splitLayout(cellWidths = c("350px", "300px", "500px",
                                           "200px", "140px", "130px", "130px",
                                           "150px", "150px", "150px", "150px", 
                                           "300px", 
                                           "200px", "200px", "200px",
                                           "350px", "350px", "350px",
                                           "190px", "190px", "190px", "190px", "190px",
                                           "80px", "70px", "80px"),
                            style = "border:1px;padding:20px;white-space:normal;",
                            
                            # Select the datasets that you want to include in the analysis
                            checkboxGroupInput("breast_custom_DGEA_dataset_checkbox", "Select studies",
                                               selected = unique(Pheno_exprs$Dataset), 
                                               choices = unique(Pheno_exprs$Dataset),
                                               inline = TRUE, width = "300px"),
                            
                            # add "select all" button for the datasets to choose from
                            actionButton("select_all_breast_custom_DGEA", "Select/De-select All"),
                            
                            # Title input
                            textInput(inputId = "breast_custom_DGEA_title_input",
                                      "Volcano plot title", value = "Responders vs. Non-responders"),
                            
                            # Filter for treatment
                            checkboxGroupInput("breast_custom_DGEA_treatment_filter", "Treatment",
                                               selected = unique(Pheno_exprs$Treatment),
                                               choices = unique(Pheno_exprs$Treatment),
                                               inline = FALSE, width = "200px"),
                            
                            # Filter for timepoint
                            checkboxGroupInput("breast_custom_DGEA_timepoint_filter", "Timepoint",
                                               selected = unique(Pheno_exprs$Timepoint_coded),
                                               choices = unique(Pheno_exprs$Timepoint_coded),
                                               inline = FALSE, width = "180px"),
                            
                            # Filter for location
                            checkboxGroupInput("breast_custom_DGEA_location_filter", "Location",
                                               selected = unique(Pheno_exprs$Location),
                                               choices = unique(Pheno_exprs$Location),
                                               inline = FALSE, width = "150px"),
                            
                            # Filter for pam50
                            checkboxGroupInput("breast_custom_DGEA_pam50_filter", "pam50",
                                               selected = unique(Pheno_exprs$pam50),
                                               choices = unique(Pheno_exprs$pam50),
                                               inline = FALSE, width = "150px"),
                            
                            # Filter for rorS risk
                            checkboxGroupInput("breast_custom_DGEA_rorS_filter", "Risk of recurrence",
                                               selected = unique(Pheno_exprs$rorS_risk),
                                               choices = unique(Pheno_exprs$rorS_risk),
                                               inline = FALSE, width = "200px"),
                            
                            # Filter for Mammaprint risk
                            checkboxGroupInput("breast_custom_DGEA_Mammaprint_filter", "Mammaprint risk",
                                               selected = unique(Pheno_exprs$Mammaprint_risk),
                                               choices = unique(Pheno_exprs$Mammaprint_risk),
                                               inline = FALSE, width = "200px"),
                            
                            # Filter for Menopause status
                            checkboxGroupInput("breast_custom_DGEA_Meno_filter", "Menopause",
                                               selected = unique(Pheno_exprs$Menopause.status),
                                               choices = unique(Pheno_exprs$Menopause.status),
                                               inline = FALSE, width = "200px"),
                            
                            # Filter for ER status
                            checkboxGroupInput("breast_custom_DGEA_ER_filter", "ER status",
                                               selected = unique(Pheno_exprs$ER.status),
                                               choices = unique(Pheno_exprs$ER.status),
                                               inline = FALSE, width = "100px"),
                            
                            # Model adjustments
                            checkboxGroupInput("breast_custom_DGEA_adjustments", 
                                               "Variables to adjust for in the model",
                                               selected = c("Response", "pam50"),
                                               choices = sort(c("Response", "pam50",
                                                                "rorS_risk", "Mammaprint_risk", "Location",
                                                                "ER.status", "Menopause.status", 
                                                                Timepoint = "Timepoint_coded")),
                                               inline = TRUE, width = "300px"),
                          
                            # Updatable panel for contrast level 1
                            div(id = "div_1_breast_custom_volcano", 
                                tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                selectInput("breast_cancer_custom_DGEA_contrast_selection",
                                            "Select contrast",
                                            choices = c("Response", "pam50"),
                                            selected = "Response",
                                            width = "250px")),
                            
                            # Updatable panel for contrast level 2
                            div(id = "div_2_breast_custom_volcano", 
                                tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                selectInput("breast_cancer_custom_DGEA_contrast_level1_selection",
                                            "Select level 1",
                                            choices = c("Responder", "Non_responder"),
                                            selected = "Responder",
                                            width = "250px")),
                            
                            # Updatable panel for contrast level 3
                            div(id = "div_3_breast_custom_volcano", 
                                tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                selectInput("breast_cancer_custom_DGEA_contrast_level2_selection",
                                            "Select level 2",
                                            choices = c("Responder", "Non_responder"),
                                            selected = "Non_responder",
                                            width = "250px")),
                            
                            # Select p-value for y-axis (corrected/uncorrected)
                            selectInput("pval_selector_breast_custom", "p-value to plot", 
                                        choices = c("P.Value", "adj.P.Val"),
                                        selected = "adj.P.Val"),
                            
                            # Select x-coordinate for vertical logFC lines
                            numericInput("logFC_breast_custom", 
                                         "DET: Diff. Exp. Threshold - vertical lines", value = 0.25, min = 0,
                                         max = Inf),
                            
                            # Select y-coordinate for p-value threshold
                            numericInput("pval_breast_custom", 
                                         "PVT (adj.P.Val threshold) - horizontal line", value = 0.05, min = 0,
                                         max = Inf),
                            
                            # Select colors for scatter points
                            colourpicker::colourInput("scatter_color_breast_NS_custom", "Color for p>PVT (n.s. results)", 
                                                      value = "#BEBEBE"),
                            colourpicker::colourInput("scatter_color_breast_DE_only_custom", "Color for DE>DET (p>PVT)", 
                                                      value = "#FFC0CB"),
                            colourpicker::colourInput("scatter_color_breast_p_only_custom", "Color for p<PVT (DE<DET)", 
                                                      value = "#551A8B"),
                            colourpicker::colourInput("scatter_color_breast_DE_p_custom", "Color for DE>DET & p<PVT", 
                                                      value = "#8B0000"),
                            
                            # Select opacity of data points
                            sliderInput("alpha_breast_volcano_custom", "Opacity (alpha)",
                                        min = 0, max = 1, value = 0.7),
                            
                            # add "Analyse" trigger button
                            actionButton("breast_custom_DGEA_analyse_button", "Analyse",
                                         style = "color: #FFFFFF; background-color: #1986B2; border-color: #2e6da4"),
                            
                            # Button to reset inputs to default
                            actionButton(inputId = "reset_input_breast_custom_DGEA", 
                                         label = "Reset",
                                         style = "color: #FFFFFF; background-color: #000000; border-color: #2e6da4"),
                            
                            # add "Info" button
                            actionButton("info_custom_DGEA_breast_cancer", "Info")
                            
                )
            ),
            
            bsModal("custom_dgea_info", "Information", "info_custom_DGEA_breast_cancer",
                    fluidRow(
                      htmlOutput("custom_dgea_info_text")
                    )),
            # Volcano plot and top table output in a new page
            bsModal("custom_DGEA_output", "Differential Gene Expression Analysis results",
                    "breast_custom_DGEA_analyse_button",
                    size = "large", fluidRow(
                      column(
                        width = 12,
                        
                        # A box to plot the volcano
                        box(
                          title = "Volcano", id = "breast_cancer_custom_DGEA_volcano",
                          status = "warning", width = 12, height = 600, solidHeader = TRUE, #align = "center",
                          plotlyOutput("breast_cancer_volcano_custom", width = 500, height = 500)
                        )
                      ),
                      
                      column(
                        width = 12,
                        
                        # Top table output
                        box(
                          title = "Top Table", width = 12, solidHeader = TRUE,
                          status = "warning", id = "breast_cancer_custom_toptable_output",
                          DT::dataTableOutput("breast_cancer_custom_toptable")
                        ),
                        downloadButton("download_data_custom_DGEA", "Download")
                      )
                    ))
          )
          ),
  # PDAC #####
  tabItem(tabName = "PDAC_project",
          h2("Stage-specific diagnostic biomarkers in PDAC")),
  
  # OSCC #####
  tabItem(tabName = "OSCC_project",
          h2("Recurrence in OSCC"))
  )
)

# Sidebar #####
# The dashboard sidebar. Main levels: BC, PDAC, OSCC
# subItems are introduced here - NOT IN THE BODY
sidebar <- dashboardSidebar(
  sidebarMenu(style = "white-space: normal;", # wraps text to fit in sidebar
    menuItem(tabName = "breast_cancer_project",
             text = "Breast Cancer",
             startExpanded = TRUE, # use this to enable subItems
             
             # Exploratory analysis
             menuSubItem(tabName = "breast_cancer_exploratory",
                         text = "Exploratory analysis"),
             
             # Sunburst
             menuSubItem(tabName = "breast_cancer_sunburst",
                         text = "Sunburst plots"),
             
             # Volcano
             menuSubItem(tabName = "breast_cancer_subItem_volcano",
                         text = "Volcano"),
             
             # Custom DGEA
             menuSubItem(tabName = "breast_cancer_subItem_DGEA",
                         text = "Custom DGEA"),
             
             # Machine Learning
             menuSubItem(tabName = "breast_cancer_subItem_ml",
                         text = "Machine Learning")
    ), 
    menuItem(tabName = "PDAC_project", 
             text = "Pancreatic Ductal Adenocarcinoma (PDAC)"
    ),
    menuItem(tabName = "OSCC_project", 
             text = "Oral Squamous Cell Carcinoma"
    )
  )
)

# Server #####
server <- function(input, output, session) {
  ### IMPORTANT ###
  # Wrap everything that's adjusted based on input values in a reactive() expression
  # Then use action Buttons and isolate in the output section, so that
  # the server is not constantly adjusting output for every small input change
  
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
            x = breast_cancer_full_pheno[subset_breast_hist,input$histvar_select_breast],
            type = "histogram", 
            histnorm = names(hist_types_breast[which(hist_types_breast %in% input$hist_type_breast)]),
            marker = list(color = input$hist_fill_breast,
                          line = list(color = input$hist_color_breast, width = 0.5)),
            autobinx = FALSE, nbinsx = ~input$hist_breast_bins*2, height = 300) %>%
      layout(title = list(text = paste0("<b>Histogram of ", input$histvar_select_breast),
                          x = 0, y = 0.99, font = list(size  = 10)),
             annotations = list(x = -0.098, y = 1.065, xref = "paper", yref = "paper",
                                showarrow = F,
                                text = paste0("Studies: ",
                                              c(list(input$hist_dataset_checkbox))),
                                font = list(size = 8)),
             xaxis = list(title = list(text = paste0("<b>", input$histvar_select_breast)),
                          linecolor = '#000000',
                          linewidth = 2),
             yaxis = list(title = list(text = y_axis_titles_breast[input$hist_type_breast][[1]]),
                          linecolor = '#000000',
                          linewidth = 2))
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
  
  
  # Breast Cancer: bar chart #####
  
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
                              x = 0, y = 0.99, font = list(size  = 10)),
                 annotations = list(x = -0.098, y = 1.065, xref = "paper", yref = "paper",
                                    showarrow = F,
                                    text = paste0("Studies: ",
                                                  c(list(input$hist_dataset_checkbox))),
                                    font = list(size = 8)),
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
                          x = 0, y = 0.99, font = list(size  = 10)),
             annotations = list(x = -0.098, y = 1.065, xref = "paper", yref = "paper",
                                showarrow = F,
                                text = paste0("Studies: ",
                                              c(list(input$hist_dataset_checkbox))),
                                font = list(size = 8)),
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
  
  # Breast cancer: sunbursts #####
  
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
                                   "XVC1", "XVC2", "XVE", "SJS",
                                   "Non_responder", "Responder", 
                                   "Basal", "Her2", "LumA", "LumB", "Normal",
                                   "T1", "T2",
                                   "High", "Intermediate", "Low",
                                   "Risk", "No risk",
                                   "Others", "Claudin",
                                   "HER2+", "ER-/HER2-", "ER+/HER2- High Prolif", "ER+/HER2- Low Prolif",
                                   "UK", "USA", "Korea",
                                   "Mixed", "PM", # Mixed is used for both ER and Meno status here
                                   "ER+",
                                   "Cluster 1", "Cluster 2"))
  
  # Plot sunburst function (analytical set)
  plot_analytical_sunburst <- reactive({
    
    # Define the subset of studies selected by the user
    subset_breast_analytical_sunburst <- breast_cancer_full_pheno$Dataset %in%
      input$sunburst_analytical_dataset_checkbox
    
    # If the user selected 4 variables
    if(input$sunburst_var1_select_breast != "None" &&
       input$sunburst_var2_select_breast != "None" &&
       input$sunburst_var3_select_breast != "None" &&
       input$sunburst_var4_select_breast != "None"){
      
      Pheno_sunburst = breast_cancer_full_pheno[subset_breast_analytical_sunburst,] %>% 
        dplyr::select(!!sym(input$sunburst_var1_select_breast),
                      !!sym(input$sunburst_var2_select_breast),
                      !!sym(input$sunburst_var3_select_breast),
                      !!sym(input$sunburst_var4_select_breast)) %>%
        group_by(!!sym(input$sunburst_var1_select_breast),
                 !!sym(input$sunburst_var2_select_breast),
                 !!sym(input$sunburst_var3_select_breast),
                 !!sym(input$sunburst_var4_select_breast)) %>%
        summarise(Counts = n()) %>%
        as.data.frame()
      
      
    } else if(input$sunburst_var1_select_breast != "None" &&
              input$sunburst_var2_select_breast != "None" &&
              input$sunburst_var3_select_breast != "None" &&
              input$sunburst_var4_select_breast == "None") {
      
      # If the user selected 3 variables 
      Pheno_sunburst = breast_cancer_full_pheno[subset_breast_analytical_sunburst,] %>%
        dplyr::select(!!sym(input$sunburst_var1_select_breast),
                      !!sym(input$sunburst_var2_select_breast),
                      !!sym(input$sunburst_var3_select_breast)) %>%
        group_by(!!sym(input$sunburst_var1_select_breast),
                 !!sym(input$sunburst_var2_select_breast),
                 !!sym(input$sunburst_var3_select_breast)) %>%
        summarise(Counts = n()) %>%
        as.data.frame()
      
      
    } else if(input$sunburst_var1_select_breast != "None" &&
              input$sunburst_var2_select_breast != "None" &&
              input$sunburst_var3_select_breast == "None" &&
              input$sunburst_var4_select_breast == "None") {
      
      # If the user selected 2 variables
      Pheno_sunburst = breast_cancer_full_pheno[subset_breast_analytical_sunburst,] %>%
        dplyr::select(!!sym(input$sunburst_var1_select_breast),
                      !!sym(input$sunburst_var2_select_breast)) %>%
        group_by(!!sym(input$sunburst_var1_select_breast),
                 !!sym(input$sunburst_var2_select_breast)) %>%
        summarise(Counts = n()) %>%
        as.data.frame()
      
    } else if(input$sunburst_var1_select_breast != "None" &&
              input$sunburst_var2_select_breast == "None" &&
              input$sunburst_var3_select_breast == "None" &&
              input$sunburst_var4_select_breast == "None") {
      
      # If the user selected 1 variable only
      Pheno_sunburst = breast_cancer_full_pheno[subset_breast_analytical_sunburst,] %>%
        dplyr::select(!!sym(input$sunburst_var1_select_breast)) %>%
        group_by(!!sym(input$sunburst_var1_select_breast)) %>%
        summarise(Counts = n()) %>%
        as.data.frame()
    }
    
    # Convert df to sunburst format
    Pheno_sunburst_new = as.sunburstDF(Pheno_sunburst, value_column = "Counts",
                                     add_root = FALSE) %>% 
      inner_join(coloring, by = "labels")
    
    # Plotly
    pie = plot_ly(height  = 600) %>%
      add_trace(ids = Pheno_sunburst_new$ids, labels = Pheno_sunburst_new$labels, 
                parents = Pheno_sunburst_new$parents, values = Pheno_sunburst_new$values,
                type = 'sunburst', branchvalues = 'total',
                insidetextorientation = 'radial', maxdepth = ncol(Pheno_sunburst),
                marker = list(colors = Pheno_sunburst_new$colors),
                opacity = input$alpha_breast_sunburst_analytical) %>%
      layout(
        grid = list(columns = 1, rows = 1),
        margin = list(l = 0, r = 0, b = 0, t = 0)
      )
    pie
    
  })
  
  # Plot sunburst function (consensus set)
  plot_consensus_sunburst <- reactive({
    
    # Define the subset of studies selected by the user
    subset_breast_consensus_sunburst <- breast_cancer_consensus_set$Dataset %in%
      input$sunburst_consensus_dataset_checkbox
    
    # If the user selected 4 variables
    if(input$sunburst_var1_select_breast_consensus != "None" &&
       input$sunburst_var2_select_breast_consensus != "None" &&
       input$sunburst_var3_select_breast_consensus != "None" &&
       input$sunburst_var4_select_breast_consensus != "None"){
      
      Pheno_sunburst = breast_cancer_consensus_set[subset_breast_consensus_sunburst,] %>% 
        dplyr::select(!!sym(input$sunburst_var1_select_breast_consensus),
                      !!sym(input$sunburst_var2_select_breast_consensus),
                      !!sym(input$sunburst_var3_select_breast_consensus),
                      !!sym(input$sunburst_var4_select_breast_consensus)) %>%
        group_by(!!sym(input$sunburst_var1_select_breast_consensus),
                 !!sym(input$sunburst_var2_select_breast_consensus),
                 !!sym(input$sunburst_var3_select_breast_consensus),
                 !!sym(input$sunburst_var4_select_breast_consensus)) %>%
        summarise(Counts = n()) %>%
        as.data.frame()
      
      
    } else if(input$sunburst_var1_select_breast_consensus != "None" &&
              input$sunburst_var2_select_breast_consensus != "None" &&
              input$sunburst_var3_select_breast_consensus != "None" &&
              input$sunburst_var4_select_breast_consensus == "None") {
      
      # If the user selected 3 variables 
      Pheno_sunburst = breast_cancer_consensus_set[subset_breast_consensus_sunburst,] %>%
        dplyr::select(!!sym(input$sunburst_var1_select_breast_consensus),
                      !!sym(input$sunburst_var2_select_breast_consensus),
                      !!sym(input$sunburst_var3_select_breast_consensus)) %>%
        group_by(!!sym(input$sunburst_var1_select_breast_consensus),
                 !!sym(input$sunburst_var2_select_breast_consensus),
                 !!sym(input$sunburst_var3_select_breast_consensus)) %>%
        summarise(Counts = n()) %>%
        as.data.frame()
      
      
    } else if(input$sunburst_var1_select_breast_consensus != "None" &&
              input$sunburst_var2_select_breast_consensus != "None" &&
              input$sunburst_var3_select_breast_consensus == "None" &&
              input$sunburst_var4_select_breast_consensus == "None") {
      
      # If the user selected 2 variables
      Pheno_sunburst = breast_cancer_consensus_set[subset_breast_consensus_sunburst,] %>%
        dplyr::select(!!sym(input$sunburst_var1_select_breast_consensus),
                      !!sym(input$sunburst_var2_select_breast_consensus)) %>%
        group_by(!!sym(input$sunburst_var1_select_breast_consensus),
                 !!sym(input$sunburst_var2_select_breast_consensus)) %>%
        summarise(Counts = n()) %>%
        as.data.frame()
      
    } else if(input$sunburst_var1_select_breast_consensus != "None" &&
              input$sunburst_var2_select_breast_consensus == "None" &&
              input$sunburst_var3_select_breast_consensus == "None" &&
              input$sunburst_var4_select_breast_consensus == "None") {
      
      # If the user selected 1 variable only
      Pheno_sunburst = breast_cancer_consensus_set[subset_breast_consensus_sunburst,] %>%
        dplyr::select(!!sym(input$sunburst_var1_select_breast_consensus)) %>%
        group_by(!!sym(input$sunburst_var1_select_breast_consensus)) %>%
        summarise(Counts = n()) %>%
        as.data.frame()
    }
    
    # Convert df to sunburst format
    Pheno_sunburst_new = as.sunburstDF(Pheno_sunburst, value_column = "Counts",
                                     add_root = FALSE) %>% 
      inner_join(coloring, by = "labels")
    
    # Plotly
    pie = plot_ly(height = 600) %>%
      add_trace(ids = Pheno_sunburst_new$ids, labels = Pheno_sunburst_new$labels, 
                parents = Pheno_sunburst_new$parents, values = Pheno_sunburst_new$values,
                type = 'sunburst', branchvalues = 'total',
                insidetextorientation = 'radial', maxdepth = ncol(Pheno_sunburst),
                marker = list(colors = Pheno_sunburst_new$colors),
                opacity = input$alpha_breast_sunburst_analytical) %>%
      layout(
        grid = list(columns = 1, rows = 1),
        margin = list(l = 0, r = 0, b = 0, t = 0)
      )
    pie
    
  })
  
  # Plot analytical set sunburst
  output$analytical_sunburst_breast <- renderPlotly({
    input$draw_breast_sunburst_analytical
    isolate({plot_analytical_sunburst()})
    })
  
  # Select all button
  observe({
    if(input$select_all_sunburst_analytical_breast > 0){
      if (input$select_all_sunburst_analytical_breast %% 2 == 0) {
        updateCheckboxGroupInput(session, "sunburst_analytical_dataset_checkbox",
                                 choices = unique(breast_cancer_full_pheno$Dataset),
                                 selected = unique(breast_cancer_full_pheno$Dataset),
                                 inline = TRUE)
      }
      else
      {
        updateCheckboxGroupInput(session, "sunburst_analytical_dataset_checkbox",
                                 choices = unique(breast_cancer_full_pheno$Dataset),
                                 selected = c(),
                                 inline = TRUE)
      }
    }
  })
  
  # Reset parameters after pressing the corresponding button
  observeEvent(input$reset_input_breast_sunburst_analytical, {
    shinyjs::reset("sunburst_var1_select_breast")
    shinyjs::reset("sunburst_var2_select_breast")
    shinyjs::reset("sunburst_var3_select_breast")
    shinyjs::reset("sunburst_var4_select_breast")
  })
  
  # Pop-up info message, triggered when the user presses the Info button
  output$analytical_sunburst_breast_info_text <- renderText({
    paste0("<br> &#8226 Select parameters for the sunburst plot.",
           "<br> &#8226 Default parameters are the ones used in the project.",
           "<br> &#8226 You can pick up to <b>four</b> variables for the sunburst.",
           "<br> &#8226 Colors are pre-selected due to the very large number of combinations and levels.")})
  
  # Plot consensus set sunburst
  output$consensus_sunburst_breast <- renderPlotly({
    input$draw_breast_sunburst_consensus
    isolate({plot_consensus_sunburst()})
  })
  
  # Select all button
  observe({
    if(input$select_all_sunburst_consensus_breast > 0){
      if (input$select_all_sunburst_consensus_breast %% 2 == 0) {
        updateCheckboxGroupInput(session, "sunburst_consensus_dataset_checkbox",
                                 choices = unique(breast_cancer_full_pheno$Dataset),
                                 selected = unique(breast_cancer_full_pheno$Dataset),
                                 inline = TRUE)
      }
      else
      {
        updateCheckboxGroupInput(session, "sunburst_consensus_dataset_checkbox",
                                 choices = unique(breast_cancer_full_pheno$Dataset),
                                 selected = c(),
                                 inline = TRUE)
      }
    }
  })
  
  # Reset parameters after pressing the corresponding button
  observeEvent(input$reset_input_breast_sunburst_consensus, {
    shinyjs::reset("sunburst_var1_select_breast_consensus")
    shinyjs::reset("sunburst_var2_select_breast_consensus")
    shinyjs::reset("sunburst_var3_select_breast_consensus")
    shinyjs::reset("sunburst_var4_select_breast_consensus")
  })
  
  # Pop-up info message, triggered when the user presses the Info button
  output$consensus_sunburst_breast_info_text <- renderText({
    paste0("<br> &#8226 Select parameters for the sunburst plot.",
           "<br> &#8226 Default parameters are the ones used in the project.",
           "<br> &#8226 You can pick up to <b>four</b> variables for the sunburst.",
           "<br> &#8226 Colors are pre-selected due to the very large number of combinations and levels.")})
  
  # Breast Cancer: volcano plot #####
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
    breast_cancer_DGEA$volc_p_status[breast_cancer_DGEA$adj.P.Val>input$pval_breast &
                                       abs(breast_cancer_DGEA$logFC)<input$logFC_breast] = "Not Significant"
    breast_cancer_DGEA$volc_p_status[breast_cancer_DGEA$adj.P.Val>input$pval_breast &
                                       abs(breast_cancer_DGEA$logFC)>input$logFC_breast] = 
      paste0("|DE|>", input$logFC_breast," s.d. & p>", input$pval_breast)
    breast_cancer_DGEA$volc_p_status[breast_cancer_DGEA$adj.P.Val<input$pval_breast &
                                       abs(breast_cancer_DGEA$logFC)<input$logFC_breast] = 
      paste0("|DE|<", input$logFC_breast," s.d. & p<", input$pval_breast)
    breast_cancer_DGEA$volc_p_status[breast_cancer_DGEA$adj.P.Val<input$pval_breast &
                                       abs(breast_cancer_DGEA$logFC)>input$logFC_breast] = 
      paste0("|DE|>", input$logFC_breast," s.d. & p<", input$pval_breast)
    breast_cancer_DGEA$volc_p_status = factor(breast_cancer_DGEA$volc_p_status,
                                              levels = c("Not Significant", 
                                                         paste0("|DE|>", input$logFC_breast," s.d. & p>", input$pval_breast),
                                                         paste0("|DE|<", input$logFC_breast," s.d. & p<", input$pval_breast), 
                                                         paste0("|DE|>", input$logFC_breast," s.d. & p<", input$pval_breast)),
                                              labels = c("Not Significant", 
                                                         paste0("|DE|>", input$logFC_breast," s.d. & p>", input$pval_breast),
                                                         paste0("|DE|<", input$logFC_breast," s.d. & p<", input$pval_breast), 
                                                         paste0("|DE|>", input$logFC_breast," s.d. & p<", input$pval_breast)))
    
    # Create the palette for the scatter colors
    pal = c(input$scatter_color_breast_NS, 
            input$scatter_color_breast_DE_only, 
            input$scatter_color_breast_p_only, 
            input$scatter_color_breast_DE_p)
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
  

  # Plot the breast cancer volcano after the user pressed the "Draw!" button
  output$breast_volcano <- renderPlotly({
    input$draw_breast_volcano 
    isolate({prepare_volcano()})
    })
  
  # Reset parameters after pressing the corresponding button
  observeEvent(input$reset_input_breast_volcano, {
    shinyjs::reset("Volcano_tuning_breast") 
  })
  
  # Pop-up info message, triggered when the user presses the Info button
  output$volcano_breast_info_text <- renderText({
    paste0("<br> &#8226 Select parameters for the volcano plot.",
           "<br> &#8226 Default parameters are the ones used in the project.",
           "<br> &#8226 You can overlay gene symbols by clicking on individual data points.",
           "<br> &#8226 The x-axis shows the numerical difference between the mean expression in group 1 (responders) and group 2 (non-responders) measured in standard deviations from the gene's overall mean expression.")})
  
  # Breast Cancer: Machine Learning #####
  # A function for dynamic conditional selectInput for the ML model subcategory
  
  # Box 1
  observeEvent(input$breast_cancer_ml_model_category, {
    if(input$breast_cancer_ml_model_category == "Logistic Regression"){
      updateRadioButtons(inputId = "breast_cancer_ml_model_subcategory",
                         choices = c("Backward", "Lasso-regularised"),
                         selected = "Lasso-regularised")
    } else if(input$breast_cancer_ml_model_category == "Decision Trees"){
      updateRadioButtons(inputId = "breast_cancer_ml_model_subcategory",
                         choices = sort(c(`C5.0 (tuned with Cohen's k)` = "C5.0 - k",
                                          `Bagging (100 iterations)` = "100X Bagging",
                                          `AdaBoost` = "Boosting",
                                          `Random Forest (tuned with Cohen's k)` = "RForest - k",
                                          `Random Forest (tuned with ROC)` = "RForest - ROC",
                                          `C5.0 (tuned with ROC)` = "C5.0 - ROC")),
                         selected = "C5.0 - ROC")
    } else if(input$breast_cancer_ml_model_category == "Support Vector Machines"){
      updateRadioButtons(inputId = "breast_cancer_ml_model_subcategory",
                         choices = sort(c(`Linear kernel` = "Linear",
                                          `L2-regularised linear kernel` = "L2 Linear",
                                          `Radial Basis Function (RBF) kernel` = "RBF")),
                         selected = "RBF")
    }
  })
  
  # Box 2
  observeEvent(input$breast_cancer_ml_model_category_2, {
    if(input$breast_cancer_ml_model_category_2 == "Logistic Regression"){
      updateRadioButtons(inputId = "breast_cancer_ml_model_subcategory_2",
                         choices = c("Backward", "Lasso-regularised"),
                         selected = "Lasso-regularised")
    } else if(input$breast_cancer_ml_model_category_2 == "Decision Trees"){
      updateRadioButtons(inputId = "breast_cancer_ml_model_subcategory_2",
                         choices = sort(c(`C5.0 (tuned with Cohen's k)` = "C5.0 - k",
                                          `Bagging (100 iterations)` = "100X Bagging",
                                          `AdaBoost` = "Boosting",
                                          `Random Forest (tuned with Cohen's k)` = "RForest - k",
                                          `Random Forest (tuned with ROC)` = "RForest - ROC",
                                          `C5.0 (tuned with ROC)` = "C5.0 - ROC")),
                         selected = "C5.0 - ROC")
    } else if(input$breast_cancer_ml_model_category_2 == "Support Vector Machines"){
      updateRadioButtons(inputId = "breast_cancer_ml_model_subcategory_2",
                         choices = sort(c(`Linear kernel` = "Linear",
                                          `L2-regularised linear kernel` = "L2 Linear",
                                          `Radial Basis Function (RBF) kernel` = "RBF")),
                         selected = "RBF")
    }
  })
  
  # Box 3
  observeEvent(input$breast_cancer_ml_model_category_3, {
    if(input$breast_cancer_ml_model_category_3 == "Logistic Regression"){
      updateRadioButtons(inputId = "breast_cancer_ml_model_subcategory_3",
                         choices = c("Backward", "Lasso-regularised"),
                         selected = "Lasso-regularised")
    } else if(input$breast_cancer_ml_model_category_3 == "Decision Trees"){
      updateRadioButtons(inputId = "breast_cancer_ml_model_subcategory_3",
                         choices = sort(c(`C5.0 (tuned with Cohen's k)` = "C5.0 - k",
                                          `Bagging (100 iterations)` = "100X Bagging",
                                          `AdaBoost` = "Boosting",
                                          `Random Forest (tuned with Cohen's k)` = "RForest - k",
                                          `Random Forest (tuned with ROC)` = "RForest - ROC",
                                          `C5.0 (tuned with ROC)` = "C5.0 - ROC")),
                         selected = "C5.0 - ROC")
    } else if(input$breast_cancer_ml_model_category_3 == "Support Vector Machines"){
      updateRadioButtons(inputId = "breast_cancer_ml_model_subcategory_3",
                         choices = sort(c(`Linear kernel` = "Linear",
                                          `L2-regularised linear kernel` = "L2 Linear",
                                          `Radial Basis Function (RBF) kernel` = "RBF")),
                         selected = "RBF")
    }
  })
  
  # Plot counter
  counter = reactiveValues(counter = 1)
  
  # Hide second model selection box
  observeEvent(input$multiple_rocs_breast, {
    if(input$multiple_rocs_breast %% 2 == 1){
      shinyjs::show("model_select_breast_2")
    } else {
      shinyjs::hide("model_select_breast_2")
    }
  })
  
  # If the user clicks apply then set the counter to 2
  observeEvent(input$apply_comparison_breast_2, {
    counter$counter = 2
  })
  
  # Hide third model and variable selection box
  observeEvent(input$multiple_rocs_breast_2, {
    if(input$multiple_rocs_breast_2 %% 2 == 1){
      shinyjs::show("model_select_breast_3")
    } else {
      shinyjs::hide("model_select_breast_3")
    }
  })
  
  # If the user clicks apply then set the counter to 2
  observeEvent(input$apply_comparison_breast_3, {
    counter$counter = 3
  })
  
  observeEvent(input$breast_cancer_ROC_plot, {
    counter$counter = 1
  })
  
  err_metrics = function(CM)
  {
    TN =CM[1,1]
    TP =CM[2,2]
    FP =CM[1,2]
    FN =CM[2,1]
    precision =(TP)/(TP+FP)
    recall_score =(FP)/(FP+TN)
    f1_score=2*((precision*recall_score)/(precision+recall_score))
    accuracy_model  =(TP+TN)/(TP+TN+FP+FN)
    False_positive_rate =(FP)/(FP+TN)
    False_negative_rate =(FN)/(FN+TP)
    err_df = as.data.frame(cbind(c("Accuracy", "Precision", "Recall", "FPR", "FNR", "F1"),
                                 c(accuracy_model, precision, recall_score, False_positive_rate,
                                   False_negative_rate, f1_score)))
    return(err_df)
  }
  
  # ROC curve function
  plot_breast_cancer_ROC <- reactive({
    
    if(counter$counter == 1){
      # Plot legend
      Legends = input$breast_cancer_ml_legend_entry_1
      auc_values = list()
      
      # Variables for inputs
      category = input$breast_cancer_ml_model_category
      subcategory = input$breast_cancer_ml_model_subcategory
      ml_study_subset = full_ml_set$Dataset %in% input$ml_breast_dataset_checkbox
      
      # Subset the data that the model will be assessed on
      dataset = full_ml_set[ml_study_subset,] %>%
        dplyr::filter(Treatment %in% input$breast_cancer_ml_treatment_filter) %>%
        dplyr::filter(Timepoint %in% input$breast_cancer_ml_timepoint_filter) %>%
        dplyr::filter(Location %in% input$breast_cancer_ml_location_filter) %>%
        dplyr::filter(pam50 %in% input$breast_cancer_ml_pam50_filter) %>%
        dplyr::filter(rorS_risk %in% input$breast_cancer_ml_rorS_filter) %>%
        dplyr::filter(Mammaprint_risk %in% input$breast_cancer_ml_Mammaprint_filter) %>%
        dplyr::filter(Menopause.status %in% input$breast_cancer_ml_Meno_filter) %>%
        dplyr::filter(ER.status %in% input$breast_cancer_ml_ER_filter)
      
      # Number of samples in the filtered dataset
      nsamples = nrow(dataset)
      
      model = ML[[category]][[subcategory]]
      
      # Predict and create a confusion matrix
      
      # Make predictions according to model chosen
      if(subcategory == "100X Bagging"){
        predictions_frame = adabag::predict.bagging(model, dataset)
        predictions = predictions_frame[["class"]]
        confusion_matrix = table(predictions, dataset$Response)
        model_accuracy = 1 - (confusion_matrix[1] + confusion_matrix[4])/
          sum(confusion_matrix)
        predicted_probabilities = predictions_frame[["prob"]]
        errs_1 = err_metrics(confusion_matrix)
        colnames(errs_1) = c("Metrics", Legends[[1]])
      } else if(subcategory == "Boosting") {
        predictions_frame = predict(model, dataset)
        predictions = predictions_frame[["class"]]
        confusion_matrix = predictions_frame[["confusion"]]
        model_accuracy = 1 - (confusion_matrix[1] + confusion_matrix[4])/
          sum(confusion_matrix)
        predicted_probabilities = predictions_frame[["prob"]]
        errs_1 = err_metrics(confusion_matrix)
        colnames(errs_1) = c("Metrics", Legends[[1]])
      } else {
        predictions = predict(model, dataset)
        confusion_matrix = table(predictions, dataset$Response)
        model_accuracy = (confusion_matrix[1] + confusion_matrix[4])/
          sum(confusion_matrix)
        predicted_probabilities = predict(model, dataset, type = "prob")
        errs_1 = err_metrics(confusion_matrix)
        colnames(errs_1) = c("Metrics", Legends[[1]])
      }
      
      # ROC plot generation
      if(category == "Support Vector Machines"){
        print("ROC plots not available for Support Vector Machines")
      } else {
        join = cbind(predictions, predicted_probabilities, dataset)
        colnames(join)[c(2,3)] = c("Responder", "Non_responder")
        join = join[order(join$Responder),]
        model_roc = roc(predictor = join$Responder, 
                        response = as.character(join$Response))
        auc_values[[1]] = round(auc(model_roc), 3)
        p = plot(model_roc, 
             main = "ROC curve",
             col = randomColor(), lwd = 3, legacy.axes = TRUE, xlim = c(1,0), ylim = c(0,1), 
             asp = 0.92, cex = 4, xaxs = "i", yaxs = "i", width = 900, height = 900)
      }
      
      return(list(plot = p, conf = confusion_matrix, 
                  acc = model_accuracy,
                  Legends = Legends, auc_values = auc_values, nsamples = nsamples,
                  error_metrics = errs_1))
      
      
      } else if(counter$counter == 2) {
        # Plot legend
        Legends = input$breast_cancer_ml_legend_entry_1
        auc_values = list()
        
        # Variables for inputs
        category = input$breast_cancer_ml_model_category
        subcategory = input$breast_cancer_ml_model_subcategory
        ml_study_subset = full_ml_set$Dataset %in% input$ml_breast_dataset_checkbox
        
        # Subset the data that the model will be assessed on
        dataset = full_ml_set[ml_study_subset,] %>%
          dplyr::filter(Treatment %in% input$breast_cancer_ml_treatment_filter) %>%
          dplyr::filter(Timepoint %in% input$breast_cancer_ml_timepoint_filter) %>%
          dplyr::filter(Location %in% input$breast_cancer_ml_location_filter) %>%
          dplyr::filter(pam50 %in% input$breast_cancer_ml_pam50_filter) %>%
          dplyr::filter(rorS_risk %in% input$breast_cancer_ml_rorS_filter) %>%
          dplyr::filter(Mammaprint_risk %in% input$breast_cancer_ml_Mammaprint_filter) %>%
          dplyr::filter(Menopause.status %in% input$breast_cancer_ml_Meno_filter) %>%
          dplyr::filter(ER.status %in% input$breast_cancer_ml_ER_filter)
        
        # Number of samples in the filtered dataset
        nsamples = nrow(dataset)
        
        model = ML[[category]][[subcategory]]
        
        # Predict and create a confusion matrix
        
        # Make predictions according to model chosen
        if(subcategory == "100X Bagging"){
          predictions_frame = adabag::predict.bagging(model, dataset)
          predictions = predictions_frame[["class"]]
          confusion_matrix = table(predictions, dataset$Response)
          model_accuracy = 1 - (confusion_matrix[1] + confusion_matrix[4])/
            sum(confusion_matrix)
          predicted_probabilities = predictions_frame[["prob"]]
          errs_1 = err_metrics(confusion_matrix)
          colnames(errs_1) = c("Metrics", Legends[[1]])
        } else if(subcategory == "Boosting") {
          predictions_frame = predict(model, dataset)
          predictions = predictions_frame[["class"]]
          confusion_matrix = predictions_frame[["confusion"]]
          model_accuracy = 1 - (confusion_matrix[1] + confusion_matrix[4])/
            sum(confusion_matrix)
          predicted_probabilities = predictions_frame[["prob"]]
          errs_1 = err_metrics(confusion_matrix)
          colnames(errs_1) = c("Metrics", Legends[[1]])
        } else {
          predictions = predict(model, dataset)
          confusion_matrix = table(predictions, dataset$Response)
          model_accuracy = (confusion_matrix[1] + confusion_matrix[4])/
            sum(confusion_matrix)
          predicted_probabilities = predict(model, dataset, type = "prob")
          errs_1 = err_metrics(confusion_matrix)
          colnames(errs_1) = c("Metrics", Legends[[1]])
        }
        
        # ROC plot generation
        if(category == "Support Vector Machines"){
          print("ROC plots not available for Support Vector Machines")
        } else {
          join = cbind(predictions, predicted_probabilities, dataset)
          colnames(join)[c(2,3)] = c("Responder", "Non_responder")
          join = join[order(join$Responder),]
          model_roc = roc(predictor = join$Responder, 
                          response = as.character(join$Response))
          auc_values[[1]] = round(auc(model_roc), 3)
          p = plot(model_roc, 
               main = "ROC curve",
               col = randomColor(), lwd = 3, legacy.axes = TRUE, xlim = c(1,0), ylim = c(0,1), 
               asp = 0.92, cex = 4, xaxs = "i", yaxs = "i", width = 900, height = 900)
        }
        
        # Plot legend
        Legends = c(Legends, input$breast_cancer_ml_legend_entry_2)
        
        # Variables for inputs
        category_2 = input$breast_cancer_ml_model_category_2
        subcategory_2 = input$breast_cancer_ml_model_subcategory_2
        ml_study_subset_2 = full_ml_set$Dataset %in% input$ml_breast_dataset_checkbox_2
        
        # Subset the data that the model will be assessed on
        dataset_2 = full_ml_set[ml_study_subset_2,] %>%
          dplyr::filter(Treatment %in% input$breast_cancer_ml_treatment_filter_2) %>%
          dplyr::filter(Timepoint %in% input$breast_cancer_ml_timepoint_filter_2) %>%
          dplyr::filter(Location %in% input$breast_cancer_ml_location_filter_2) %>%
          dplyr::filter(pam50 %in% input$breast_cancer_ml_pam50_filter_2) %>%
          dplyr::filter(rorS_risk %in% input$breast_cancer_ml_rorS_filter_2) %>%
          dplyr::filter(Mammaprint_risk %in% input$breast_cancer_ml_Mammaprint_filter_2) %>%
          dplyr::filter(Menopause.status %in% input$breast_cancer_ml_Meno_filter_2) %>%
          dplyr::filter(ER.status %in% input$breast_cancer_ml_ER_filter_2)
        
        # Number of samples in the filtered dataset
        nsamples = c(nsamples, nrow(dataset_2))
        
        model_2 = ML[[category_2]][[subcategory_2]]
        
        # Predict and create a confusion matrix
        
        # Make predictions according to model chosen
        if(subcategory_2 == "100X Bagging"){
          predictions_frame_2 = adabag::predict.bagging(model_2, dataset_2)
          predictions_2 = predictions_frame_2[["class"]]
          confusion_matrix_2 = table(predictions_2, dataset_2$Response)
          model_accuracy_2 = 1 - (confusion_matrix_2[1] + confusion_matrix_2[4])/
            sum(confusion_matrix_2)
          predicted_probabilities_2 = predictions_frame_2[["prob"]]
          errs_2 = err_metrics(confusion_matrix_2)
          colnames(errs_2) = c("Metrics", Legends[[2]])
        } else if(subcategory_2 == "Boosting") {
          predictions_frame_2 = predict(model_2, dataset_2)
          predictions_2 = predictions_frame_2[["class"]]
          confusion_matrix_2 = predictions_frame_2[["confusion"]]
          model_accuracy_2 = 1 - (confusion_matrix_2[1] + confusion_matrix_2[4])/
            sum(confusion_matrix_2)
          predicted_probabilities_2 = predictions_frame_2[["prob"]]
          errs_2 = err_metrics(confusion_matrix_2)
          colnames(errs_2) = c("Metrics", Legends[[2]])
        } else {
          predictions_2 = predict(model_2, dataset_2)
          confusion_matrix_2 = table(predictions_2, dataset_2$Response)
          model_accuracy_2 = (confusion_matrix_2[1] + confusion_matrix_2[4])/
            sum(confusion_matrix_2)
          predicted_probabilities_2 = predict(model_2, dataset_2, type = "prob")
          errs_2 = err_metrics(confusion_matrix_2)
          colnames(errs_2) = c("Metrics", Legends[[2]])
        }
        
        # ROC plot generation
        if(category_2 == "Support Vector Machines"){
          print("ROC plots not available for Support Vector Machines")
        } else {
          join_2 = cbind(predictions_2, predicted_probabilities_2, dataset_2)
          colnames(join_2)[c(2,3)] = c("Responder", "Non_responder")
          join_2 = join_2[order(join_2$Responder),]
          model_roc_2 = roc(predictor = join_2$Responder, 
                            response = as.character(join_2$Response))
          auc_values[[2]] = round(auc(model_roc_2), 3)
          p2 = plot(model_roc_2, col = randomColor(), lwd = 3, 
               add = TRUE, cex = 4)
        }
        
        return(list(plot = c(p, p2), conf = c(confusion_matrix,
                                                  confusion_matrix_2), 
                    acc = c(model_accuracy, model_accuracy_2),
                    Legends = Legends, auc_values = auc_values, nsamples = nsamples,
                    error_metrics = inner_join(errs_1, errs_2, by = "Metrics")))
        
        
        } else if(counter$counter == 3) {
          
          # Plot legend
          Legends = input$breast_cancer_ml_legend_entry_1
          auc_values = list()
          
          # Variables for inputs
          category = input$breast_cancer_ml_model_category
          subcategory = input$breast_cancer_ml_model_subcategory
          ml_study_subset = full_ml_set$Dataset %in% input$ml_breast_dataset_checkbox
          
          # Subset the data that the model will be assessed on
          dataset = full_ml_set[ml_study_subset,] %>%
            dplyr::filter(Treatment %in% input$breast_cancer_ml_treatment_filter) %>%
            dplyr::filter(Timepoint %in% input$breast_cancer_ml_timepoint_filter) %>%
            dplyr::filter(Location %in% input$breast_cancer_ml_location_filter) %>%
            dplyr::filter(pam50 %in% input$breast_cancer_ml_pam50_filter) %>%
            dplyr::filter(rorS_risk %in% input$breast_cancer_ml_rorS_filter) %>%
            dplyr::filter(Mammaprint_risk %in% input$breast_cancer_ml_Mammaprint_filter) %>%
            dplyr::filter(Menopause.status %in% input$breast_cancer_ml_Meno_filter) %>%
            dplyr::filter(ER.status %in% input$breast_cancer_ml_ER_filter)
          
          # Number of samples in the filtered dataset
          nsamples = nrow(dataset)
          
          model = ML[[category]][[subcategory]]
          
          # Predict and create a confusion matrix
          
          # Make predictions according to model chosen
          if(subcategory == "100X Bagging"){
            predictions_frame = adabag::predict.bagging(model, dataset)
            predictions = predictions_frame[["class"]]
            confusion_matrix = table(predictions, dataset$Response)
            model_accuracy = 1 - (confusion_matrix[1] + confusion_matrix[4])/
              sum(confusion_matrix)
            predicted_probabilities = predictions_frame[["prob"]]
            errs_1 = err_metrics(confusion_matrix)
            colnames(errs_1) = c("Metrics", Legends[[1]])
          } else if(subcategory == "Boosting") {
            predictions_frame = predict(model, dataset)
            predictions = predictions_frame[["class"]]
            confusion_matrix = predictions_frame[["confusion"]]
            model_accuracy = 1 - (confusion_matrix[1] + confusion_matrix[4])/
              sum(confusion_matrix)
            predicted_probabilities = predictions_frame[["prob"]]
            errs_1 = err_metrics(confusion_matrix)
            colnames(errs_1) = c("Metrics", Legends[[1]])
          } else {
            predictions = predict(model, dataset)
            confusion_matrix = table(predictions, dataset$Response)
            model_accuracy = (confusion_matrix[1] + confusion_matrix[4])/
              sum(confusion_matrix)
            predicted_probabilities = predict(model, dataset, type = "prob")
            errs_1 = err_metrics(confusion_matrix)
            colnames(errs_1) = c("Metrics", Legends[[1]])
          }
          
          # ROC plot generation
          if(category == "Support Vector Machines"){
            print("ROC plots not available for Support Vector Machines")
          } else {
            join = cbind(predictions, predicted_probabilities, dataset)
            colnames(join)[c(2,3)] = c("Responder", "Non_responder")
            join = join[order(join$Responder),]
            model_roc = roc(predictor = join$Responder, 
                            response = as.character(join$Response))
            auc_values[[1]] = round(auc(model_roc), 3)
            p = plot(model_roc, 
                 main = "ROC curve",
                 col = randomColor(), lwd = 3, legacy.axes = TRUE, xlim = c(1,0), ylim = c(0,1), 
                 asp = 0.92, cex = 4, xaxs = "i", yaxs = "i", width = 900, height = 900)
          }
          
          # Plot legend
          Legends = c(Legends, input$breast_cancer_ml_legend_entry_2)
          
          # Variables for inputs
          category_2 = input$breast_cancer_ml_model_category_2
          subcategory_2 = input$breast_cancer_ml_model_subcategory_2
          ml_study_subset_2 = full_ml_set$Dataset %in% input$ml_breast_dataset_checkbox_2
          
          # Subset the data that the model will be assessed on
          dataset_2 = full_ml_set[ml_study_subset_2,] %>%
            dplyr::filter(Treatment %in% input$breast_cancer_ml_treatment_filter_2) %>%
            dplyr::filter(Timepoint %in% input$breast_cancer_ml_timepoint_filter_2) %>%
            dplyr::filter(Location %in% input$breast_cancer_ml_location_filter_2) %>%
            dplyr::filter(pam50 %in% input$breast_cancer_ml_pam50_filter_2) %>%
            dplyr::filter(rorS_risk %in% input$breast_cancer_ml_rorS_filter_2) %>%
            dplyr::filter(Mammaprint_risk %in% input$breast_cancer_ml_Mammaprint_filter_2) %>%
            dplyr::filter(Menopause.status %in% input$breast_cancer_ml_Meno_filter_2) %>%
            dplyr::filter(ER.status %in% input$breast_cancer_ml_ER_filter_2)
          
          # Number of samples in the filtered dataset
          nsamples = c(nsamples, nrow(dataset_2))
          
          model_2 = ML[[category_2]][[subcategory_2]]
          
          # Predict and create a confusion matrix
          
          # Make predictions according to model chosen
          if(subcategory_2 == "100X Bagging"){
            predictions_frame_2 = adabag::predict.bagging(model_2, dataset_2)
            predictions_2 = predictions_frame_2[["class"]]
            confusion_matrix_2 = table(predictions_2, dataset_2$Response)
            model_accuracy_2 = 1 - (confusion_matrix_2[1] + confusion_matrix_2[4])/
              sum(confusion_matrix_2)
            predicted_probabilities_2 = predictions_frame_2[["prob"]]
            errs_2 = err_metrics(confusion_matrix_2)
            colnames(errs_2) = c("Metrics", Legends[[2]])
          } else if(subcategory_2 == "Boosting") {
            predictions_frame_2 = predict(model_2, dataset_2)
            predictions_2 = predictions_frame_2[["class"]]
            confusion_matrix_2 = predictions_frame_2[["confusion"]]
            model_accuracy_2 = 1 - (confusion_matrix_2[1] + confusion_matrix_2[4])/
              sum(confusion_matrix_2)
            predicted_probabilities_2 = predictions_frame_2[["prob"]]
            errs_2 = err_metrics(confusion_matrix_2)
            colnames(errs_2) = c("Metrics", Legends[[2]])
          } else {
            predictions_2 = predict(model_2, dataset_2)
            confusion_matrix_2 = table(predictions_2, dataset_2$Response)
            model_accuracy_2 = (confusion_matrix_2[1] + confusion_matrix_2[4])/
              sum(confusion_matrix_2)
            predicted_probabilities_2 = predict(model_2, dataset_2, type = "prob")
            errs_2 = err_metrics(confusion_matrix_2)
            colnames(errs_2) = c("Metrics", Legends[[2]])
          }
          
          # ROC plot generation
          if(category_2 == "Support Vector Machines"){
            print("ROC plots not available for Support Vector Machines")
          } else {
            join_2 = cbind(predictions_2, predicted_probabilities_2, dataset_2)
            colnames(join_2)[c(2,3)] = c("Responder", "Non_responder")
            join_2 = join_2[order(join_2$Responder),]
            model_roc_2 = roc(predictor = join_2$Responder, 
                              response = as.character(join_2$Response))
            auc_values[[2]] = round(auc(model_roc_2), 3)
            p2 = plot(model_roc_2, col = randomColor(), lwd = 3, 
                      add = TRUE, cex = 4)
          }
          
          # Plot legend
          Legends = c(Legends, input$breast_cancer_ml_legend_entry_3)
          
          # Variables for inputs
          category_3 = input$breast_cancer_ml_model_category_3
          subcategory_3 = input$breast_cancer_ml_model_subcategory_3
          ml_study_subset_3 = full_ml_set$Dataset %in% input$ml_breast_dataset_checkbox_3
          
          # Subset the data that the model will be assessed on
          dataset_3 = full_ml_set[ml_study_subset_3,] %>%
            dplyr::filter(Treatment %in% input$breast_cancer_ml_treatment_filter_3) %>%
            dplyr::filter(Timepoint %in% input$breast_cancer_ml_timepoint_filter_3) %>%
            dplyr::filter(Location %in% input$breast_cancer_ml_location_filter_3) %>%
            dplyr::filter(pam50 %in% input$breast_cancer_ml_pam50_filter_3) %>%
            dplyr::filter(rorS_risk %in% input$breast_cancer_ml_rorS_filter_3) %>%
            dplyr::filter(Mammaprint_risk %in% input$breast_cancer_ml_Mammaprint_filter_3) %>%
            dplyr::filter(Menopause.status %in% input$breast_cancer_ml_Meno_filter_3) %>%
            dplyr::filter(ER.status %in% input$breast_cancer_ml_ER_filter_3)
          
          # Number of samples in the filtered dataset
          nsamples = c(nsamples, nrow(dataset_3))
          
          model_3 = ML[[category_3]][[subcategory_3]]
          
          # Predict and create a confusion matrix
          
          # Make predictions according to model chosen
          if(subcategory_3 == "100X Bagging"){
            predictions_frame_3 = adabag::predict.bagging(model_3, dataset_3)
            predictions_3 = predictions_frame_3[["class"]]
            confusion_matrix_3 = table(predictions_3, dataset_3$Response)
            model_accuracy_3 = 1 - (confusion_matrix_3[1] + confusion_matrix_3[4])/
              sum(confusion_matrix_3)
            predicted_probabilities_3 = predictions_frame_3[["prob"]]
            errs_3 = err_metrics(confusion_matrix_3)
            colnames(errs_3) = c("Metrics", Legends[[3]])
          } else if(subcategory_3 == "Boosting") {
            predictions_frame_3 = predict(model_3, dataset_3)
            predictions_3 = predictions_frame_3[["class"]]
            confusion_matrix_3 = predictions_frame_3[["confusion"]]
            model_accuracy_3 = 1 - (confusion_matrix_3[1] + confusion_matrix_3[4])/
              sum(confusion_matrix_3)
            predicted_probabilities_3 = predictions_frame_3[["prob"]]
            errs_3 = err_metrics(confusion_matrix_3)
            colnames(errs_3) = c("Metrics", Legends[[3]])
          } else {
            predictions_3 = predict(model_3, dataset_3)
            confusion_matrix_3 = table(predictions_3, dataset_3$Response)
            model_accuracy_3 = (confusion_matrix_3[1] + confusion_matrix_3[4])/
              sum(confusion_matrix_3)
            predicted_probabilities_3 = predict(model_3, dataset_3, type = "prob")
            errs_3 = err_metrics(confusion_matrix_3)
            colnames(errs_3) = c("Metrics", Legends[[3]])
          }
          
          # ROC plot generation
          if(category_3 == "Support Vector Machines"){
            print("ROC plots not available for Support Vector Machines")
          } else {
            join_3 = cbind(predictions_3, predicted_probabilities_3, dataset_3)
            colnames(join_3)[c(2,3)] = c("Responder", "Non_responder")
            join_3 = join_3[order(join_3$Responder),]
            model_roc_3 = roc(predictor = join_3$Responder, 
                              response = as.character(join_3$Response))
            auc_values[[3]] = round(auc(model_roc_3), 3)
            p3 = plot(model_roc_3, col = randomColor(), lwd = 3, 
                 add = TRUE, cex = 4)
          }
          return(list(plot = c(p, p2, p3), conf = c(confusion_matrix,
                      confusion_matrix_2, confusion_matrix_3), 
                      acc = c(model_accuracy, model_accuracy_2, model_accuracy_3),
                      Legends = Legends, auc_values = auc_values, nsamples = nsamples,
                      error_metrics = inner_join(inner_join(errs_1, errs_2, by = "Metrics"),
                                                 errs_3, by = "Metrics")))
              }
    
    })
    
  
  # Plot ROC curve
  output$breast_cancer_ROC_plot <- renderPlot({
    input$predict_ml_breast_cancer
    isolate({
      p = plot_breast_cancer_ROC()
      p$plot
      legend(0.5, 0.25, legend=paste0(p$Legends, ", AUC=", p$auc_values, 
                                       ", n=", p$nsamples),
             col=randomColor(count = length(p$Legends)), lty=1, cex=0.8)
      })
  })
  
  # Print error metrics
  output$breast_cancer_error_table <- DT::renderDataTable({
    input$predict_ml_breast_cancer
    isolate({
    plot_breast_cancer_ROC()$error_metrics})
  })
  
  # Select all button model 1
  observe({
    if(input$select_all_ml_model1_breast > 0){
      if (input$select_all_ml_model1_breast %% 2 == 0) {
        updateCheckboxGroupInput(session, "ml_breast_dataset_checkbox",
                                 choices = unique(full_ml_set$Dataset),
                                 selected = unique(full_ml_set$Dataset),
                                 inline = TRUE)
      }
      else
      {
        updateCheckboxGroupInput(session, "ml_breast_dataset_checkbox",
                                 choices = unique(full_ml_set$Dataset),
                                 selected = c(),
                                 inline = TRUE)
      }
    }
  })
  
  # Reset button model 1
  observeEvent(input$reset_input_breast_ml_model1, {
    shinyjs::reset("breast_cancer_ml_treatment_filter")
    shinyjs::reset("breast_cancer_ml_timepoint_filter")
    shinyjs::reset("breast_cancer_ml_location_filter")
    shinyjs::reset("breast_cancer_ml_pam50_filter")
    shinyjs::reset("breast_cancer_ml_rorS_filter")
    shinyjs::reset("breast_cancer_ml_Mammaprint_filter")
    shinyjs::reset("breast_cancer_ml_Meno_filter")
    shinyjs::reset("breast_cancer_ml_ER_filter") 
  })
  
  # Pop-up info message, triggered when the user presses the Info button
  output$ml_breast_info_text <- renderText({
    paste0("<br> &#8226 Support Vector Machines <b>do not produce ROC plots</b>, only error metrics.",
           "<br> &#8226 The final model we chose in our project is the <b>C5.0-ROC-optimised decision tree</b>.",
           "<br> &#8226 You can pick the model category and then subcategory you are interested in and then the study subset of interest.", 
           "<br> &#8226 You can also filter by multiple variables, for more specific model predictions. Excessive filtering may result in a NULL set.",
           "<br> &#8226 You can plot up to three ROC curves at the same plot, <b>but you have to click 'Apply' after you have specified your model and data subset and also provide an appropriate legend for each selection of yours</b>.",
           "<br> &#8226 After pressing 'Predict' and viewing your output, if you want to do multiple models again, you'll have to <b>press Apply</b> in <b>all</b> additional models before plotting.")})
  
  # Select all button model 2
  observe({
    if(input$select_all_ml_model2_breast > 0){
      if (input$select_all_ml_model2_breast %% 2 == 0) {
        updateCheckboxGroupInput(session, "ml_breast_dataset_checkbox_2",
                                 choices = unique(full_ml_set$Dataset),
                                 selected = unique(full_ml_set$Dataset),
                                 inline = TRUE)
      }
      else
      {
        updateCheckboxGroupInput(session, "ml_breast_dataset_checkbox_2",
                                 choices = unique(full_ml_set$Dataset),
                                 selected = c(),
                                 inline = TRUE)
      }
    }
  })
  
  # Reset button model 2
  observeEvent(input$reset_input_breast_ml_model2, {
    shinyjs::reset("breast_cancer_ml_treatment_filter_2")
    shinyjs::reset("breast_cancer_ml_timepoint_filter_2")
    shinyjs::reset("breast_cancer_ml_location_filter_2")
    shinyjs::reset("breast_cancer_ml_pam50_filter_2")
    shinyjs::reset("breast_cancer_ml_rorS_filter_2")
    shinyjs::reset("breast_cancer_ml_Mammaprint_filter_2")
    shinyjs::reset("breast_cancer_ml_Meno_filter_2")
    shinyjs::reset("breast_cancer_ml_ER_filter_2") 
  })
  
  # Select all button model 3
  observe({
    if(input$select_all_ml_model3_breast > 0){
      if (input$select_all_ml_model3_breast %% 2 == 0) {
        updateCheckboxGroupInput(session, "ml_breast_dataset_checkbox_3",
                                 choices = unique(full_ml_set$Dataset),
                                 selected = unique(full_ml_set$Dataset),
                                 inline = TRUE)
      }
      else
      {
        updateCheckboxGroupInput(session, "ml_breast_dataset_checkbox_3",
                                 choices = unique(full_ml_set$Dataset),
                                 selected = c(),
                                 inline = TRUE)
      }
    }
  })
  
  # Reset button model 3
  observeEvent(input$reset_input_breast_ml_model3, {
    shinyjs::reset("breast_cancer_ml_treatment_filter_3")
    shinyjs::reset("breast_cancer_ml_timepoint_filter_3")
    shinyjs::reset("breast_cancer_ml_location_filter_3")
    shinyjs::reset("breast_cancer_ml_pam50_filter_3")
    shinyjs::reset("breast_cancer_ml_rorS_filter_3")
    shinyjs::reset("breast_cancer_ml_Mammaprint_filter_3")
    shinyjs::reset("breast_cancer_ml_Meno_filter_3")
    shinyjs::reset("breast_cancer_ml_ER_filter_3") 
  })
  
  
  # Breast Cancer: Custom DGEA #####
  
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
    topTable$volc_p_status[topTable$adj.P.Val>input$pval_breast_custom &
                             abs(topTable$logFC)<input$logFC_breast_custom] = "Not Significant"
    topTable$volc_p_status[topTable$adj.P.Val>input$pval_breast_custom &
                             abs(topTable$logFC)>input$logFC_breast_custom] = 
      paste0("|DE|>", input$logFC_breast_custom," s.d. & p>", input$pval_breast_custom)
    topTable$volc_p_status[topTable$adj.P.Val<input$pval_breast_custom &
                             abs(topTable$logFC)<input$logFC_breast_custom] = 
      paste0("|DE|<", input$logFC_breast_custom," s.d. & p<", input$pval_breast_custom)
    topTable$volc_p_status[topTable$adj.P.Val<input$pval_breast_custom &
                             abs(topTable$logFC)>input$logFC_breast_custom] = 
      paste0("|DE|>", input$logFC_breast_custom," s.d. & p<", input$pval_breast_custom)
    topTable$volc_p_status = factor(topTable$volc_p_status,
                                    levels = c("Not Significant", 
                                               paste0("|DE|>", input$logFC_breast_custom," s.d. & p>", input$pval_breast_custom),
                                               paste0("|DE|<", input$logFC_breast_custom," s.d. & p<", input$pval_breast_custom), 
                                               paste0("|DE|>", input$logFC_breast_custom," s.d. & p<", input$pval_breast_custom)),
                                    labels = c("Not Significant", 
                                               paste0("|DE|>", input$logFC_breast_custom," s.d. & p>", input$pval_breast_custom),
                                               paste0("|DE|<", input$logFC_breast_custom," s.d. & p<", input$pval_breast_custom), 
                                               paste0("|DE|>", input$logFC_breast_custom," s.d. & p<", input$pval_breast_custom)))
    
    # Create the palette for the scatter colors
    pal = c(input$scatter_color_breast_NS_custom, 
            input$scatter_color_breast_DE_only_custom, 
            input$scatter_color_breast_p_only_custom, 
            input$scatter_color_breast_DE_p_custom)
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
    paste0("<br> &#8226 This analysis is run on <b>training</b> and <b>validation</b> samples only.",
           "<br> &#8226 You can select how to filter the combined samples based on multiple variables.",
           "<br> &#8226 You must choose at least one variable for adjustment. Results are by default adjusted for dataset.",
           "<br> &#8226 Select level 1 and level 2 for the contrasts, i.e. the groups you want
                           to compare. Results should be interpreted as down-/up-regulated in level 1 compared to level 2.",
           "<br> &#8226 <b>You must refresh the page after each time you press the 'Analyse' button to produce new results</b>.")
  })
}


# UI
ui <- dashboardPage(skin = "blue", # color of the top of the dashboard page
                    header = header,
                    sidebar = sidebar,
                    body = body)
shinyApp(ui, server)
