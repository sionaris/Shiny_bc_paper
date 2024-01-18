# Header #####
header <- dashboardHeader(title = "My PhD project")

# Body #####
body <- dashboardBody(tabItems
                      (
                        # Add tab items for breast cancer, PDAC and OSCC
                        # Breast Cancer - general tab
                        tabItem(tabName = "breast_cancer_project"),
                        
                        # Breast Cancer - exploratory analysis #####
                        tabItem(
                          tabName = "breast_cancer_exploratory",
                          h2("Exploratory analysis plots and tables"),
                          fluidRow(
                            # Histogram plot ##
                            box(
                              id = "Histogram_breast",
                              title = "Histogram",
                              status = "primary",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              width = 7,
                              height = 400,
                              plotlyOutput("breast_histogram")
                            ),
                            
                            # Histogram tuning
                            box(
                              id = "Histogram_tuning_breast",
                              title = "Histogram tuning",
                              status = "warning",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              width = 5,
                              height = 400,
                              shinyjs::useShinyjs(),
                              
                              # Select variable to plot
                              div(
                                id = "div_histvar_select_breast",
                                style = "display:inline-block; width: 32%; margin-left: 7.5%; margin-right: 5%",
                                selectInput(
                                  "histvar_select_breast",
                                  "Select variable",
                                  choices = sort(
                                    c(
                                      "pam50",
                                      "scmod1",
                                      "ClaudinLow",
                                      "IC10",
                                      "Mammaprint_risk",
                                      "Mammaprint_score",
                                      "rorS_risk",
                                      "rorS_score",
                                      "Location",
                                      "Menopause.status",
                                      "ER.status",
                                      "Year",
                                      "Platform",
                                      "Platform_comp",
                                      "Treatment",
                                      "Response"
                                    )
                                  ),
                                  selected = "pam50"
                                )
                              ),
                              
                              # Select type of histogram (probability, percentage, classic)
                              div(
                                id = "div_hist_type_breast",
                                style = "display:inline-block; width: 38%; margin-left: 5%; margin-right: 5%",
                                selectInput(
                                  "hist_type_breast",
                                  "Select type of histogram",
                                  choices = list(
                                    classic = "classic",
                                    probability = "probability",
                                    percent = "percentage"
                                  ),
                                  selected = "classic"
                                )
                              ),
                              
                              # div for checkbox input and select-all button
                              div(
                                id = "div_checkbox_selectall_group_breast_hist",
                                style = "width: 100%;",
                                # Select the datasets that you want to include in the plot
                                div(
                                  id = "div_hist_dataset_checkbox",
                                  style = "display: inline-block; width: 55%; margin-left: 7.5%;",
                                  checkboxGroupInput(
                                    "hist_dataset_checkbox",
                                    "Select studies",
                                    selected = unique(breast_cancer_full_pheno$Dataset),
                                    choices = unique(breast_cancer_full_pheno$Dataset),
                                    inline = TRUE
                                  )
                                ),
                                
                                # add "select all" button for the datasets to choose from
                                div(
                                  id = "div_select_all_hist_breast",
                                  style = "display: inline-block; margin: 5% 5% 0% 5%; width: 25%; position: absolute;",
                                  actionButton(
                                    "select_all_hist_breast",
                                    "Select/De-select All",
                                    style = "background-color: #F8ECBB; font-weight: bold; border-radius: 10px;",
                                    icon = icon("check", style = "margin-right: 5px; vertical-align: middle;")
                                  )
                                )
                              ),
                              
                              # Select colors for fill and outline of the bins
                              div(
                                id = "div_hist_fill_breast",
                                style = "display:inline-block; margin-top: 0%; margin-left: 7.5%; margin-right: 2.5%; width: 25%;",
                                colourpicker::colourInput(
                                  "hist_fill_breast",
                                  "Bin fill color",
                                  value = "#1194B1",
                                  allowTransparent = TRUE
                                )
                              ),
                              div(
                                id = "div_hist_color_breast",
                                style = "display:inline-block; margin-left: 2.5%; margin-right: 2.5%; width: 25%",
                                colourpicker::colourInput(
                                  "hist_color_breast",
                                  "Bin outline color",
                                  value = "#000000",
                                  allowTransparent = TRUE
                                )
                              ),
                              
                              # Select number of bins
                              div(
                                id = "div_hist_breast_bins",
                                style = "display:inline-block; margin-left: 2.5%; margin-right: 5%; width: 25%",
                                numericInput(
                                  "hist_breast_bins",
                                  "Select # of bins",
                                  value = 10,
                                  min = 0,
                                  max = Inf,
                                  width = "150px"
                                )
                              ),
                              
                              # div for action buttons
                              div(
                                id = "div_action_buttons_hist_breast",
                                style = "display: flex; width: 100%; align-content: center; justify-content: center",
                                # Button to trigger plot generation/update
                                div(
                                  id = "div_draw_breast_hist",
                                  # style = "display: flex; margin-right: 3%; margin-top: 3%;",
                                  tags$head(
                                    tags$link(rel = "stylesheet", type = "text/css",
                                              href = "rgb_button_css.css")
                                  ),
                                  actionButton(
                                    inputId = "draw_breast_hist",
                                    label = "Draw!",
                                    icon = icon("paintbrush", style = "margin-right: 5px; vertical-align: middle;")
                                  ) %>%
                                    tagAppendAttributes(class = 'rgb-button')
                                ),
                                
                                # Button to reset inputs to default
                                div(
                                  id = "div_default_breast_hist",
                                  style = "padding-top: 0.5%; padding-left: 6.5%;",
                                  tags$head(
                                    tags$link(rel = "stylesheet", type = "text/css",
                                              href = "default_button.css")
                                  ),
                                  actionButton(
                                    inputId = "reset_input_breast_hist",
                                    label = "Default parameters",
                                    icon = icon("repeat", style = "margin-right: 5px; vertical-align: middle;")
                                  ) %>%
                                    tagAppendAttributes(class = 'default-button')
                                ),
                                
                                # Add an info button (pop up with shinyalert())
                                div(
                                  id = "div_info_breast_hist",
                                  style = "padding-left: 6.5%;",
                                  # style = "display: inline-flex; margin-left: 3%; margin-top: 3%;",
                                  tags$head(
                                    tags$link(rel = "stylesheet", type = "text/css",
                                              href = "info_button.css")
                                  ),
                                  actionButton(
                                    inputId = "hist_breast_info",
                                    label = "Info",
                                    icon = icon("circle-info", style = "margin-right: 5px; vertical-align: middle;")
                                  ) %>%
                                    tagAppendAttributes(class = 'info-button')
                                )
                              )
                            ),
                            
                            bsModal(
                              "histogram_breast_info",
                              "Information",
                              "hist_breast_info",
                              fluidRow(htmlOutput("hist_breast_info_text"))
                            ),
                            
                            # Bar chart ##
                            box(
                              id = "Barchart_plot_breast",
                              title = "Bar chart",
                              status = "primary",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              width = 7,
                              height = 400,
                              plotlyOutput("breast_barchart")
                            ),
                            
                            # Bar chart tuning
                            box(
                              id = "Barchart_plot_tuning_breast",
                              title = "Bar chart tuning",
                              status = "warning",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              width = 5,
                              height = 400,
                              shinyjs::useShinyjs(),
                              
                              # Select main variable to plot
                              div(
                                id = "div_barchart_varmain_select_breast",
                                style = "display:inline-block; width: 27.5%; margin-left: 3%; margin-right: 2.5%",
                                selectInput(
                                  "barchart_varmain_select_breast",
                                  "Main variable",
                                  choices = sort(
                                    c(
                                      "pam50",
                                      "scmod1",
                                      "ClaudinLow",
                                      "IC10",
                                      "Mammaprint_risk",
                                      "rorS_risk",
                                      "Location",
                                      "Menopause.status",
                                      "ER.status",
                                      "Year",
                                      "Platform",
                                      "Platform_comp",
                                      "Treatment",
                                      "Response"
                                    )
                                  ),
                                  selected = "pam50"
                                )
                              ),
                              
                              # Select clustering variable (optional)
                              div(
                                id = "div_barchart_varclust_select_breast",
                                style = "display:inline-block; width: 27.5%; margin-left: 2.5%; margin-right: 2.5%",
                                selectInput(
                                  "barchart_varclust_select_breast",
                                  "Second variable (optional)",
                                  choices = c("None", sort(
                                    c(
                                      "pam50",
                                      "scmod1",
                                      "ClaudinLow",
                                      "IC10",
                                      "Mammaprint_risk",
                                      "rorS_risk",
                                      "Location",
                                      "Menopause.status",
                                      "ER.status",
                                      "Year",
                                      "Platform",
                                      "Platform_comp",
                                      "Treatment",
                                      "Response"
                                    )
                                  )),
                                  selected = "None"
                                )
                              ),
                              
                              # Select type of bar chart (clustered, stacked) - if a second variable is selected
                              div(
                                id = "div_barchart_type_breast",
                                style = "display:inline-block; width: 27.5%; margin-left: 2.5%; margin-right: 3%",
                                selectInput(
                                  "barchart_type_breast",
                                  "Select type of bar chart",
                                  choices = c("group", "stack"),
                                  selected = "group"
                                )
                              ),
                              
                              # div for checkbox input and select-all button
                              div(
                                id = "div_checkbox_selectall_group_breast_barchart",
                                style = "width: 100%;",
                                # Select the datasets that you want to include in the plot
                                div(
                                  id = "div_barchart_dataset_checkbox",
                                  style = "display: inline-block; width: 55%; margin-left: 3%;",
                                  checkboxGroupInput(
                                    "barchart_dataset_checkbox",
                                    "Select studies",
                                    selected = unique(breast_cancer_full_pheno$Dataset),
                                    choices = unique(breast_cancer_full_pheno$Dataset),
                                    inline = TRUE
                                  )
                                ),
                                
                                # add "select all" button for the datasets to choose from
                                div(
                                  id = "div_select_all_barchart_breast",
                                  style = "display: inline-block; margin: 5% 3% 0% 7%; width: 25%; position: absolute;",
                                  actionButton(
                                    "select_all_barchart_breast",
                                    "Select/De-select All",
                                    style = "background-color: #F8ECBB; font-weight: bold; border-radius: 10px;",
                                    icon = icon("check", style = "margin-right: 5px; vertical-align: middle;")
                                  )
                                )
                              ),
                              
                              # div for color and fill selections for barchart
                              div(
                                id = "div_fill_color_group_barachart",
                                style = "display: flex; justify-content: center; align-content:center;",
                                # Select colors for fill and outline of the bins
                                div(
                                  id = "div_barchart_fill_breast",
                                  style = "display:inline-block; margin-top: 0%; margin-left: 3%; margin-right: 7.5%; width: 35%;",
                                  colourpicker::colourInput(
                                    "barchart_fill_breast",
                                    "Bin fill color",
                                    value = "#9419B2",
                                    allowTransparent = TRUE
                                  )
                                ),
                                div(
                                  id = "div_barchart_color_breast",
                                  style = "display:inline-block; margin-left: 7.5%; margin-right: 3%; width: 35%",
                                  colourpicker::colourInput(
                                    "barchart_color_breast",
                                    "Bin outline color",
                                    value = "#000000",
                                    allowTransparent = TRUE
                                  )
                                )
                              ),
                              
                              # div for action buttons
                              div(
                                id = "div_action_buttons_hist_breast",
                                style = "display: flex; width: 100%; align-content: center; justify-content: center",
                                # Button to trigger plot generation/update
                                div(
                                  id = "div_draw_breast_hist",
                                  actionButton(
                                    inputId = "draw_breast_hist",
                                    label = "Draw!",
                                    icon = icon("paintbrush", style = "margin-right: 5px; vertical-align: middle;")
                                  ) %>%
                                    tagAppendAttributes(class = 'rgb-button')
                                ),
                                
                                # Button to reset inputs to default
                                div(
                                  id = "div_default_breast_hist",
                                  actionButton(
                                    inputId = "reset_input_breast_barchart",
                                    label = "Default parameters",
                                    icon = icon("repeat", style = "margin-right: 5px; vertical-align: middle;")
                                  ) %>%
                                    tagAppendAttributes(class = 'default-button'),
                                  style = "padding-top: 0.5%; padding-left: 6.5%;"
                                ),
                                
                                # Add an info button (pop up with shinyalert())
                                div(
                                  id = "div_info_breast_hist",
                                  style = "padding-left: 6.5%;",
                                  actionButton(
                                    inputId = "barchart_breast_info",
                                    label = "Info",
                                    icon = icon("circle-info", style = "margin-right: 5px; vertical-align: middle;")
                                  ) %>%
                                    tagAppendAttributes(class = 'info-button')
                                )
                              )
                            ),
                            
                            bsModal(
                              "barchart_breast_information",
                              "Information",
                              "barchart_breast_info",
                              fluidRow(htmlOutput("barchart_breast_info_text"))
                            )
                          )
                        ),
                        
                        # Breast Cancer - sunburst tab #####
                        tabItem(
                          tabName = "breast_cancer_sunburst",
                          h2("Sunburst plots: analytical dataset and consensus subtypes"),
                          fluidRow(
                            # tabBox of plotting sunbursts
                            tabBox(
                              title = "Sunburst plots",
                              id = "sunburst_tabs_breast",
                              height = 700,
                              width = 7,
                              selected = "Analytical data sunburst plot",
                              
                              # Panel for analytical set
                              tabPanel(
                                "Analytical data sunburst plot",
                                title = "Analytical dataset",
                                plotlyOutput("analytical_sunburst_breast")
                              ),
                              
                              # Panel for consensus set
                              tabPanel(
                                "Consensus data sunburst plot",
                                title = "Consensus subtypes",
                                plotlyOutput("consensus_sunburst_breast")
                              )
                            ),
                            
                            # tabBox of tuning sunbursts
                            tabBox(
                              title = "Sunburst tuning",
                              id = "sunburst_tuning_breast",
                              height = 700,
                              width = 5,
                              selected = "Analytical tuning",
                              
                              # Panel for analytical set
                              tabPanel(
                                "Analytical tuning",
                                title = "Analytical dataset",
                                shinyjs::useShinyjs(),
                                
                                # Select variable 1 for analytical set
                                selectInput(
                                  "sunburst_var1_select_breast",
                                  "Select root variable",
                                  choices = sort(
                                    c(
                                      "pam50",
                                      "scmod1",
                                      "ClaudinLow",
                                      Timepoint = "Timepoint_coded",
                                      "Mammaprint_risk",
                                      "rorS_risk",
                                      "Location",
                                      "Menopause.status",
                                      "Dataset",
                                      "None",
                                      "ER.status",
                                      "Treatment",
                                      "Response"
                                    )
                                  ),
                                  selected = "Treatment"
                                ),
                                
                                # Select variable 2 for analytical set
                                selectInput(
                                  "sunburst_var2_select_breast",
                                  "Select second variable",
                                  choices = sort(
                                    c(
                                      "pam50",
                                      "scmod1",
                                      "ClaudinLow",
                                      Timepoint = "Timepoint_coded",
                                      "Mammaprint_risk",
                                      "rorS_risk",
                                      "Location",
                                      "Menopause.status",
                                      "Dataset",
                                      "None",
                                      "ER.status",
                                      "Treatment",
                                      "Response"
                                    )
                                  ),
                                  selected = "Dataset"
                                ),
                                
                                # Select variable 3 for analytical set
                                selectInput(
                                  "sunburst_var3_select_breast",
                                  "Select third variable",
                                  choices = sort(
                                    c(
                                      "pam50",
                                      "scmod1",
                                      "ClaudinLow",
                                      Timepoint = "Timepoint_coded",
                                      "Mammaprint_risk",
                                      "rorS_risk",
                                      "Location",
                                      "Menopause.status",
                                      "Dataset",
                                      "None",
                                      "ER.status",
                                      "Treatment",
                                      "Response"
                                    )
                                  ),
                                  selected = "Response"
                                ),
                                
                                # Select variable 4 for analytical set
                                selectInput(
                                  "sunburst_var4_select_breast",
                                  "Select fourth variable",
                                  choices = sort(
                                    c(
                                      "pam50",
                                      "scmod1",
                                      "ClaudinLow",
                                      Timepoint = "Timepoint_coded",
                                      "Mammaprint_risk",
                                      "rorS_risk",
                                      "Location",
                                      "Menopause.status",
                                      "Dataset",
                                      "None",
                                      "ER.status",
                                      "Treatment",
                                      "Response"
                                    )
                                  ),
                                  selected = "pam50"
                                ),
                                
                                # add "select all" button for the datasets to choose from
                                actionButton(
                                  "select_all_sunburst_analytical_breast",
                                  "Select/De-select All",
                                  style = "background-color: #F8ECBB; font-weight: bold; border-radius: 10px;",
                                  icon = icon("check", style = "margin-right: 5px; vertical-align: middle;")
                                ),
                                
                                # Select the datasets that you want to include in the plot
                                checkboxGroupInput(
                                  "sunburst_analytical_dataset_checkbox",
                                  "Select studies",
                                  selected = unique(breast_cancer_full_pheno$Dataset),
                                  choices = unique(breast_cancer_full_pheno$Dataset),
                                  inline = TRUE
                                ),
                                
                                # Select opacity of data points
                                sliderInput(
                                  "alpha_breast_sunburst_analytical",
                                  "Opacity (alpha)",
                                  min = 0,
                                  max = 1,
                                  value = 0.9
                                ),
                                
                                # div for action buttons
                                div(
                                  id = "div_action_buttons_analytical_sunburst_breast",
                                  style = "display: flex; width: 100%; align-content: center; justify-content: center",
                                  # Button to trigger plot generation/update
                                  div(
                                    id = "div_draw_analytical_sunburst_breast",
                                    actionButton(
                                      inputId = "draw_breast_sunburst_analytical",
                                      label = "Draw!",
                                      icon = icon("paintbrush", style = "margin-right: 5px; vertical-align: middle;")
                                    ) %>%
                                      tagAppendAttributes(class = 'rgb-button')
                                  ),
                                  
                                  # Button to reset inputs to default
                                  div(
                                    id = "div_default_analytical_sunburst_breast",
                                    actionButton(
                                      inputId = "reset_input_breast_sunburst_analytical",
                                      label = "Default parameters",
                                      icon = icon("repeat", style = "margin-right: 5px; vertical-align: middle;")
                                    ) %>%
                                      tagAppendAttributes(class = 'default-button'),
                                    style = "padding-top: 0.5%; padding-left: 6.5%;"
                                  ),
                                  
                                  # Add an info button (pop up with shinyalert())
                                  div(
                                    id = "div_info_analytical_sunburst_breast",
                                    style = "padding-left: 6.5%;",
                                    actionButton(
                                      inputId = "sunburst_analytical_breast_info",
                                      label = "Info",
                                      icon = icon("circle-info", style = "margin-right: 5px; vertical-align: middle;")
                                    ) %>%
                                      tagAppendAttributes(class = 'info-button')
                                  )
                                ),
                                
                                bsModal(
                                  "analytical_sunburst_breast_info",
                                  "Information",
                                  "sunburst_analytical_breast_info",
                                  fluidRow(htmlOutput("analytical_sunburst_breast_info_text"))
                                )
                              ),
                              
                              # Panel for consensus set
                              tabPanel(
                                id = "Consensus tuning",
                                title = "Consensus subtypes",
                                shinyjs::useShinyjs(),
                                
                                # Select variable 1 for consensus set
                                selectInput(
                                  "sunburst_var1_select_breast_consensus",
                                  "Select root variable",
                                  choices = sort(
                                    c(
                                      "pam50",
                                      "scmod1",
                                      "ClaudinLow",
                                      "Timepoint",
                                      "Mammaprint_risk",
                                      "rorS_risk",
                                      "Location",
                                      "Menopause.status",
                                      "Dataset",
                                      "None",
                                      "ER.status",
                                      "Treatment",
                                      "Response",
                                      "Cluster"
                                    )
                                  ),
                                  selected = "Cluster"
                                ),
                                
                                # Select variable 2 for consensus set
                                selectInput(
                                  "sunburst_var2_select_breast_consensus",
                                  "Select second variable",
                                  choices = sort(
                                    c(
                                      "pam50",
                                      "scmod1",
                                      "ClaudinLow",
                                      "Timepoint",
                                      "Mammaprint_risk",
                                      "rorS_risk",
                                      "Location",
                                      "Menopause.status",
                                      "Dataset",
                                      "None",
                                      "ER.status",
                                      "Treatment",
                                      "Response",
                                      "Cluster"
                                    )
                                  ),
                                  selected = "Dataset"
                                ),
                                
                                # Select variable 3 for consensus set
                                selectInput(
                                  "sunburst_var3_select_breast_consensus",
                                  "Select third variable",
                                  choices = sort(
                                    c(
                                      "pam50",
                                      "scmod1",
                                      "ClaudinLow",
                                      "Timepoint",
                                      "Mammaprint_risk",
                                      "rorS_risk",
                                      "Location",
                                      "Menopause.status",
                                      "Dataset",
                                      "None",
                                      "ER.status",
                                      "Treatment",
                                      "Response",
                                      "Cluster"
                                    )
                                  ),
                                  selected = "Response"
                                ),
                                
                                # Select variable 4 for consensus set
                                selectInput(
                                  "sunburst_var4_select_breast_consensus",
                                  "Select fourth variable",
                                  choices = sort(
                                    c(
                                      "pam50",
                                      "scmod1",
                                      "ClaudinLow",
                                      "Timepoint",
                                      "Mammaprint_risk",
                                      "rorS_risk",
                                      "Location",
                                      "Menopause.status",
                                      "Dataset",
                                      "None",
                                      "ER.status",
                                      "Treatment",
                                      "Response",
                                      "Cluster"
                                    )
                                  ),
                                  selected = "Mammaprint_risk"
                                ),
                                
                                # add "select all" button for the datasets to choose from
                                actionButton(
                                  "select_all_sunburst_consensus_breast",
                                  "Select/De-select All",
                                  style = "background-color: #F8ECBB; font-weight: bold; border-radius: 10px;",
                                  icon = icon("check", style = "margin-right: 5px; vertical-align: middle;")
                                ),
                                
                                # Select the datasets that you want to include in the plot
                                checkboxGroupInput(
                                  "sunburst_consensus_dataset_checkbox",
                                  "Select studies",
                                  selected = unique(breast_cancer_consensus_set$Dataset),
                                  choices = unique(breast_cancer_consensus_set$Dataset),
                                  inline = TRUE
                                ),
                                
                                # Select opacity of data points
                                sliderInput(
                                  "alpha_breast_sunburst_consensus",
                                  "Opacity (alpha)",
                                  min = 0,
                                  max = 1,
                                  value = 0.9
                                ),
                                
                                # div for action buttons
                                div(
                                  id = "div_action_buttons_consensus_sunburst_breast",
                                  style = "display: flex; width: 100%; align-content: center; justify-content: center",
                                  # Button to trigger plot generation/update
                                  div(
                                    id = "div_draw_consensus_sunburst_breast",
                                    actionButton(
                                      inputId = "draw_breast_sunburst_consensus",
                                      label = "Draw!",
                                      icon = icon("paintbrush", style = "margin-right: 5px; vertical-align: middle;")
                                    ) %>%
                                      tagAppendAttributes(class = 'rgb-button')
                                  ),
                                  
                                  # Button to reset inputs to default
                                  div(
                                    id = "div_default_consensus_sunburst_breast",
                                    actionButton(
                                      inputId = "reset_input_breast_sunburst_consensus",
                                      label = "Default parameters",
                                      icon = icon("repeat", style = "margin-right: 5px; vertical-align: middle;")
                                    ) %>%
                                      tagAppendAttributes(class = 'default-button'),
                                    style = "padding-top: 0.5%; padding-left: 6.5%;"
                                  ),
                                  
                                  # Add an info button (pop up with shinyalert())
                                  div(
                                    id = "div_info_consensus_sunburst_breast",
                                    style = "padding-left: 6.5%;",
                                    actionButton(
                                      inputId = "sunburst_consensus_breast_info",
                                      label = "Info",
                                      icon = icon("circle-info", style = "margin-right: 5px; vertical-align: middle;")
                                    ) %>%
                                      tagAppendAttributes(class = 'info-button')
                                  )
                                ),
                                
                                bsModal(
                                  "consensus_sunburst_breast_info",
                                  "Information",
                                  "sunburst_consensus_breast_info",
                                  fluidRow(htmlOutput("consensus_sunburst_breast_info_text"))
                                )
                              )
                            )
                          )
                        ),
                        
                        # Breast Cancer - volcano #####
                        tabItem(
                          tabName = "breast_cancer_subItem_volcano",
                          h2("Neoadjuvant Treatment (NAT) in Breast Cancer"),
                          tags$head(
                            tags$link(rel = "stylesheet",
                                      type = "text/css",
                                      href = "volcano_waiter_adjustment.css")
                          ),
                          fluidRow(
                            # Volcano plot
                            div(
                              id = "div_volcano_plot_box",
                              box(
                                id = "Volcano_plot_breast",
                                title = "Volcano plot",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = 8,
                                height = 820,
                                useWaiter(),
                                plotlyOutput("breast_volcano")
                              )
                            ),
                            
                            # Interactive switches and tools for plotting the volcano
                            box(
                              id = "Volcano_tuning_breast",
                              title = "Volcano tuning",
                              status = "warning",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              width = 4,
                              height = 820,
                              shinyjs::useShinyjs(),
                              
                              # Select p-value for y-axis (corrected/uncorrected)
                              selectInput(
                                "pval_selector_breast",
                                "p-value to plot",
                                choices = c("P.Value", "adj.P.Val"),
                                selected = "adj.P.Val"
                              ),
                              
                              # Select x-coordinate for vertical logFC lines
                              numericInput(
                                "logFC_breast",
                                "DET: Diff. Exp. Threshold - vertical lines",
                                value = 0.25,
                                min = 0,
                                max = Inf
                              ),
                              
                              # Select y-coordinate for p-value threshold
                              numericInput(
                                "pval_breast",
                                "PVT (adj.P.Val threshold) - horizontal line",
                                value = 0.05,
                                min = 0,
                                max = Inf
                              ),
                              
                              # Select colors for scatter points
                              colourpicker::colourInput(
                                "scatter_color_breast_NS",
                                "Color for p>PVT (n.s. results)",
                                value = "#BEBEBE"
                              ),
                              colourpicker::colourInput(
                                "scatter_color_breast_p_only",
                                "Color for DE<|DET| & p<PVT",
                                value = "#FFC0CB"
                              ),
                              colourpicker::colourInput(
                                "scatter_color_breast_downr",
                                "Color for DE<-|DET| & p<PVT",
                                value = "royalblue"
                              ),
                              colourpicker::colourInput(
                                "scatter_color_breast_upr",
                                "Color for DE>|DET| & p<PVT",
                                value = "red4"
                              ),
                              
                              # Select opacity of data points
                              sliderInput(
                                "alpha_breast_volcano",
                                "Opacity (alpha)",
                                min = 0,
                                max = 1,
                                value = 0.7
                              ),
                              
                              # Write down a representative title for the plot
                              textInput(
                                "breast_volcano_title",
                                "Set a title for your plot",
                                value = c("Volcano plot: NAT responders vs. non-responders")
                              ),
                              
                              div(
                                id = "div_action_buttons_consensus_sunburst_breast",
                                style = "display: flex; width: 100%; align-content: center; justify-content: center",
                                # Button to trigger plot generation/update
                                div(
                                  id = "div_draw_breast_volcano",
                                  actionButton(
                                    inputId = "draw_breast_volcano",
                                    label = "Draw!",
                                    icon = icon("paintbrush", style = "margin-right: 5px; vertical-align: middle;")
                                  ) %>%
                                    tagAppendAttributes(class = "rgb-button")
                                ),
                                
                                # Button to reset inputs to default
                                div(
                                  id = "div_default_breast_volcano",
                                  actionButton(
                                    inputId = "reset_input_breast_volcano",
                                    label = "Default parameters",
                                    icon = icon("repeat", style = "margin-right: 5px; vertical-align: middle;")
                                  ) %>%
                                    tagAppendAttributes(class = "default-button"),
                                  style = "padding-left: 5%;"
                                ),
                                
                                # Add an info button (pop up with shinyalert())
                                div(
                                  id = "div_breast_volcano_info_button",
                                  style = "padding-left: 5%; padding-top: -5%;",
                                  actionButton(
                                    inputId = "volcano_breast_info",
                                    label = "Info",
                                    icon = icon("circle-info", style = "margin-right: 5px; vertical-align: middle;")
                                  ) %>%
                                    tagAppendAttributes(class = "info-button")
                                )
                              ),
                              
                              bsModal(
                                "preset_volcano_breast_info",
                                "Information",
                                "volcano_breast_info",
                                fluidRow(htmlOutput("volcano_breast_info_text"))
                              )
                            ),
                            
                            # Hovering info in tuning parameters
                            bsTooltip(
                              "pval_selector_breast",
                              "Select p-value for plotting on y-axis (-log10(p-value))",
                              placement = "bottom",
                              trigger = "hover",
                              options = NULL
                            ),
                            bsTooltip(
                              "logFC_breast",
                              "x-coordinate for symmetrical vertical lines (illustrative purposes)",
                              placement = "bottom",
                              trigger = "hover",
                              options = NULL
                            ),
                            bsTooltip(
                              "pval_breast",
                              "y-coordinate for horizontal p-value threshold (default:0.05)",
                              placement = "bottom",
                              trigger = "hover",
                              options = NULL
                            )
                          )
                        ),
                        
                        # Breast Cancer - Machine Learning (Response prediction models) #####
                        tabItem(
                          tabName = "breast_cancer_subItem_ml",
                          h2("Machine Learning"),
                          fluidRow(
                            # Interactive model and data selection panel
                            box(
                              id = "ML_control_breast",
                              title = "Select model and data",
                              status = "primary",
                              solidHeader = TRUE,
                              width = 12,
                              height = 270,
                              shinyjs::useShinyjs(),
                              
                              splitLayout(
                                cellWidths = c("200px", "250px", "350px", "200px"),
                                style = "border:1px;padding:10px;white-space:normal;",
                                # Select ML model category
                                radioButtons(
                                  inputId = "breast_cancer_ml_model_category_1",
                                  label = "Select model category",
                                  choices = names(ML),
                                  selected = "Decision Trees",
                                  width = "150px"
                                ),
                                
                                # Updatable radioButtons for model subcategory
                                radioButtons(
                                  "breast_cancer_ml_model_subcategory_1",
                                  "Select model subcategory",
                                  choices = sort(names(ML[["Decision Trees"]])),
                                  selected = "C5.0 - ROC",
                                  width = "250px"
                                ),
                                
                                # Select the datasets that you want to include in the plot
                                checkboxGroupInput(
                                  "ml_breast_dataset_checkbox",
                                  "Select studies",
                                  selected = unique(full_ml_set$Dataset),
                                  choices = unique(full_ml_set$Dataset),
                                  inline = TRUE,
                                  width = "300px"
                                ),
                                
                                # add "select all" button for the datasets to choose from
                                actionButton(
                                  "select_all_ml_model1_breast",
                                  "Select/De-select All",
                                  style = "background-color: #F8ECBB; font-weight: bold; border-radius: 10px;",
                                  icon = icon("check", style = "margin-right: 5px; vertical-align: middle;")
                                )
                              )
                            ),
                            # Filtering the subset of the data based on variables for more specific prediction
                            
                            # A wide box with checkboxes
                            box(
                              id = "ML_breast_cancer_variable_filtering",
                              title = "Variable filtering",
                              status = "primary",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              width = 12,
                              fluidRow(
                                column(
                                  3,
                                  wellPanel(
                                    style = "background-color: transparent; box-shadow: none; margin-top: 2%; padding-right: 2.5%; padding-left: 2.5%; padding-top:2%; padding-bottom: 2%; border: 3px solid #093773; border-radius: 15px; height: 210px;",
                                    tags$h4("Clinical variables", style = "font-weight: bold;"),
                                    div(
                                      style = "display: flex; justify-content: space-between;",
                                      checkboxGroupInput(
                                        "breast_cancer_ml_treatment_filter",
                                        "Treatment",
                                        selected = unique(Pheno_exprs$Treatment),
                                        choices = unique(Pheno_exprs$Treatment)
                                      ),
                                      checkboxGroupInput(
                                        "breast_cancer_ml_timepoint_filter",
                                        "Timepoint",
                                        selected = unique(Pheno_exprs$Timepoint_coded),
                                        choices = unique(Pheno_exprs$Timepoint_coded)
                                      ),
                                      checkboxGroupInput(
                                        "breast_cancer_ml_pam50_filter",
                                        "pam50",
                                        selected = unique(Pheno_exprs$pam50),
                                        choices = unique(Pheno_exprs$pam50)
                                      )
                                    )
                                  )
                                ),
                                column(
                                  3,
                                  wellPanel(
                                    style = "background-color: transparent; box-shadow: none; margin-top: 2%; padding-right: 2.5%; padding-left: 2.5%; padding-top:2%; padding-bottom: 2%; border: 3px solid #093773; border-radius: 15px; height: 210px;",
                                    tags$h4("Demographic/Epidemiologic", style = "font-weight: bold; background-color: transparent;"),
                                    div(
                                      style = "display: flex; justify-content: space-between;",
                                      checkboxGroupInput(
                                        "breast_cancer_ml_location_filter",
                                        "Location",
                                        selected = unique(Pheno_exprs$Location),
                                        choices = unique(Pheno_exprs$Location)
                                      ),
                                      checkboxGroupInput(
                                        "breast_cancer_ml_Meno_filter",
                                        "Menopause",
                                        selected = unique(Pheno_exprs$Menopause.status),
                                        choices = unique(Pheno_exprs$Menopause.status)
                                      ),
                                      checkboxGroupInput(
                                        "breast_cancer_ml_ER_filter",
                                        "ER status",
                                        selected = unique(Pheno_exprs$ER.status),
                                        choices = unique(Pheno_exprs$ER.status)
                                      )
                                    )
                                  )
                                ),
                                column(
                                  3,
                                  wellPanel(
                                    style = "background-color: transparent; box-shadow: none; margin-top: 2%; padding-right: 5%; padding-left: 2.5%; padding-top:2%; padding-bottom: 2%; border: 3px solid #093773; border-radius: 15px; height: 210px;",
                                    tags$h4("Risk scores", style = "font-weight: bold;"),
                                    div(
                                      style = "display: flex; justify-content: space-between;",
                                      checkboxGroupInput(
                                        "breast_cancer_ml_rorS_filter",
                                        "Risk of recurrence",
                                        selected = unique(Pheno_exprs$rorS_risk),
                                        choices = unique(Pheno_exprs$rorS_risk)
                                      ),
                                      checkboxGroupInput(
                                        "breast_cancer_ml_Mammaprint_filter",
                                        "Mammaprint risk",
                                        selected = unique(Pheno_exprs$Mammaprint_risk),
                                        choices = unique(Pheno_exprs$Mammaprint_risk)
                                      )
                                    )
                                  )
                                ),
                                column(
                                  3,
                                  wellPanel(
                                    style = "background-color: transparent; align-items:center; justify-content: center; box-shadow: none; margin-top: 2%; padding-top:2%; padding-bottom: 2%; border: 3px solid purple; border-radius: 15px; height: 210px;",
                                    div(
                                      id = "div_model1_legend",
                                      style = "height: 70px; align-items: center; justify-content:center;",
                                      #legend title input
                                      textInput(inputId = "breast_cancer_ml_legend_entry_1",
                                                "Name for legend", value = "Model 1")
                                    ),
                                    # add "Comparison" button
                                    div(
                                      id = "div_model1_add_remove_comparison_button",
                                      tags$head(
                                        tags$link(rel = "stylesheet", type = "text/css",
                                                  href = "comparisons_button.css")
                                      ),
                                      actionButton("multiple_rocs_breast", "Add comparison") %>%
                                        tagAppendAttributes(class = "comparisons-button"),
                                      style = "display: flex; justify-content: center; align-items: center; padding-top: 3%;"
                                    ),
                                    div(
                                      id = "div_model1_trigger_buttons",
                                      style = "display: flex; width: 100%; align-content: center; justify-content: center;",
                                      # add "Predict" - button that triggers the prediction model on the selected data
                                      div(
                                        id = "div_model1_prediction_button",
                                        actionButton(
                                          "predict_ml_breast_cancer",
                                          "Predict!",
                                          icon = icon("eye", style = "margin-right: 5px; vertical-align: middle;")
                                        ) %>%
                                          tagAppendAttributes(class = "rgb-button"),
                                        style = "padding-left: 15%; padding-top: 9%;"
                                      ),
                                      
                                      # Button to reset inputs to default
                                      div(
                                        id = "div_model1_reset_button",
                                        actionButton(
                                          inputId = "reset_input_breast_ml_model1",
                                          label = "Default filters",
                                          icon = icon("repeat", style = "margin-right: 5px; vertical-align: middle;")
                                        ) %>%
                                          tagAppendAttributes(class = "default-button"),
                                        style = "padding-right: 5%; padding-top: 10.5%; padding-left: 5%;"
                                      ),
                                      
                                      # add "Info" button
                                      div(
                                        id = "div_model1_info_button",
                                        actionButton(
                                          "info_ml_breast_cancer",
                                          "Info",
                                          icon = icon("circle-info", style = "margin-right: 5px; vertical-align: middle;")
                                        ) %>%
                                          tagAppendAttributes(class = "info-button"),
                                        style = "padding-right: 15%; padding-top: 9%;"
                                      )
                                    )
                                  )
                                )
                              )
                            ),
                            
                            bsModal(
                              "ml_breast_info",
                              "Information",
                              "info_ml_breast_cancer",
                              fluidRow(htmlOutput("ml_breast_info_text"))
                            )
                          ),
                          
                          
                          # Model selection box for comparison #2
                          hidden(div(id = "model_select_breast_2",
                                     fluidRow(
                                       # Interactive model and data selection panel
                                       box(
                                         id = "ML_control_breast_2",
                                         title = "Select model and data #2",
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 12,
                                         height = 270,
                                         shinyjs::useShinyjs(),
                                         
                                         splitLayout(
                                           cellWidths = c("200px", "250px", "350px", "200px"),
                                           style = "border:1px;padding:10px;white-space:normal;",
                                           # Select ML model category
                                           radioButtons(
                                             inputId = "breast_cancer_ml_model_category_2",
                                             label = "Select model category",
                                             choices = names(ML),
                                             selected = "Decision Trees",
                                             width = "150px"
                                           ),
                                           
                                           # Updatable radioButtons for model subcategory
                                           radioButtons(
                                             "breast_cancer_ml_model_subcategory_2",
                                             "Select model subcategory",
                                             choices = sort(names(ML[["Decision Trees"]])),
                                             selected = "C5.0 - ROC",
                                             width = "250px"
                                           ),
                                           
                                           # Select the datasets that you want to include in the plot
                                           checkboxGroupInput(
                                             "ml_breast_dataset_checkbox_2",
                                             "Select studies",
                                             selected = unique(full_ml_set$Dataset),
                                             choices = unique(full_ml_set$Dataset),
                                             inline = TRUE,
                                             width = "300px"
                                           ),
                                           
                                           # add "select all" button for the datasets to choose from
                                           actionButton(
                                             "select_all_ml_model2_breast",
                                             "Select/De-select All",
                                             style = "background-color: #F8ECBB; font-weight: bold; border-radius: 10px;",
                                             icon = icon("check", style = "margin-right: 5px; vertical-align: middle;")
                                           )
                                         )
                                       ),
                                       # Filtering the subset of the data based on variables for more specific prediction
                                       
                                       # A wide box with checkboxes
                                       box(
                                         id = "ML_breast_cancer_variable_filtering_2",
                                         title = "Variable filtering #2",
                                         status = "primary",
                                         solidHeader = TRUE,
                                         collapsible = TRUE,
                                         width = 12,
                                         fluidRow(
                                           column(
                                             3,
                                             wellPanel(
                                               style = "background-color: transparent; box-shadow: none; margin-top: 2%; padding-right: 2.5%; padding-left: 2.5%; padding-top:2%; padding-bottom: 2%; border: 3px solid #093773; border-radius: 15px; height: 210px;",
                                               tags$h4("Clinical variables", style = "font-weight: bold;"),
                                               div(
                                                 style = "display: flex; justify-content: space-between;",
                                                 checkboxGroupInput(
                                                   "breast_cancer_ml_treatment_filter_2",
                                                   "Treatment",
                                                   selected = unique(Pheno_exprs$Treatment),
                                                   choices = unique(Pheno_exprs$Treatment)
                                                 ),
                                                 checkboxGroupInput(
                                                   "breast_cancer_ml_timepoint_filter_2",
                                                   "Timepoint",
                                                   selected = unique(Pheno_exprs$Timepoint_coded),
                                                   choices = unique(Pheno_exprs$Timepoint_coded)
                                                 ),
                                                 checkboxGroupInput(
                                                   "breast_cancer_ml_pam50_filter_2",
                                                   "pam50",
                                                   selected = unique(Pheno_exprs$pam50),
                                                   choices = unique(Pheno_exprs$pam50)
                                                 )
                                               )
                                             )
                                           ),
                                           column(
                                             3,
                                             wellPanel(
                                               style = "background-color: transparent; box-shadow: none; margin-top: 2%; padding-right: 2.5%; padding-left: 2.5%; padding-top:2%; padding-bottom: 2%; border: 3px solid #093773; border-radius: 15px; height: 210px;",
                                               tags$h4("Demographic/Epidemiologic", style = "font-weight: bold; background-color: transparent;"),
                                               div(
                                                 style = "display: flex; justify-content: space-between;",
                                                 checkboxGroupInput(
                                                   "breast_cancer_ml_location_filter_2",
                                                   "Location",
                                                   selected = unique(Pheno_exprs$Location),
                                                   choices = unique(Pheno_exprs$Location)
                                                 ),
                                                 checkboxGroupInput(
                                                   "breast_cancer_ml_Meno_filter_2",
                                                   "Menopause",
                                                   selected = unique(Pheno_exprs$Menopause.status),
                                                   choices = unique(Pheno_exprs$Menopause.status)
                                                 ),
                                                 checkboxGroupInput(
                                                   "breast_cancer_ml_ER_filter_2",
                                                   "ER status",
                                                   selected = unique(Pheno_exprs$ER.status),
                                                   choices = unique(Pheno_exprs$ER.status)
                                                 )
                                               )
                                             )
                                           ),
                                           column(
                                             3,
                                             wellPanel(
                                               style = "background-color: transparent; box-shadow: none; margin-top: 2%; padding-right: 5%; padding-left: 2.5%; padding-top:2%; padding-bottom: 2%; border: 3px solid #093773; border-radius: 15px; height: 210px;",
                                               tags$h4("Risk scores", style = "font-weight: bold;"),
                                               div(
                                                 style = "display: flex; justify-content: space-between;",
                                                 checkboxGroupInput(
                                                   "breast_cancer_ml_rorS_filter_2",
                                                   "Risk of recurrence",
                                                   selected = unique(Pheno_exprs$rorS_risk),
                                                   choices = unique(Pheno_exprs$rorS_risk)
                                                 ),
                                                 checkboxGroupInput(
                                                   "breast_cancer_ml_Mammaprint_filter_2",
                                                   "Mammaprint risk",
                                                   selected = unique(Pheno_exprs$Mammaprint_risk),
                                                   choices = unique(Pheno_exprs$Mammaprint_risk)
                                                 )
                                               )
                                             )
                                           ),
                                           column(
                                             3,
                                             wellPanel(
                                               tags$head(tags$link(rel = "stylesheet", type = "text/css",
                                                                   href = "remove_comparisons_button.css")),
                                               style = "background-color: transparent; align-items:center; justify-content: center; box-shadow: none; margin-top: 2%; padding-top:2%; padding-bottom: 2%; border: 3px solid purple; border-radius: 15px; height: 210px;",
                                               div(
                                                 id = "div_model2_legend",
                                                 style = "height: 70px; align-items: center; justify-content:center;",
                                                 #legend title input
                                                 textInput(inputId = "breast_cancer_ml_legend_entry_2",
                                                           "Name for legend", value = "Model 2")
                                               ),
                                               # add "Comparison" button
                                               div(id="div_model2_comparison_buttons", style = "display: flex; justify-content: space-around;",
                                                   div(
                                                 id = "div_model2_add_comparison_button",
                                                 style = "display: flex; justify-content: center; align-items: center;",
                                                 actionButton("multiple_rocs_breast_2", "Add new comparison") %>%
                                                   tagAppendAttributes(class = "comparisons-button")
                                               ),
                                               div(
                                                 id = "div_model2_remove_comparison_button",
                                                 style = "display: flex; justify-content: center; align-items: center;",
                                                 actionButton("multiple_rocs_breast_2_remove", "Remove") %>%
                                                   tagAppendAttributes(class = "remove-comparisons-button")
                                               )),
                                               div(
                                                 id = "div_model2_trigger_buttons",
                                                 style = "display: flex; width: 100%; align-content: center; justify-content: center; height: 100px;",
                                                 # add "Apply" - button that adds the comparison to the plot
                                                 div(
                                                   id = "div_model2_apply_button",
                                                   tags$head(
                                                     tags$link(rel = "stylesheet", type = "text/css",
                                                               href = "apply_button.css")
                                                   ),
                                                   actionButton("apply_comparison_breast_2",
                                                                "Apply") %>%
                                                     tagAppendAttributes(class = "apply-button"),
                                                   style = "padding-top: 8%;"
                                                 ),
                                                 
                                                 # Button to reset inputs to default
                                                 div(
                                                   id = "div_model2_reset_button",
                                                   actionButton(
                                                     inputId = "reset_input_breast_ml_model2",
                                                     label = "Default filters",
                                                     icon = icon("repeat", style = "margin-right: 5px; vertical-align: middle;")
                                                   ) %>%
                                                     tagAppendAttributes(class = "default-button"),
                                                   style = "padding-top: 12%; padding-left: 3%;"
                                                 )
                                               )
                                             )
                                           )
                                         )
                                       )
                                     ))),
                          
                          # Model selection box for comparison #3
                          hidden(div(id = "model_select_breast_3",
                                     fluidRow(
                                       # Interactive model and data selection panel
                                       box(
                                         id = "ML_control_breast_3",
                                         title = "Select model and data #3",
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 12,
                                         height = 270,
                                         shinyjs::useShinyjs(),
                                         
                                         splitLayout(
                                           cellWidths = c("200px", "250px", "350px", "200px"),
                                           style = "border:1px;padding:10px;white-space:normal;",
                                           # Select ML model category
                                           radioButtons(
                                             inputId = "breast_cancer_ml_model_category_3",
                                             label = "Select model category",
                                             choices = names(ML),
                                             selected = "Decision Trees",
                                             width = "150px"
                                           ),
                                           
                                           # Updatable radioButtons for model subcategory
                                           radioButtons(
                                             "breast_cancer_ml_model_subcategory_3",
                                             "Select model subcategory",
                                             choices = sort(names(ML[["Decision Trees"]])),
                                             selected = "C5.0 - ROC",
                                             width = "250px"
                                           ),
                                           
                                           # Select the datasets that you want to include in the plot
                                           checkboxGroupInput(
                                             "ml_breast_dataset_checkbox_3",
                                             "Select studies",
                                             selected = unique(full_ml_set$Dataset),
                                             choices = unique(full_ml_set$Dataset),
                                             inline = TRUE,
                                             width = "300px"
                                           ),
                                           
                                           # add "select all" button for the datasets to choose from
                                           actionButton(
                                             "select_all_ml_model3_breast",
                                             "Select/De-select All",
                                             style = "background-color: #F8ECBB; font-weight: bold; border-radius: 10px;",
                                             icon = icon("check", style = "margin-right: 5px; vertical-align: middle;")
                                           )
                                         )
                                       ),
                                       # Filtering the subset of the data based on variables for more specific prediction
                                       
                                       # A wide box with checkboxes
                                       box(
                                         id = "ML_breast_cancer_variable_filtering_3",
                                         title = "Variable filtering #3",
                                         status = "primary",
                                         solidHeader = TRUE,
                                         collapsible = TRUE,
                                         width = 12,
                                         fluidRow(
                                           column(
                                             3,
                                             wellPanel(
                                               style = "background-color: transparent; box-shadow: none; margin-top: 2%; padding-right: 2.5%; padding-left: 2.5%; padding-top:2%; padding-bottom: 2%; border: 3px solid #093773; border-radius: 15px; height: 210px;",
                                               tags$h4("Clinical variables", style = "font-weight: bold;"),
                                               div(
                                                 style = "display: flex; justify-content: space-between;",
                                                 checkboxGroupInput(
                                                   "breast_cancer_ml_treatment_filter_3",
                                                   "Treatment",
                                                   selected = unique(Pheno_exprs$Treatment),
                                                   choices = unique(Pheno_exprs$Treatment)
                                                 ),
                                                 checkboxGroupInput(
                                                   "breast_cancer_ml_timepoint_filter_3",
                                                   "Timepoint",
                                                   selected = unique(Pheno_exprs$Timepoint_coded),
                                                   choices = unique(Pheno_exprs$Timepoint_coded)
                                                 ),
                                                 checkboxGroupInput(
                                                   "breast_cancer_ml_pam50_filter_3",
                                                   "pam50",
                                                   selected = unique(Pheno_exprs$pam50),
                                                   choices = unique(Pheno_exprs$pam50)
                                                 )
                                               )
                                             )
                                           ),
                                           column(
                                             3,
                                             wellPanel(
                                               style = "background-color: transparent; box-shadow: none; margin-top: 2%; padding-right: 2.5%; padding-left: 2.5%; padding-top:2%; padding-bottom: 2%; border: 3px solid #093773; border-radius: 15px; height: 210px;",
                                               tags$h4("Demographic/Epidemiologic", style = "font-weight: bold; background-color: transparent;"),
                                               div(
                                                 style = "display: flex; justify-content: space-between;",
                                                 checkboxGroupInput(
                                                   "breast_cancer_ml_location_filter_3",
                                                   "Location",
                                                   selected = unique(Pheno_exprs$Location),
                                                   choices = unique(Pheno_exprs$Location)
                                                 ),
                                                 checkboxGroupInput(
                                                   "breast_cancer_ml_Meno_filter_3",
                                                   "Menopause",
                                                   selected = unique(Pheno_exprs$Menopause.status),
                                                   choices = unique(Pheno_exprs$Menopause.status)
                                                 ),
                                                 checkboxGroupInput(
                                                   "breast_cancer_ml_ER_filter_3",
                                                   "ER status",
                                                   selected = unique(Pheno_exprs$ER.status),
                                                   choices = unique(Pheno_exprs$ER.status)
                                                 )
                                               )
                                             )
                                           ),
                                           column(
                                             3,
                                             wellPanel(
                                               style = "background-color: transparent; box-shadow: none; margin-top: 2%; padding-right: 5%; padding-left: 2.5%; padding-top:2%; padding-bottom: 2%; border: 3px solid #093773; border-radius: 15px; height: 210px;",
                                               tags$h4("Risk scores", style = "font-weight: bold;"),
                                               div(
                                                 style = "display: flex; justify-content: space-between;",
                                                 checkboxGroupInput(
                                                   "breast_cancer_ml_rorS_filter_3",
                                                   "Risk of recurrence",
                                                   selected = unique(Pheno_exprs$rorS_risk),
                                                   choices = unique(Pheno_exprs$rorS_risk)
                                                 ),
                                                 checkboxGroupInput(
                                                   "breast_cancer_ml_Mammaprint_filter_3",
                                                   "Mammaprint risk",
                                                   selected = unique(Pheno_exprs$Mammaprint_risk),
                                                   choices = unique(Pheno_exprs$Mammaprint_risk)
                                                 )
                                               )
                                             )
                                           ),
                                           column(
                                             3,
                                             wellPanel(
                                               style = "background-color: transparent; align-items:center; justify-content: center; box-shadow: none; margin-top: 2%; padding-right: 5%; padding-left: 2.5%; padding-top:2%; padding-bottom: 2%; border: 3px solid purple; border-radius: 15px; height: 210px;",
                                               div(
                                                 id = "div_model3_legend",
                                                 style = "height: 70px; align-items: center; justify-content:center;",
                                                 #legend title input
                                                 textInput(inputId = "breast_cancer_ml_legend_entry_3",
                                                           "Name for legend", value = "Model 3")
                                               ),
                                               # add "Comparison" button
                                               div(
                                                 id = "div_model3_add_remove_comparison_button",
                                                 style = "display: flex; justify-content: center; align-items: center;",
                                                 actionButton("multiple_rocs_breast_3", "Remove comparison") %>%
                                                   tagAppendAttributes(class = "comparisons-button")
                                               ),
                                               div(
                                                 id = "div_model3_trigger_buttons",
                                                 style = "display: flex; width: 100%; align-content: center; justify-content: center; height: 100px;",
                                                 # add "Apply" - button that adds the comparison to the plot
                                                 div(
                                                   id = "div_model3_apply_button",
                                                   tags$head(
                                                     tags$link(rel = "stylesheet", type = "text/css",
                                                               href = "apply-button.css")
                                                   ),
                                                   actionButton("apply_comparison_breast_3",
                                                                "Apply") %>%
                                                     tagAppendAttributes(class = "apply-button"),
                                                   style = "padding-top: 8%;"
                                                 ),
                                                 
                                                 # Button to reset inputs to default
                                                 div(
                                                   id = "div_model3_reset_button",
                                                   actionButton(
                                                     inputId = "reset_input_breast_ml_model3",
                                                     label = "Default filters",
                                                     icon = icon("repeat", style = "margin-right: 5px; vertical-align: middle;")
                                                   ) %>%
                                                     tagAppendAttributes(class = "default-button"),
                                                   style = "padding-top: 12%; padding-left: 3%;"
                                                 )
                                               )
                                             )
                                           )
                                         )
                                       )
                                     ))),
                          
                          # ROC plot, confusion matrix and error metrics section
                          bsModal(
                            "ML_output_breast",
                            "Output",
                            "predict_ml_breast_cancer",
                            size = "large",
                            fluidRow(column(
                              width = 12,
                              tags$head(
                                tags$link(rel = "stylesheet", type = "text/css",
                                          href = "roc_waiter_adjustments.css")
                              ),
                              # A box to plot the ROC curve
                              box(
                                title = "Model performance",
                                id = "breast_cancer_roc_box",
                                status = "warning",
                                width = 12,
                                height = 600,
                                solidHeader = TRUE,
                                align = "center",
                                useWaiter(),
                                plotOutput(
                                  "breast_cancer_ROC_plot",
                                  width = 500,
                                  height = 500
                                )
                              )
                            ),
                            
                            column(
                              width = 12,
                              
                              # Error metrics
                              box(
                                title = "Error Metrics",
                                width = 12,
                                solidHeader = TRUE,
                                status = "warning",
                                id = "breast_cancer_error_metrics",
                                DT::dataTableOutput("breast_cancer_error_table")
                              )
                            ))
                          )
                        ),
                        
                        # Breast Cancer - Imported unique sample predictor/Dataset predictor #####
                        tabItem(tabName = "breast_cancer_subItem_newpred",
                                h2("Predictions on new samples"),
                                fluidRow(
                                  box(
                                    id = "breast_cancer_new_predictions_panel",
                                    title = "Import data and predict",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = 12,
                                    height = 900,
                                    shinyjs::useShinyjs(),
                                    extendShinyjs(text = jsCode, functions = c("checkRadioStatus")),
                                    tags$head(
                                      tags$link(rel = "stylesheet", type = "text/css", href = "uniform_select_input.css")
                                    ),
                                    fluidRow(column(
                                      4,
                                      wellPanel(
                                        style = "background-color: transparent; box-shadow: none; margin-top: 2%; padding-right: 2.5%; padding-left: 5%; padding-top:2%; padding-bottom: 2%; border: 3px solid #093773; border-radius: 15px; height: 300px;",
                                        tags$h4("Import data for prediction", style = "font-weight: bold;"),
                                        
                                        # Use fluidRow and nested columns for the layout inside the first column
                                        fluidRow(column(
                                          6,
                                          div(
                                            id = "div_import_radio_buttons",
                                            radioButtons(
                                              inputId = "breast_cancer_new_prediction_type",
                                              label = "Select new data type",
                                              choices = c(
                                                "Random sample",
                                                "Import unique sample (genes only)",
                                                "Import unique sample (pre-annotated)",
                                                "Import pre-annotated dataset"
                                              ),
                                              selected = "Import unique sample (genes only)",
                                              width = "200px"
                                            )
                                          )
                                        ),
                                        column(
                                          6,
                                          div(
                                            id = "div_prespecified_treatment",
                                            radioButtons(
                                              inputId = "breast_cancer_new_prediction_prespecified_treatment",
                                              label = "Treatment column ('Endo')",
                                              choices = c("Yes", "No"),
                                              selected = "Yes",
                                              width = "200px"
                                            )
                                          ),
                                          div(
                                            id = "div_upload_button",
                                            fileInput(
                                              "breast_cancer_new_prediction_file_input",
                                              "Choose file",
                                              multiple = FALSE,
                                              accept = c(".txt", ".csv", ".xlsx", ".tsv")
                                            )
                                          )
                                        ))
                                      )
                                    ),
                                    column(
                                      8,
                                      wellPanel(
                                        style = "background-color: transparent; box-shadow: none; margin-top: 1%; padding-right: 1.25%; padding-left: 2.5%; padding-top:1%; padding-bottom: 1%; border: 3px solid #093773; border-radius: 15px; height: 300px;",
                                        tags$h4("Edit phenotypic variables", style = "font-weight: bold;"),
                                        fluidRow(
                                          class = "row-flex",
                                          # Treatment
                                          div(
                                            class = "uniform-select",
                                            id = "div_choose_treatment_newpred",
                                            radioButtons(
                                              "breast_cancer_new_prediction_treatment",
                                              "Select treatment",
                                              choices = c("Chemotherapy", "Endocrine treatment"),
                                              selected = "Chemotherapy"#,
                                              #width = "150px"
                                            )
                                          ),
                                          # pam50
                                          div(
                                            id = "div_choose_pam50_newpred",
                                            class = "uniform-select",
                                            #style = "padding-left: 2%; padding-right: 2%; width: 180px;",
                                            selectInput(
                                              inputId = "breast_cancer_new_prediction_pam50_annotation",
                                              "pam50",
                                              choices = c("Preset", "Random", sort(
                                                c("Luminal A", "Luminal B", "Normal-like",
                                                  "Basal-like", "HER2+")
                                              )),
                                              selected = "Random"
                                            )
                                          ),
                                          # scmod1
                                          div(
                                            id = "div_choose_scmod1_newpred",
                                            class = "uniform-select",
                                            #style = "padding-right: 2%; width: 180px;",
                                            selectInput(
                                              inputId = "breast_cancer_new_prediction_scmod1_annotation",
                                              "scmod1",
                                              choices = c("Preset", "Random", sort(
                                                c(
                                                  "ER-/HER2-",
                                                  "ER+/HER2- high proliferation",
                                                  "ER+/HER2- low proliferation",
                                                  "HER2+"
                                                )
                                              )),
                                              selected = "Random"
                                            )
                                          ),
                                          # Timepoint
                                          div(
                                            id = "div_choose_timepoint_newpred",
                                            class = "uniform-select",
                                            #style = "padding-right: 2%; width: 180px;",
                                            selectInput(
                                              inputId = "breast_cancer_new_prediction_timepoint_annotation",
                                              "Timepoint",
                                              choices = c("Preset", "Random",
                                                          sort(c(
                                                            "Pre-treatment", "On-treatment"
                                                          ))),
                                              selected = "Random"
                                            )
                                          )
                                        ),
                                        fluidRow(
                                          class = "row-flex",
                                          # iC10
                                          div(
                                            id = "div_choose_ic10_newpred",
                                            class = "uniform-select",
                                            # style = "padding-right: 2%; width: 180px;",
                                            selectInput(
                                              inputId = "breast_cancer_new_prediction_ic10_annotation",
                                              "Integrative Clusters",
                                              choices = c(
                                                "Preset",
                                                "Random",
                                                c(
                                                  "iC1",
                                                  "iC2",
                                                  "iC3",
                                                  "iC4",
                                                  "iC5",
                                                  "iC6",
                                                  "iC7",
                                                  "iC8",
                                                  "iC9",
                                                  "iC10"
                                                )
                                              ),
                                              selected = "Random"
                                            )
                                          ),
                                          # Mammaprint
                                          div(
                                            id = "div_choose_mammaprint_newpred",
                                            class = "uniform-select",
                                            # style = "padding-right: 2%; width: 180px;",
                                            selectInput(
                                              inputId = "breast_cancer_new_prediction_mammaprint_annotation",
                                              "Mammaprint",
                                              choices = c("Preset", "Random",
                                                          sort(c("At risk", "No risk"))),
                                              selected = "Random"
                                            )
                                          ),
                                          # rorS
                                          div(
                                            id = "div_choose_rors_newpred",
                                            class = "uniform-select",
                                            # style = "width: 180px;",
                                            selectInput(
                                              inputId = "breast_cancer_new_prediction_rors_annotation",
                                              "rorS (Risk of relapse)",
                                              choices = c("Preset", "Random",
                                                          sort(c(
                                                            "High", "Intermediate", "Low"
                                                          ))),
                                              selected = "Random"
                                            )
                                          )
                                        )
                                      )
                                    )),
                                    fluidRow(
                                      style = "padding-left: 1%;",
                                      wellPanel(
                                        style = "background-color: transparent; box-shadow: none; margin-top: 2%; padding-top:2%; padding-bottom: 2%; border: 3px solid #093773; border-radius: 15px; width: 99%; height: 450px; justify-content: center;",
                                        tags$h4("Filters and ROC plot (dataset only)", style = "font-weight: bold;"),
                                        
                                        # Use fluidRow and nested columns for the layout inside the first column
                                        fluidRow(
                                          class = "row-flex-dataset-case",
                                          # ROC plot (if possible: datasets with Response column)
                                          div(
                                            id = "div_roc_plot_radio_buttons_newpred",
                                            class = "uniform-select-dataset-case",
                                            radioButtons(
                                              "breast_cancer_new_prediction_roc",
                                              "Produce ROC plot?",
                                              choices = c("Yes", "No"),
                                              selected = "No"
                                            )
                                          ),
                                          # Filter for timepoint
                                          div(
                                            id = "div_timepoint_filter_newpred",
                                            class = "uniform-select-dataset-case",
                                            radioButtons(
                                              "breast_cancer_new_prediction_timepoint_filter",
                                              "Timepoint filter",
                                              selected = "All",
                                              choices = c("All", "Pre-treatment", "On-treatment")
                                            )
                                          ),
                                          # Filter for pam50
                                          div(
                                            id = "div_pam50_filter_newpred",
                                            class = "uniform-select-dataset-case",
                                            radioButtons(
                                              "breast_cancer_new_prediction_pam50_filter",
                                              "pam50 filter",
                                              selected = "All",
                                              choices = c(
                                                "All",
                                                "Luminal A",
                                                "Luminal B",
                                                "Normal-like",
                                                "Basal-like",
                                                "HER2+"
                                              )
                                            )
                                          ),
                                          # Filter for scmod1
                                          div(
                                            id = "div_scmod1_filter_newpred",
                                            class = "uniform-select-dataset-case",
                                            radioButtons(
                                              "breast_cancer_new_prediction_scmod1_filter",
                                              "scmod1 filter",
                                              selected = "All",
                                              choices = c(
                                                "All",
                                                "ER-/HER2-",
                                                "ER+/HER2- high proliferation",
                                                "ER+/HER2- low proliferation",
                                                "HER2+"
                                              )
                                            )
                                          ),
                                          # Filter for rorS risk
                                          div(
                                            id = "div_rors_filter_newpred",
                                            class = "uniform-select-dataset-case",
                                            radioButtons(
                                              "breast_cancer_new_prediction_rorS_filter",
                                              "Risk of recurrence",
                                              selected = "All",
                                              choices = c("All", "High", "Intermediate", "Low")
                                            )
                                          ),
                                          # Filter for Mammaprint risk
                                          div(
                                            id = "div_mammaprint_filter_newpred",
                                            class = "uniform-select-dataset-case",
                                            radioButtons(
                                              "breast_cancer_new_prediction_Mammaprint_filter",
                                              "Mammaprint risk",
                                              selected = "All",
                                              choices = c("All", "At risk", "No risk")
                                            )
                                          ),
                                          # Filter for iC10
                                          div(
                                            id = "div_ic10_filter_newpred",
                                            class = "uniform-select-dataset-case",
                                            radioButtons(
                                              "breast_cancer_new_prediction_ic10_filter",
                                              "iC10 filter",
                                              selected = "All",
                                              choices = c(
                                                "All",
                                                "iC1",
                                                "iC2",
                                                "iC3",
                                                "iC4",
                                                "iC5",
                                                "iC6",
                                                "iC7",
                                                "iC8",
                                                "iC9",
                                                "iC10"
                                              )
                                            )
                                          ),
                                          div(
                                            id = "newpred_action_buttons",
                                            class = "vertical-flex",
                                            # add "Predict" - button that triggers the prediction model on the selected data
                                            div(
                                              id = "div_newpred_predict_button",
                                              style = "padding-top: 20%; padding-bottom: 20%;",
                                              actionButton(
                                                "predict_new_prediction_breast_cancer",
                                                "Predict!",
                                                # style = "color: #FFFFFF; background-color: #1986B2; border-color: #2e6da4"
                                                icon = icon("eye", style = "margin-right: 5px; vertical-align: middle;")
                                              ) %>%
                                                tagAppendAttributes(class = "rgb-button")
                                            ),
                                            # Button to reset inputs to default
                                            div(
                                              id = "div_reset_button_newpred",
                                              style = "padding-bottom: 20%;",
                                              actionButton(
                                                inputId = "reset_new_prediction_breast_cancer",
                                                label = "Reset parameters",
                                                icon = icon("repeat", style = "margin-right: 5px; vertical-align: middle;")
                                                #style = "color: #FFFFFF; background-color: #000000; border-color: #2e6da4"
                                              ) %>%
                                                tagAppendAttributes(class = "default-button")
                                            ),
                                            # add "Info" button
                                            div(
                                              id = "div_info_button_newpred",
                                              style = "padding-bottom: 20%;",
                                              actionButton(
                                                "info_new_prediction_breast_cancer",
                                                "Info",
                                                icon = icon("circle-info", style = "margin-right: 5px; vertical-align: middle;")
                                              ) %>%
                                                tagAppendAttributes(class = "info-button")
                                            )
                                          )
                                          
                                        )
                                      )
                                    ),
                                    bsModal(
                                      "new_prediction_breast_info",
                                      "Information",
                                      "info_new_prediction_breast_cancer",
                                      fluidRow(htmlOutput("new_prediction_breast_info_text"))
                                    ),
                                    
                                    # ROC plot
                                    bsModal(
                                      "new_prediction_roc",
                                      "Output",
                                      "predict_new_prediction_breast_cancer",
                                      size = "large",
                                      fluidRow(column(
                                        width = 12,
                                        # A box to plot the ROC curve
                                        box(
                                          title = "Model performance",
                                          id = "newpred_breast_cancer_roc_box",
                                          status = "warning",
                                          width = 12,
                                          height = 600,
                                          solidHeader = TRUE,
                                          align = "center",
                                          useWaiter(),
                                          plotOutput("newpred_ROC_plot", width = 500, height = 500),
                                          verbatimTextOutput("results_text"),
                                          # Additional output for textual information
                                          div(
                                            id = "div_newpred_download_button",
                                            style = "padding-top: 1%;",
                                            downloadButton('download_new_prediction_results', 'Download results')
                                          )
                                        )
                                      ))
                                    )
                                  )
                                )),
                        
                        # Breast Cancer - Custom DGEA #####
                        tabItem(
                          tabName = "breast_cancer_subItem_DGEA",
                          h2("Customized Differential Gene Expression Analysis"),
                          fluidRow(
                            box(
                              title = "Customized Differential Gene Expression Analysis Controls",
                              status = "primary",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              width = 12,
                              tabsetPanel(
                                tabPanel(
                                  "Data Selection",
                                  checkboxGroupInput(
                                    "breast_custom_DGEA_dataset_checkbox",
                                    "Select studies",
                                    selected = unique(Pheno_exprs$Dataset),
                                    choices = unique(Pheno_exprs$Dataset),
                                    inline = TRUE
                                  ),
                                  actionButton(
                                    "select_all_breast_custom_DGEA",
                                    "Select/De-select All",
                                    icon = icon("check", style = "margin-right: 5px; vertical-align: middle;"),
                                    style = "background-color: #F8ECBB; font-weight: bold; border-radius: 10px;"
                                  )
                                ),
                                tabPanel(
                                  "Filtering and Adjustments",
                                  fluidRow(
                                    column(
                                      4,
                                      wellPanel(
                                        style = "background-color: transparent; box-shadow: none; margin-top: 2%; padding-right: 2.5%; padding-left: 5%; padding-top:2%; padding-bottom: 2%; border: 3px solid #093773; border-radius: 15px; height: 210px;",
                                        tags$h4("Clinical variables", style = "font-weight: bold;"),
                                        div(
                                          style = "display: flex; justify-content: space-between;",
                                          checkboxGroupInput(
                                            "breast_custom_DGEA_treatment_filter",
                                            "Treatment",
                                            selected = unique(Pheno_exprs$Treatment),
                                            choices = unique(Pheno_exprs$Treatment)
                                          ),
                                          checkboxGroupInput(
                                            "breast_custom_DGEA_timepoint_filter",
                                            "Timepoint",
                                            selected = unique(Pheno_exprs$Timepoint_coded),
                                            choices = unique(Pheno_exprs$Timepoint_coded)
                                          ),
                                          checkboxGroupInput(
                                            "breast_custom_DGEA_pam50_filter",
                                            "pam50",
                                            selected = unique(Pheno_exprs$pam50),
                                            choices = unique(Pheno_exprs$pam50)
                                          )
                                        )
                                      )
                                    ),
                                    column(
                                      4,
                                      wellPanel(
                                        style = "background-color: transparent; box-shadow: none; margin-top: 2%; padding-right: 2.5%; padding-left: 2.5%; padding-top:2%; padding-bottom: 2%; border: 3px solid #093773; border-radius: 15px; height: 210px;",
                                        tags$h4("Demographic/Epidemiologic", style = "font-weight: bold; background-color: transparent;"),
                                        div(
                                          style = "display: flex; justify-content: space-between;",
                                          checkboxGroupInput(
                                            "breast_custom_DGEA_location_filter",
                                            "Location",
                                            selected = unique(Pheno_exprs$Location),
                                            choices = unique(Pheno_exprs$Location)
                                          ),
                                          checkboxGroupInput(
                                            "breast_custom_DGEA_Meno_filter",
                                            "Menopause",
                                            selected = unique(Pheno_exprs$Menopause.status),
                                            choices = unique(Pheno_exprs$Menopause.status)
                                          ),
                                          checkboxGroupInput(
                                            "breast_custom_DGEA_ER_filter",
                                            "ER status",
                                            selected = unique(Pheno_exprs$ER.status),
                                            choices = unique(Pheno_exprs$ER.status)
                                          )
                                        )
                                      )
                                    ),
                                    column(
                                      4,
                                      wellPanel(
                                        style = "background-color: transparent; box-shadow: none; margin-top: 2%; padding-right: 5%; padding-left: 2.5%; padding-top:2%; padding-bottom: 2%; border: 3px solid #093773; border-radius: 15px; height: 210px;",
                                        tags$h4("Risk scores", style = "font-weight: bold;"),
                                        div(
                                          style = "display: flex; justify-content: space-between;",
                                          checkboxGroupInput(
                                            "breast_custom_DGEA_rorS_filter",
                                            "Risk of recurrence",
                                            selected = unique(Pheno_exprs$rorS_risk),
                                            choices = unique(Pheno_exprs$rorS_risk)
                                          ),
                                          checkboxGroupInput(
                                            "breast_custom_DGEA_Mammaprint_filter",
                                            "Mammaprint risk",
                                            selected = unique(Pheno_exprs$Mammaprint_risk),
                                            choices = unique(Pheno_exprs$Mammaprint_risk)
                                          )
                                        )
                                      )
                                    )
                                  ),
                                  fluidRow(
                                    div(
                                      style = "display: flex; justify-content: space-between; width: 80%; margin: auto; padding-right: 5%; padding-left: 2.5%; padding-top:2%; padding-bottom: 2%; align-items: center; border: 3px solid purple; border-radius: 15px;",
                                      div(
                                        id = "div_breast_custom_dgea_adjustments",
                                        style = "flex: 1;",
                                        checkboxGroupInput(
                                          "breast_custom_DGEA_adjustments",
                                          "Variables to adjust for in the model",
                                          selected = c("Response", "pam50"),
                                          choices = sort(
                                            c(
                                              "Response",
                                              "pam50",
                                              "rorS_risk",
                                              "Mammaprint_risk",
                                              "Location",
                                              "ER.status",
                                              "Menopause.status",
                                              "Timepoint_coded"
                                            )
                                          ),
                                          inline = FALSE
                                        )
                                      ),
                                      div(
                                        id = "div_contrast_selections",
                                        style = "display: flex; justify-content: space-between; flex: 1;",
                                        div(
                                          id = "div_select_contrast_variable",
                                          style = "padding-right: 5%; width: 100%;",
                                          selectInput(
                                            "breast_cancer_custom_DGEA_contrast_selection",
                                            "Select contrast",
                                            choices = c("Response", "pam50"),
                                            selected = "Response",
                                            width = "100%"
                                          )
                                        ),
                                        div(
                                          id = "div_select_contrast_variable_level1",
                                          style = "padding-right: 5%; width: 100%;",
                                          selectInput(
                                            "breast_cancer_custom_DGEA_contrast_level1_selection",
                                            "Select level 1",
                                            choices = c("Responder", "Non_responder"),
                                            selected = "Responder",
                                            width = "100%"
                                          )
                                        ),
                                        selectInput(
                                          "breast_cancer_custom_DGEA_contrast_level2_selection",
                                          "Select level 2",
                                          choices = c("Responder", "Non_responder"),
                                          selected = "Non_responder",
                                          width = "100%"
                                        )
                                      )
                                    )
                                  )
                                ),
                                tabPanel(
                                  "Plot Settings",
                                  textInput("breast_custom_DGEA_title_input",
                                            "Volcano plot title",
                                            value = "Responders vs. Non-responders"),
                                  fluidRow(
                                    column(
                                      3,
                                      colourpicker::colourInput(
                                        "scatter_color_breast_NS_custom",
                                        "Color for p>PVT (n.s. results)",
                                        value = "#BEBEBE"
                                      )
                                    ),
                                    column(
                                      3,
                                      colourpicker::colourInput(
                                        "scatter_color_breast_p_only_custom",
                                        "Color for DE<|DET| & p<PVT",
                                        value = "#FFC0CB"
                                      )
                                    ),
                                    column(
                                      3,
                                      colourpicker::colourInput(
                                        "scatter_color_breast_downr_custom",
                                        "Color for DE<-|DET| & p<PVT",
                                        value = "royalblue"
                                      )
                                    ),
                                    column(
                                      3,
                                      colourpicker::colourInput(
                                        "scatter_color_breast_upr_custom",
                                        "Color for DE>|DET| & p<PVT",
                                        value = "red4"
                                      )
                                    )
                                  ),
                                  fluidRow(
                                    column(
                                      3,
                                      numericInput(
                                        "logFC_breast_custom",
                                        "DET: Diff. Exp. Threshold - vertical lines",
                                        value = 0.25,
                                        min = 0,
                                        max = Inf
                                      )
                                    ),
                                    column(
                                      3,
                                      numericInput(
                                        "pval_breast_custom",
                                        "PVT (adj.P.Val threshold) - horizontal line",
                                        value = 0.05,
                                        min = 0,
                                        max = Inf
                                      )
                                    ),
                                    column(
                                      3,
                                      selectInput(
                                        "pval_selector_breast_custom",
                                        "p-value to plot",
                                        choices = c("P.Value", "adj.P.Val"),
                                        selected = "adj.P.Val"
                                      )
                                    ),
                                    column(
                                      3,
                                      sliderInput(
                                        "alpha_breast_volcano_custom",
                                        "Opacity (alpha)",
                                        min = 0,
                                        max = 1,
                                        value = 0.7
                                      )
                                    )
                                  )
                                )
                              ),
                              div(
                                style = "display: flex; justify-content: center; margin-top: 20px;",
                                div(
                                  id = "div_custom_dgea_analyse_button",
                                  actionButton(
                                    "breast_custom_DGEA_analyse_button",
                                    "Analyse",
                                    icon = icon("magnifying-glass-chart", style = "margin-right: 5px; vertical-align: middle;")
                                  ) %>%
                                    tagAppendAttributes(class = "rgb-button")
                                ),
                                div(
                                  id = "div_custom_dgea_reset_button",
                                  style = "padding-top: 0.25%; padding-left: 2%;",
                                  actionButton(
                                    "reset_input_breast_custom_DGEA",
                                    "Default settings",
                                    icon = icon("repeat", style = "margin-right: 5px; vertical-align: middle;")
                                  ) %>%
                                    tagAppendAttributes(class = "default-button")
                                ),
                                div(
                                  id = "div_custon_dgea_info_button",
                                  actionButton(
                                    "info_custom_DGEA_breast_cancer",
                                    "Info",
                                    icon = icon("circle-info", style = "margin-right: 5px; vertical-align: middle;")
                                  ) %>%
                                    tagAppendAttributes(class = "info-button"),
                                  style = "padding-left: 1.5%;"
                                )
                              )
                            ),
                            bsModal(
                              "custom_dgea_info",
                              "Information",
                              "info_custom_DGEA_breast_cancer",
                              fluidRow(htmlOutput("custom_dgea_info_text"))
                            ),
                            bsModal(
                              "custom_DGEA_output",
                              "Differential Gene Expression Analysis Results",
                              "breast_custom_DGEA_analyse_button",
                              size = "large",
                              fluidRow(column(
                                12,
                                box(
                                  title = "Volcano Plot",
                                  status = "warning",
                                  width = 12,
                                  height = 600,
                                  solidHeader = TRUE,
                                  useWaiter(),
                                  plotlyOutput("breast_cancer_volcano_custom")
                                ),
                                box(
                                  title = "Top Table",
                                  status = "warning",
                                  width = 12,
                                  solidHeader = TRUE,
                                  DT::dataTableOutput("breast_cancer_custom_toptable"),
                                  downloadButton("download_data_custom_DGEA", "Download")
                                )
                              ))
                            )
                          )
                        )
                      ))

# Sidebar #####
# The dashboard sidebar. Main levels: BC
# subItems are introduced here - NOT IN THE BODY
sidebar <- dashboardSidebar(sidebarMenu(
  style = "white-space: normal;",
  # wraps text to fit in sidebar
  menuItem(
    tabName = "breast_cancer_project",
    text = "Breast Cancer",
    startExpanded = TRUE,
    # use this to enable subItems
    
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
                text = "Machine Learning"),
    
    # Machine Learning
    menuSubItem(tabName = "breast_cancer_subItem_newpred",
                text = "New prediction")
  )
))

# UI
ui <-
  dashboardPage(
    skin = "blue",
    # color of the top of the dashboard page
    header = header,
    sidebar = sidebar,
    body = body
  )