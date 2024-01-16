# Libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(openxlsx)
library(readr)
library(data.table)
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
library(zip)

# Load the required data #####

# Breast Cancer ###
breast_cancer_study_chars = openxlsx::read.xlsx("input_data/Study_characteristics.xlsx", sheet = 1)
breast_cancer_study_chars$Dataset = c("C1", "C2", "C3", "E1_1", "E1_2", "E1_3",
                                      "E2", "E3", "E4_1", "E4_2", "XVC1", "XVC2",
                                      "XVE", "XVC3")
breast_cancer_Pheno = openxlsx::read.xlsx("input_data/Pheno.xlsx", sheet = 1)
breast_cancer_DGEA = openxlsx::read.xlsx("input_data/DGEA.xlsx")
breast_cancer_extval = openxlsx::read.xlsx("input_data/Ext_val.xlsx")
breast_cancer_full_pheno = rbind(breast_cancer_Pheno, breast_cancer_extval)
breast_cancer_full_pheno = breast_cancer_full_pheno %>% 
  inner_join(breast_cancer_study_chars %>%
               dplyr::select(Dataset, Publication, Year, Samples, Patients,
                             Location, ER.status, Menopause.status,
                             `Age.(years)`, Design = `Timepoints`),
             by = "Dataset")

# Read in consensus data
breast_cancer_consensus_set = readRDS("input_data/annotation_for_heatmap.rds")

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
RegLogR = readRDS("input_data/Reg_LogR.rds")
BackLogR = readRDS("input_data/Back_LogR.rds")
LogR = list(`Lasso-regularised` = RegLogR,
            Backward = BackLogR)
DT = readRDS("input_data/DT.rds")
SVM = readRDS("input_data/SVM.rds")
ML = list(`Logistic Regression` = c("Lasso-egularised", "Backward"),
          `Decision Trees` = DT,
          `Support Vector Machines` = SVM)
ML[["Logistic Regression"]] = LogR

# Read in ready-for-predictions data
train = readRDS("input_data/train_set.rds") 
validation = readRDS("input_data/validation_set.rds")
test = readRDS("input_data/test_set.rds")
extval = readRDS("input_data/ext_val_set.rds")
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
z_exprs = readRDS("input_data/normalised_expression.rds")
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
