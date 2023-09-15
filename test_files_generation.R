# Load global data
source("global.R")

# Prep
all_correct_columns = colnames(full_ml_set)[3:186]
all_correct_columns = all_correct_columns[all_correct_columns != "Endo"]
gene_columns_with_X = colnames(full_ml_set)[which(grepl("X_", colnames(full_ml_set)))]
gene_columns_without_X = gene_columns_with_X
str_sub(gene_columns_without_X, 1, 2) = ""
pheno_columns = setdiff(all_correct_columns, gene_columns_with_X)
all_correct_columns_without_X = c(gene_columns_without_X, pheno_columns)

# Samples (vectors) #####
# Wrong genes only example
wrong_genes_example = rnorm(166, mean = 0, sd = 1)
names(wrong_genes_example) = as.character(seq(1, 166))
wrong_genes_example = as.data.frame(t(wrong_genes_example))
write.table(wrong_genes_example, "file_checks/wrong_genes_example.txt",
            row.names = FALSE)
write.csv(wrong_genes_example, "file_checks/wrong_genes_example.csv",
          row.names = FALSE)
readr::write_tsv(wrong_genes_example, "file_checks/wrong_genes_example.tsv")
openxlsx::write.xlsx(wrong_genes_example, "file_checks/wrong_genes_example.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/wrong_genes_example.txt", header = TRUE)
check2 = readr::read_csv("file_checks/wrong_genes_example.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/wrong_genes_example.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/wrong_genes_example.xlsx", colNames = TRUE)

# Right genes example
correct_genes_example = rnorm(166, mean = 0, sd = 1)
names(correct_genes_example) = gene_columns_without_X
correct_genes_example = as.data.frame(t(correct_genes_example))
write.table(correct_genes_example, "file_checks/correct_genes_example.txt",
            row.names = FALSE)
write.csv(correct_genes_example, "file_checks/correct_genes_example.csv",
          row.names = FALSE)
readr::write_tsv(correct_genes_example, "file_checks/correct_genes_example.tsv")
openxlsx::write.xlsx(correct_genes_example, "file_checks/correct_genes_example.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/correct_genes_example.txt", header = TRUE)
check2 = readr::read_csv("file_checks/correct_genes_example.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/correct_genes_example.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/correct_genes_example.xlsx", colNames = TRUE)

# Wrong_length genes only
wrong_length_correct_genes_example = rnorm(165, mean = 0, sd = 1)
names(wrong_length_correct_genes_example) = gene_columns_without_X[1:165]
wrong_length_correct_genes_example = as.data.frame(t(wrong_length_correct_genes_example))
write.table(wrong_length_correct_genes_example, "file_checks/wrong_length_correct_genes_example.txt",
            row.names = FALSE)
write.csv(wrong_length_correct_genes_example, "file_checks/wrong_length_correct_genes_example.csv",
          row.names = FALSE)
readr::write_tsv(wrong_length_correct_genes_example, "file_checks/wrong_length_correct_genes_example.tsv")
openxlsx::write.xlsx(wrong_length_correct_genes_example, "file_checks/wrong_length_correct_genes_example.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/wrong_length_correct_genes_example.txt", header = TRUE)
check2 = readr::read_csv("file_checks/wrong_length_correct_genes_example.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/wrong_length_correct_genes_example.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/wrong_length_correct_genes_example.xlsx", colNames = TRUE)

# X_wrong genes example
X_wrong_genes_example = rnorm(166, mean = 0, sd = 1)
names(X_wrong_genes_example) = paste0("X_", seq(1, 166))
X_wrong_genes_example = as.data.frame(t(X_wrong_genes_example))
write.table(X_wrong_genes_example, "file_checks/X_wrong_genes_example.txt",
            row.names = FALSE)
write.csv(X_wrong_genes_example, "file_checks/X_wrong_genes_example.csv",
          row.names = FALSE)
readr::write_tsv(X_wrong_genes_example, "file_checks/X_wrong_genes_example.tsv")
openxlsx::write.xlsx(X_wrong_genes_example, "file_checks/X_wrong_genes_example.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/X_wrong_genes_example.txt", header = TRUE)
check2 = readr::read_csv("file_checks/X_wrong_genes_example.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/X_wrong_genes_example.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/X_wrong_genes_example.xlsx", colNames = TRUE)

# X_correct genes example
X_correct_genes_example = rnorm(166, mean = 0, sd = 1)
names(X_correct_genes_example) = gene_columns_with_X
X_correct_genes_example = as.data.frame(t(X_correct_genes_example))
write.table(X_correct_genes_example, "file_checks/X_correct_genes_example.txt",
            row.names = FALSE)
write.csv(X_correct_genes_example, "file_checks/X_correct_genes_example.csv",
          row.names = FALSE)
readr::write_tsv(X_correct_genes_example, "file_checks/X_correct_genes_example.tsv")
openxlsx::write.xlsx(X_correct_genes_example, "file_checks/X_correct_genes_example.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/X_correct_genes_example.txt", header = TRUE)
check2 = readr::read_csv("file_checks/X_correct_genes_example.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/X_correct_genes_example.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/X_correct_genes_example.xlsx", colNames = TRUE)

# X_wrong_length genes only
X_wrong_length_correct_genes_example = rnorm(166, mean = 0, sd = 1)
names(X_wrong_length_correct_genes_example) = gene_columns_with_X
X_wrong_length_correct_genes_example = as.data.frame(t(X_wrong_length_correct_genes_example))
write.table(X_wrong_length_correct_genes_example, "file_checks/X_wrong_length_correct_genes_example.txt",
            row.names = FALSE)
write.csv(X_wrong_length_correct_genes_example, "file_checks/X_wrong_length_correct_genes_example.csv",
          row.names = FALSE)
readr::write_tsv(X_wrong_length_correct_genes_example, "file_checks/X_wrong_length_correct_genes_example.tsv")
openxlsx::write.xlsx(X_wrong_length_correct_genes_example, "file_checks/X_wrong_length_correct_genes_example.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/X_wrong_length_correct_genes_example.txt", header = TRUE)
check2 = readr::read_csv("file_checks/X_wrong_length_correct_genes_example.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/X_wrong_length_correct_genes_example.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/X_wrong_length_correct_genes_example.xlsx", colNames = TRUE)

# Vector with wrong gene names (no X) but correct pheno columns
wrong_genes_correct_pheno = c(rnorm(166, mean = 0, sd = 1), 
                              sample(c(0, 1), 18, replace = TRUE))
names(wrong_genes_correct_pheno) = c(as.character(seq(1, 166)), pheno_columns, "Endo")
wrong_genes_correct_pheno = as.data.frame(t(wrong_genes_correct_pheno))
write.table(wrong_genes_correct_pheno, "file_checks/wrong_genes_correct_pheno.txt",
            row.names = FALSE)
write.csv(wrong_genes_correct_pheno, "file_checks/wrong_genes_correct_pheno.csv",
          row.names = FALSE)
readr::write_tsv(wrong_genes_correct_pheno, "file_checks/wrong_genes_correct_pheno.tsv")
openxlsx::write.xlsx(wrong_genes_correct_pheno, "file_checks/wrong_genes_correct_pheno.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/wrong_genes_correct_pheno.txt", header = TRUE)
check2 = readr::read_csv("file_checks/wrong_genes_correct_pheno.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/wrong_genes_correct_pheno.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/wrong_genes_correct_pheno.xlsx", colNames = TRUE)

# Vector with wrong gene names (X) but correct pheno columns
X_wrong_genes_correct_pheno = c(rnorm(166, mean = 0, sd = 1), 
                                sample(c(0, 1), 18, replace = TRUE))
names(X_wrong_genes_correct_pheno) = c(paste0("X_", seq(1, 166)), pheno_columns, "Endo")
X_wrong_genes_correct_pheno = as.data.frame(t(X_wrong_genes_correct_pheno))
write.table(X_wrong_genes_correct_pheno, "file_checks/X_wrong_genes_correct_pheno.txt",
            row.names = FALSE)
write.csv(X_wrong_genes_correct_pheno, "file_checks/X_wrong_genes_correct_pheno.csv",
          row.names = FALSE)
readr::write_tsv(X_wrong_genes_correct_pheno, "file_checks/X_wrong_genes_correct_pheno.tsv")
openxlsx::write.xlsx(X_wrong_genes_correct_pheno, "file_checks/X_wrong_genes_correct_pheno.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/X_wrong_genes_correct_pheno.txt", header = TRUE)
check2 = readr::read_csv("file_checks/X_wrong_genes_correct_pheno.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/X_wrong_genes_correct_pheno.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/X_wrong_genes_correct_pheno.xlsx", colNames = TRUE)

# Vector with correct gene names (no X) but wrong pheno columns
correct_genes_wrong_pheno = c(rnorm(166, mean = 0, sd = 1), 
                              sample(c(0, 1), 18, replace = TRUE))
names(correct_genes_wrong_pheno) = c(gene_columns_without_X, paste0("RR_", pheno_columns),
                                     "RR_Endo")
correct_genes_wrong_pheno = as.data.frame(t(correct_genes_wrong_pheno))
write.table(correct_genes_wrong_pheno, "file_checks/correct_genes_wrong_pheno.txt",
            row.names = FALSE)
write.csv(correct_genes_wrong_pheno, "file_checks/correct_genes_wrong_pheno.csv",
          row.names = FALSE)
readr::write_tsv(correct_genes_wrong_pheno, "file_checks/correct_genes_wrong_pheno.tsv")
openxlsx::write.xlsx(correct_genes_wrong_pheno, "file_checks/correct_genes_wrong_pheno.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/correct_genes_wrong_pheno.txt", header = TRUE)
check2 = readr::read_csv("file_checks/correct_genes_wrong_pheno.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/correct_genes_wrong_pheno.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/correct_genes_wrong_pheno.xlsx", colNames = TRUE)

# Vector with correct gene names (X) but wrong pheno columns
X_correct_genes_wrong_pheno = c(rnorm(166, mean = 0, sd = 1), 
                                sample(c(0, 1), 18, replace = TRUE))
names(X_correct_genes_wrong_pheno) = c(gene_columns_with_X, paste0("RR_", pheno_columns),
                                       "RR_Endo")
X_correct_genes_wrong_pheno = as.data.frame(t(X_correct_genes_wrong_pheno))
write.table(X_correct_genes_wrong_pheno, "file_checks/X_correct_genes_wrong_pheno.txt",
            row.names = FALSE)
write.csv(X_correct_genes_wrong_pheno, "file_checks/X_correct_genes_wrong_pheno.csv",
          row.names = FALSE)
readr::write_tsv(X_correct_genes_wrong_pheno, "file_checks/X_correct_genes_wrong_pheno.tsv")
openxlsx::write.xlsx(X_correct_genes_wrong_pheno, "file_checks/X_correct_genes_wrong_pheno.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/X_correct_genes_wrong_pheno.txt", header = TRUE)
check2 = readr::read_csv("file_checks/X_correct_genes_wrong_pheno.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/X_correct_genes_wrong_pheno.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/X_correct_genes_wrong_pheno.xlsx", colNames = TRUE)

# Rogue values in pheno columns
correct_genes_rogue_pheno = c(rnorm(166, mean = 0, sd = 1), 
                              sample(c(2, 5), 17, replace = TRUE), "Aris")
names(correct_genes_rogue_pheno) = c(gene_columns_without_X, pheno_columns,
                                     "Endo")
correct_genes_rogue_pheno = as.data.frame(t(correct_genes_rogue_pheno))
write.table(correct_genes_rogue_pheno, "file_checks/correct_genes_rogue_pheno.txt",
            row.names = FALSE)
write.csv(correct_genes_rogue_pheno, "file_checks/correct_genes_rogue_pheno.csv",
          row.names = FALSE)
readr::write_tsv(correct_genes_rogue_pheno, "file_checks/correct_genes_rogue_pheno.tsv")
openxlsx::write.xlsx(correct_genes_rogue_pheno, "file_checks/correct_genes_rogue_pheno.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/correct_genes_rogue_pheno.txt", header = TRUE)
check2 = readr::read_csv("file_checks/correct_genes_rogue_pheno.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/correct_genes_rogue_pheno.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/correct_genes_rogue_pheno.xlsx", colNames = TRUE)

# X_Rogue values in pheno columns
X_correct_genes_rogue_pheno = c(rnorm(166, mean = 0, sd = 1), 
                                sample(c(2, 5), 17, replace = TRUE), "Aris")
names(X_correct_genes_rogue_pheno) = c(gene_columns_with_X, pheno_columns,
                                       "Endo")
X_correct_genes_rogue_pheno = as.data.frame(t(X_correct_genes_rogue_pheno))
write.table(X_correct_genes_rogue_pheno, "file_checks/X_correct_genes_rogue_pheno.txt",
            row.names = FALSE)
write.csv(X_correct_genes_rogue_pheno, "file_checks/X_correct_genes_rogue_pheno.csv",
          row.names = FALSE)
readr::write_tsv(X_correct_genes_rogue_pheno, "file_checks/X_correct_genes_rogue_pheno.tsv")
openxlsx::write.xlsx(X_correct_genes_rogue_pheno, "file_checks/X_correct_genes_rogue_pheno.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/X_correct_genes_rogue_pheno.txt", header = TRUE)
check2 = readr::read_csv("file_checks/X_correct_genes_rogue_pheno.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/X_correct_genes_rogue_pheno.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/X_correct_genes_rogue_pheno.xlsx", colNames = TRUE)

# Correct full sample (no X)
correct_sample = c(rnorm(166, mean = 0, sd = 1), 
                   sample(c(0, 1), 18, replace = TRUE))
names(correct_sample) = c(gene_columns_without_X, pheno_columns,
                          "Endo")
correct_sample = as.data.frame(t(correct_sample))
write.table(correct_sample, "file_checks/correct_sample.txt",
            row.names = FALSE)
write.csv(correct_sample, "file_checks/correct_sample.csv",
          row.names = FALSE)
readr::write_tsv(correct_sample, "file_checks/correct_sample.tsv")
openxlsx::write.xlsx(correct_sample, "file_checks/correct_sample.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/correct_sample.txt", header = TRUE)
check2 = readr::read_csv("file_checks/correct_sample.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/correct_sample.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/correct_sample.xlsx", colNames = TRUE)

# Correct sample (X)
X_correct_sample = c(rnorm(166, mean = 0, sd = 1), 
                     sample(c(0, 1), 18, replace = TRUE))
names(X_correct_sample) = c(gene_columns_with_X, pheno_columns,
                            "Endo")
X_correct_sample = as.data.frame(t(X_correct_sample))
write.table(X_correct_sample, "file_checks/X_correct_sample.txt",
            row.names = FALSE)
write.csv(X_correct_sample, "file_checks/X_correct_sample.csv",
          row.names = FALSE)
readr::write_tsv(X_correct_sample, "file_checks/X_correct_sample.tsv")
openxlsx::write.xlsx(X_correct_sample, "file_checks/X_correct_sample.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/X_correct_sample.txt", header = TRUE)
check2 = readr::read_csv("file_checks/X_correct_sample.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/X_correct_sample.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/X_correct_sample.xlsx", colNames = TRUE)

# Datasets #####

# Correct dataset
write.table(full_ml_set, "file_checks/full_ml_set.txt",
            row.names = FALSE)
write.csv(full_ml_set, "file_checks/full_ml_set.csv",
          row.names = FALSE)
readr::write_tsv(full_ml_set, "file_checks/full_ml_set.tsv")
openxlsx::write.xlsx(full_ml_set, "file_checks/full_ml_set.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/full_ml_set.txt", header = TRUE)
check2 = readr::read_csv("file_checks/full_ml_set.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/full_ml_set.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/full_ml_set.xlsx", colNames = TRUE)

# Dataset with wrong gene names (no X) but correct pheno columns
dataset_wrong_genes_correct_pheno = c(rnorm(166, mean = 0, sd = 1), 
                                      sample(c(0, 1), 18, replace = TRUE))
names(dataset_wrong_genes_correct_pheno) = c(as.character(seq(1, 166)), pheno_columns, "Endo")
dataset_wrong_genes_correct_pheno = rbind(dataset_wrong_genes_correct_pheno, 
                                          dataset_wrong_genes_correct_pheno, 
                                          dataset_wrong_genes_correct_pheno)
dataset_wrong_genes_correct_pheno = as.data.frame(dataset_wrong_genes_correct_pheno)
write.table(dataset_wrong_genes_correct_pheno, "file_checks/dataset_wrong_genes_correct_pheno.txt",
            row.names = FALSE)
write.csv(dataset_wrong_genes_correct_pheno, "file_checks/dataset_wrong_genes_correct_pheno.csv",
          row.names = FALSE)
readr::write_tsv(dataset_wrong_genes_correct_pheno, "file_checks/dataset_wrong_genes_correct_pheno.tsv")
openxlsx::write.xlsx(dataset_wrong_genes_correct_pheno, "file_checks/dataset_wrong_genes_correct_pheno.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/dataset_wrong_genes_correct_pheno.txt", header = TRUE)
check2 = readr::read_csv("file_checks/dataset_wrong_genes_correct_pheno.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/dataset_wrong_genes_correct_pheno.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/dataset_wrong_genes_correct_pheno.xlsx", colNames = TRUE)

# Dataset with wrong gene names (X) but correct pheno columns
dataset_X_wrong_genes_correct_pheno = c(rnorm(166, mean = 0, sd = 1), 
                                        sample(c(0, 1), 18, replace = TRUE))
names(dataset_X_wrong_genes_correct_pheno) = c(paste0("X_", seq(1, 166)), pheno_columns, "Endo")
dataset_X_wrong_genes_correct_pheno = rbind(dataset_X_wrong_genes_correct_pheno, 
                                            dataset_X_wrong_genes_correct_pheno, 
                                            dataset_X_wrong_genes_correct_pheno)
dataset_X_wrong_genes_correct_pheno = as.data.frame(dataset_X_wrong_genes_correct_pheno)
write.table(dataset_X_wrong_genes_correct_pheno, "file_checks/dataset_X_wrong_genes_correct_pheno.txt",
            row.names = FALSE)
write.csv(dataset_X_wrong_genes_correct_pheno, "file_checks/dataset_X_wrong_genes_correct_pheno.csv",
          row.names = FALSE)
readr::write_tsv(dataset_X_wrong_genes_correct_pheno, "file_checks/dataset_X_wrong_genes_correct_pheno.tsv")
openxlsx::write.xlsx(dataset_X_wrong_genes_correct_pheno, "file_checks/dataset_X_wrong_genes_correct_pheno.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/dataset_X_wrong_genes_correct_pheno.txt", header = TRUE)
check2 = readr::read_csv("file_checks/dataset_X_wrong_genes_correct_pheno.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/dataset_X_wrong_genes_correct_pheno.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/dataset_X_wrong_genes_correct_pheno.xlsx", colNames = TRUE)

# Dataset with correct gene names (no X) but wrong pheno columns
dataset_correct_genes_wrong_pheno = c(rnorm(166, mean = 0, sd = 1), 
                                      sample(c(0, 1), 18, replace = TRUE))
names(dataset_correct_genes_wrong_pheno) = c(gene_columns_without_X, paste0("RR_", pheno_columns),
                                             "RR_Endo")
dataset_correct_genes_wrong_pheno = rbind(dataset_correct_genes_wrong_pheno,
                                          dataset_correct_genes_wrong_pheno,
                                          dataset_correct_genes_wrong_pheno)
dataset_correct_genes_wrong_pheno = as.data.frame(dataset_correct_genes_wrong_pheno)
write.table(dataset_correct_genes_wrong_pheno, "file_checks/dataset_correct_genes_wrong_pheno.txt",
            row.names = FALSE)
write.csv(dataset_correct_genes_wrong_pheno, "file_checks/dataset_correct_genes_wrong_pheno.csv",
          row.names = FALSE)
readr::write_tsv(dataset_correct_genes_wrong_pheno, "file_checks/dataset_correct_genes_wrong_pheno.tsv")
openxlsx::write.xlsx(dataset_correct_genes_wrong_pheno, "file_checks/dataset_correct_genes_wrong_pheno.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/dataset_correct_genes_wrong_pheno.txt", header = TRUE)
check2 = readr::read_csv("file_checks/dataset_correct_genes_wrong_pheno.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/dataset_correct_genes_wrong_pheno.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/dataset_correct_genes_wrong_pheno.xlsx", colNames = TRUE)

# Dataset with correct gene names (X) but wrong pheno columns
dataset_X_correct_genes_wrong_pheno = c(rnorm(166, mean = 0, sd = 1), 
                                        sample(c(0, 1), 18, replace = TRUE))
names(dataset_X_correct_genes_wrong_pheno) = c(gene_columns_with_X, paste0("RR_", pheno_columns),
                                               "RR_Endo")
dataset_X_correct_genes_wrong_pheno = rbind(dataset_X_correct_genes_wrong_pheno,
                                            dataset_X_correct_genes_wrong_pheno,
                                            dataset_X_correct_genes_wrong_pheno)
dataset_X_correct_genes_wrong_pheno = as.data.frame(dataset_X_correct_genes_wrong_pheno)
write.table(dataset_X_correct_genes_wrong_pheno, "file_checks/dataset_X_correct_genes_wrong_pheno.txt",
            row.names = FALSE)
write.csv(dataset_X_correct_genes_wrong_pheno, "file_checks/dataset_X_correct_genes_wrong_pheno.csv",
          row.names = FALSE)
readr::write_tsv(dataset_X_correct_genes_wrong_pheno, "file_checks/dataset_X_correct_genes_wrong_pheno.tsv")
openxlsx::write.xlsx(dataset_X_correct_genes_wrong_pheno, "file_checks/dataset_X_correct_genes_wrong_pheno.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/dataset_X_correct_genes_wrong_pheno.txt", header = TRUE)
check2 = readr::read_csv("file_checks/dataset_X_correct_genes_wrong_pheno.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/dataset_X_correct_genes_wrong_pheno.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/dataset_X_correct_genes_wrong_pheno.xlsx", colNames = TRUE)

# Rogue values in pheno columns
dataset_correct_genes_rogue_pheno = c(rnorm(166, mean = 0, sd = 1), 
                                      sample(c(2, 5), 17, replace = TRUE), "Aris")
names(dataset_correct_genes_rogue_pheno) = c(gene_columns_without_X, pheno_columns,
                                             "Endo")
dataset_correct_genes_rogue_pheno = rbind(dataset_correct_genes_rogue_pheno,
                                          dataset_correct_genes_rogue_pheno,
                                          dataset_correct_genes_rogue_pheno)
dataset_correct_genes_rogue_pheno = as.data.frame(dataset_correct_genes_rogue_pheno)
write.table(dataset_correct_genes_rogue_pheno, "file_checks/dataset_correct_genes_rogue_pheno.txt",
            row.names = FALSE)
write.csv(dataset_correct_genes_rogue_pheno, "file_checks/dataset_correct_genes_rogue_pheno.csv",
          row.names = FALSE)
readr::write_tsv(dataset_correct_genes_rogue_pheno, "file_checks/dataset_correct_genes_rogue_pheno.tsv")
openxlsx::write.xlsx(dataset_correct_genes_rogue_pheno, "file_checks/dataset_correct_genes_rogue_pheno.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/dataset_correct_genes_rogue_pheno.txt", header = TRUE)
check2 = readr::read_csv("file_checks/dataset_correct_genes_rogue_pheno.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/dataset_correct_genes_rogue_pheno.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/dataset_correct_genes_rogue_pheno.xlsx", colNames = TRUE)

# X_Rogue values in pheno columns
dataset_X_correct_genes_rogue_pheno = c(rnorm(166, mean = 0, sd = 1), 
                                        sample(c(2, 5), 17, replace = TRUE), "Aris")
names(dataset_X_correct_genes_rogue_pheno) = c(gene_columns_with_X, pheno_columns,
                                               "Endo")
dataset_X_correct_genes_rogue_pheno = rbind(dataset_X_correct_genes_rogue_pheno,
                                            dataset_X_correct_genes_rogue_pheno,
                                            dataset_X_correct_genes_rogue_pheno)
dataset_X_correct_genes_rogue_pheno = as.data.frame(dataset_X_correct_genes_rogue_pheno)
write.table(dataset_X_correct_genes_rogue_pheno, "file_checks/dataset_X_correct_genes_rogue_pheno.txt",
            row.names = FALSE)
write.csv(dataset_X_correct_genes_rogue_pheno, "file_checks/dataset_X_correct_genes_rogue_pheno.csv",
          row.names = FALSE)
readr::write_tsv(dataset_X_correct_genes_rogue_pheno, "file_checks/dataset_X_correct_genes_rogue_pheno.tsv")
openxlsx::write.xlsx(dataset_X_correct_genes_rogue_pheno, "file_checks/dataset_X_correct_genes_rogue_pheno.xlsx",
                     overwrite = TRUE)
check1 = data.table::fread("file_checks/dataset_X_correct_genes_rogue_pheno.txt", header = TRUE)
check2 = readr::read_csv("file_checks/dataset_X_correct_genes_rogue_pheno.csv", show_col_types = FALSE,
                         col_names = TRUE)
check3 = readr::read_tsv("file_checks/dataset_X_correct_genes_rogue_pheno.tsv", col_names = TRUE,
                         show_col_types = FALSE)
check4 = openxlsx::read.xlsx("file_checks/dataset_X_correct_genes_rogue_pheno.xlsx", colNames = TRUE)

rm(list = ls()); gc()