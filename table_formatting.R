# Load necessary libraries
library(stringr)

# Variables to set the table we are working on
table_title <- "Logarithm of Later Sale Price of Reccomended Home"
panel_a_title <- "Differences in Log of Sale Price"
panel_b_title <- "Differences in Home Availability (Panel B)"
table_number <- 14  # Variable for the table number
input_files <- list(
  data1_minority = paste0("Output/table", table_number, "_dep_var_1_minority.tex"),
  data1_categories = paste0("Output/table", table_number, "_dep_var_1_categories.tex"),
  data2_minority = paste0("Output/table", table_number, "_dep_var_2_minority.tex"),
  data2_categories = paste0("Output/table", table_number, "_dep_var_2_categories.tex")
)
output_file <- paste0("Output/combined_table", table_number, ".tex")
single_panel <- TRUE  # Set this to TRUE for a single panel plot, FALSE for a double panel plot
set_dashes <- FALSE  # Set this to TRUE to replace estimates and standard errors with dashes in columns 3 and 4

# Rows to include in the top and bottom of the table
top_rows <- list(
  "ln(price), advertised home" = "Yes",
  "Racial composition, advertised home" = "Yes"
)
bottom_rows <- list(
  "ln(price), advertised home" = "Yes",
  "Racial composition, advertised home" = "Yes"
)

# Function to extract relevant data from LaTeX table
extract_data <- function(file_path) {
  lines <- readLines(file_path)
  start <- grep("\\\\midrule", lines)[1] + 1
  end <- grep("\\\\midrule", lines)[2] - 1
  data_lines <- lines[start:end]
  return(data_lines)
}

# Function to replace estimates and standard errors with dashes in columns 3 and 4
replace_with_dashes <- function(data_lines) {
  for (i in seq_along(data_lines)) {
    if (grepl("&", data_lines[i])) {
      parts <- unlist(strsplit(data_lines[i], "&"))
      if (length(parts) >= 5) {
        parts[4] <- "     -     "
        parts[5] <- "     -     "
        data_lines[i] <- paste(parts, collapse = "&")
        if (!grepl("\\\\\\\\$", data_lines[i])) {
          data_lines[i] <- paste0(data_lines[i], " \\\\")
        }
      }
    }
  }
  return(data_lines)
}

# Read and extract data from the LaTeX tables
data1_minority <- extract_data(input_files$data1_minority)
data1_categories <- extract_data(input_files$data1_categories)
data2_minority <- extract_data(input_files$data2_minority)
data2_categories <- extract_data(input_files$data2_categories)

# Apply dashes to category rows if set_dashes is TRUE
if (set_dashes) {
  data1_categories <- replace_with_dashes(data1_categories)
  data2_categories <- replace_with_dashes(data2_categories)
}

# Extract adjusted R^2 values
extract_adj_r2 <- function(file_path) {
  lines <- readLines(file_path)
  adj_r2_line <- grep("Adjusted R\\$\\^2\\$", lines, value = TRUE)
  return(adj_r2_line)
}

adj_r2_1 <- str_extract_all(extract_adj_r2(input_files$data1_minority), "-?\\d+\\.\\d+")[[1]]
adj_r2_2 <- str_extract_all(extract_adj_r2(input_files$data1_categories), "-?\\d+\\.\\d+")[[1]]
adj_r2_3 <- str_extract_all(extract_adj_r2(input_files$data2_minority), "-?\\d+\\.\\d+")[[1]]
adj_r2_4 <- str_extract_all(extract_adj_r2(input_files$data2_categories), "-?\\d+\\.\\d+")[[1]]

# Extract observations only from categories tables
extract_obs <- function(file_path) {
  lines <- readLines(file_path)
  obs_line <- grep("Observations", lines, value = TRUE)
  return(obs_line)
}

obs_2 <- str_extract_all(extract_obs(input_files$data1_categories), "\\d+")[[1]]
obs_4 <- str_extract_all(extract_obs(input_files$data2_categories), "\\d+")[[1]]

# Combine the extracted data into a single LaTeX table
combined_table <- c(
  "\\documentclass{article}",
  "\\usepackage{pdflscape}",
  "\\usepackage{booktabs}",
  "\\begin{document}",
  "\\begin{landscape}",
  "\\begin{table}[p]",
  "\\centering",
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  paste0("\\caption{", table_title, "}"),
  paste0("\\label{tab:table", table_number, "}"),
  "\\resizebox{\\columnwidth}{!}{",
  "\\begin{tabular}{l*{4}{c}}",
  "\\toprule",
  if (!single_panel) paste0("&\\multicolumn{4}{c}{", panel_a_title, "}\\\\") else "",
  if (!single_panel) "\\cmidrule{2-5}" else "",
  "&\\multicolumn{1}{c}{Original Data}&\\multicolumn{1}{c}{Updated City Name Only}&\\multicolumn{1}{c}{Correct Race Only}&\\multicolumn{1}{c}{Updated City Name \\& Correct Race}\\\\\\cmidrule(lr){2-2}\\cmidrule(lr){3-3}\\cmidrule(lr){4-4}\\cmidrule(lr){5-5}",
  "&\\multicolumn{1}{c}{(1)}         &\\multicolumn{1}{c}{(2)}         &\\multicolumn{1}{c}{(3)}         &\\multicolumn{1}{c}{(4)}         \\\\",
  "\\midrule",
  data1_minority,
  "\\midrule",  # Add horizontal rule between minority rows and category rows
  data1_categories,
  "\\midrule",
  paste0("Observations      &      ", obs_2[1], "         &      ", obs_2[2], "         &      ", obs_2[3], "         &      ", obs_2[4], "         \\\\"),
  paste0("Adjusted R$^2$ (Minority)      &      ", adj_r2_1[1], "         &      ", adj_r2_1[2], "         &      ", adj_r2_1[3], "         &      ", adj_r2_1[4], "         \\\\"),
  paste0("Adjusted R$^2$ (Category)      &      ", adj_r2_2[1], "         &      ", adj_r2_2[2], "         &      ", adj_r2_2[3], "         &      ", adj_r2_2[4], "         \\\\"),
  if (!single_panel) "\\addlinespace" else "",
  if (!single_panel) "\\midrule" else "",
  if (!single_panel) "\\addlinespace" else "",
  if (!single_panel) paste0("&\\multicolumn{4}{c}{", panel_b_title, "}\\\\") else "",
  if (!single_panel) "\\cmidrule{2-5}" else "",
  if (!single_panel) "&\\multicolumn{1}{c}{Original Data}&\\multicolumn{1}{c}{Updated City Name Only}&\\multicolumn{1}{c}{Correct Race Only}&\\multicolumn{1}{c}{Updated City Name \\& Correct Race}\\\\\\cmidrule(lr){2-2}\\cmidrule(lr){3-3}\\cmidrule(lr){4-4}\\cmidrule(lr){5-5}" else "",
  if (!single_panel) "&\\multicolumn{1}{c}{(1)}         &\\multicolumn{1}{c}{(2)}         &\\multicolumn{1}{c}{(3)}         &\\multicolumn{1}{c}{(4)}         \\\\" else "",
  if (!single_panel) "\\midrule" else "",
  if (!single_panel) data2_minority else "",
  if (!single_panel) "\\midrule" else "",  # Add horizontal rule between minority rows and category rows
  if (!single_panel) data2_categories else "",
  if (!single_panel) "\\midrule" else "",
  if (!single_panel) paste0("Observations      &      ", obs_4[1], "         &      ", obs_4[2], "         &      ", obs_4[3], "         &      ", obs_4[4], "         \\\\") else "",
  if (!single_panel) paste0("Adjusted R$^2$ (Minority)      &      ", adj_r2_3[1], "         &      ", adj_r2_3[2], "         &      ", adj_r2_3[3], "         &      ", adj_r2_3[4], "         \\\\") else "",
  if (!single_panel) paste0("Adjusted R$^2$ (Category)      &      ", adj_r2_4[1], "         &      ", adj_r2_4[2], "         &      ", adj_r2_4[3], "         &      ", adj_r2_4[4], "         \\\\") else "",
  "\\bottomrule",
  "\\multicolumn{5}{l}{\\footnotesize Cluster-robust standard errors in parentheses. Clustered at the trial level.}\\\\",
  "\\multicolumn{5}{l}{\\footnotesize \\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\",
  "\\end{tabular}",
  "}",
  "\\end{table}",
  "\\end{landscape}",
  "\\end{document}"
)

# Write the combined table to a new LaTeX file
if (set_dashes) {
  output_file <- sub("(\\.tex)$", "_dashes\\1", output_file)
}
writeLines(combined_table, output_file)
